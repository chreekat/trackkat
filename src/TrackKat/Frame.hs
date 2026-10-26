-- | Decoding the camera's data stream into blobs.
--
-- The camera thresholds each image itself and sends only "stripes": runs of
-- bright pixels on one scan line, with their brightness sums. We join
-- touching stripes into blobs and take brightness-weighted centroids.
--
-- Packet layout ported from linuxtrack's @tir_img.c@ (MIT licence).
module TrackKat.Frame
  ( Stripe (..)
  , Packet (..)
  , Blob (..)
  , parseTransfer
  , decodeStripe
  , encodeStripe
  , stripesToBlobs
  , frameWidth
  , frameHeight
  ) where

import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import qualified Data.ByteString as B
import Data.List (foldl', partition, sortOn)
import Data.Ord (Down (..))
import Data.Word (Word8)

frameWidth, frameHeight :: Int
frameWidth = 640
frameHeight = 480

data Stripe = Stripe
  { stLine :: !Int
  , stStart :: !Int
  , stPoints :: !Int
  , stSumX :: !Int  -- ^ sum of brightness * (x - start)
  , stSum :: !Int   -- ^ sum of brightness
  }
  deriving (Eq, Show)

stStop :: Stripe -> Int
stStop s = stStart s + stPoints s - 1

data Packet
  = Frame !Word8 [Stripe]   -- ^ frame counter, stripes
  | StatusReply B.ByteString
  | DevInfoReply B.ByteString
  | Unknown B.ByteString
  | Corrupt String B.ByteString
  deriving (Show)

-- | Split one USB transfer into packets.
parseTransfer :: B.ByteString -> [Packet]
parseTransfer bs
  | B.length bs < 2 = []
  | otherwise = case B.index bs 1 of
      0x10 | B.length bs > 2, B.index bs 2 `elem` [0, 5] -> [parseFrame bs]
      t | t `elem` [0x10, 0x20, 0x40, 0x1C] ->
        let n = fromIntegral (B.index bs 0)
            (pkt, rest) = B.splitAt n bs
            this = case t of
              0x20 -> StatusReply pkt
              0x40 -> DevInfoReply pkt
              _ -> Unknown pkt
         in if n == 0 then [Unknown bs] else this : parseTransfer rest
      _ -> [Unknown bs]

-- A frame fills the whole transfer: a 4-byte header whose bytes XOR to 0xAA,
-- the stripes, and a big-endian 32-bit length of the stripes.
parseFrame :: B.ByteString -> Packet
parseFrame bs
  | len < 8 = Corrupt "short frame" bs
  | foldl' xor 0 (B.unpack (B.take 4 bs)) /= 0xAA = Corrupt "bad header checksum" bs
  | trailer /= len - 8 = Corrupt ("bad length " ++ show trailer ++ " /= " ++ show (len - 8)) bs
  | otherwise = Frame (B.index bs 0) (stripes (B.take (len - 8) (B.drop 4 bs)))
  where
    len = B.length bs
    trailer = foldl' (\acc w -> acc `shiftL` 8 .|. fromIntegral w) 0 (B.unpack (B.drop (len - 4) bs))
    stripeSize = if B.index bs 2 == 5 then 8 else 4
    decode = if stripeSize == 8 then decodeStripe else decodeShortStripe
    stripes p
      | B.length p < stripeSize = []
      | otherwise = decode (B.take stripeSize p) : stripes (B.drop stripeSize p)

-- | The 8-byte TrackIR 5 stripe: start(10) line(9) points(10) sumX(20) sum(15).
decodeStripe :: B.ByteString -> Stripe
decodeStripe p =
  Stripe
    { stStart = b 0 `shiftL` 2 .|. b 1 `shiftR` 6
    , stLine = (b 1 .&. 0x3F) `shiftL` 3 .|. b 2 `shiftR` 5
    , stPoints = (b 2 .&. 0x1F) `shiftL` 5 .|. b 3 `shiftR` 3
    , stSumX = (b 3 .&. 7) `shiftL` 17 .|. b 4 `shiftL` 9 .|. b 5 `shiftL` 1 .|. b 6 `shiftR` 7
    , stSum = (b 6 .&. 0x7F) `shiftL` 8 .|. b 7
    }
  where
    b i = fromIntegral (B.index p i) :: Int

-- | Inverse of 'decodeStripe', for tests and simulation.
encodeStripe :: Stripe -> B.ByteString
encodeStripe (Stripe line start points sumX s) =
  B.pack $
    map fromIntegral
      [ start `shiftR` 2
      , (start .&. 3) `shiftL` 6 .|. line `shiftR` 3
      , (line .&. 7) `shiftL` 5 .|. points `shiftR` 5
      , (points .&. 0x1F) `shiftL` 3 .|. sumX `shiftR` 17
      , (sumX `shiftR` 9) .&. 0xFF
      , (sumX `shiftR` 1) .&. 0xFF
      , (sumX .&. 1) `shiftL` 7 .|. s `shiftR` 8
      , s .&. 0xFF
      ]

-- | Older 4-byte stripe without brightness; every pixel counts as 1.
decodeShortStripe :: B.ByteString -> Stripe
decodeShortStripe p =
  Stripe
    { stLine = b 0 .|. bit 0x20 0x100
    , stStart = b 1 .|. bit 0x80 0x100 .|. bit 0x10 0x200
    , stPoints = n
    , stSumX = n * (n - 1) `div` 2
    , stSum = n
    }
  where
    b i = fromIntegral (B.index p i) :: Int
    bit m v = if b 3 .&. m /= 0 then v else 0
    stop = b 2 .|. bit 0x40 0x100 .|. bit 0x08 0x200
    n = stop - (b 1 .|. bit 0x80 0x100 .|. bit 0x10 0x200) + 1

data Blob = Blob
  { blobX :: !Double
  , blobY :: !Double
  , blobPoints :: !Int
  , blobWeight :: !Int
  }
  deriving (Eq, Show)

-- | Join stripes that touch (adjacent lines, overlapping columns) into blobs,
-- keep those with between @minPts@ and @maxPts@ pixels, heaviest first.
stripesToBlobs :: Int -> Int -> [Stripe] -> [Blob]
stripesToBlobs minPts maxPts =
  sortOn (Down . blobWeight)
    . filter (\b -> blobPoints b >= minPts && blobPoints b <= maxPts)
    . map toBlob
    . foldl' add []
    . sortOn stLine
  where
    add groups s =
      let (touching, rest) = partition (any (touches s)) groups
       in (s : concat touching) : rest
    touches s t =
      abs (stLine s - stLine t) <= 1
        && stStart s <= stStop t
        && stStart t <= stStop s

toBlob :: [Stripe] -> Blob
toBlob ss
  | w > 0 = Blob (sumOf (\s -> stSum s * stStart s + stSumX s) / w')
                 (sumOf (\s -> stSum s * stLine s) / w') pts w
  | otherwise = Blob (sumOf (\s -> stPoints s * (2 * stStart s + stPoints s - 1)) / (2 * p'))
                     (sumOf (\s -> stPoints s * stLine s) / p') pts 0
  where
    sumOf f = fromIntegral (sum (map f ss)) :: Double
    w = sum (map stSum ss)
    w' = fromIntegral w
    pts = sum (map stPoints ss)
    p' = fromIntegral pts
