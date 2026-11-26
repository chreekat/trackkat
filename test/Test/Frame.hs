-- | The camera's byte stream cut into frames, and stripes made into blobs.
module Test.Frame (tests) where

import Control.Monad (forM_)
import Data.Bits (shiftR, xor, (.&.))
import qualified Data.ByteString as B
import Data.List (inits, sort, sortOn, tails)
import Data.Word (Word8)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck hiding ((.&.))

import Test.Check
import Test.Gen (shrinkTo)
import Test.Instances ()
import TrackKat.Frame
import TrackKat.Protocol

tests :: TestTree
tests = group "Frame" $ do
    -- Stripe bit-packing round-trips at field extremes.
    forM_ [Stripe 0 0 1 0 0, Stripe 479 639 1023 0xFFFFF 0x7FFF, Stripe 123 456 7 4321 999] $ \s ->
        checkShowing "stripe" (show s) (decodeStripe (encodeStripe s) == s)

    -- A synthetic frame with two blobs: one spanning three lines, one single stripe.
    let blobA = [Stripe 100 200 4 (6 * 50) (4 * 50), Stripe 101 199 6 (15 * 50) (6 * 50), Stripe 102 200 4 (6 * 50) (4 * 50)]
        blobB = [Stripe 300 50 5 (10 * 20) (5 * 20)]
        payload = B.concat (map encodeStripe (blobA ++ blobB))
        header = B.pack [0x07, 0x10, 0x05, 0x07 `xor` 0x10 `xor` 0x05 `xor` 0xAA]
        len = B.length payload
        trailer = B.pack [fromIntegral (len `shiftR` s .&. 0xFF) | s <- [24, 16, 8, 0]]
    case fst (parseStream (B.concat [header, payload, trailer])) of
        [Frame 7 ss] -> do
            check "stripes decoded" (ss == blobA ++ blobB)
            case stripesToBlobs 1 1000 ss of
                [a, b] -> do
                    checkShowing "blob A centre" (show a) (near (blobX a) 201.5 && near (blobY a) 101)
                    checkShowing "blob B centre" (show b) (near (blobX b) 52 && near (blobY b) 300)
                    check "blob A pixels" (blobPoints a == 14)
                bs -> checkShowing "two blobs, got" (show bs) False
        other -> checkShowing "frame parse, got" (show other) False

    -- The byte stream: frames back to back, cut into transfers anywhere.
    let frame n ss =
            let body = B.concat (map encodeStripe ss)
                bodyLen = B.length body
             in B.concat
                    [ B.pack [n, 0x10, 0x05, n `xor` 0x10 `xor` 0x05 `xor` 0xAA]
                    , body
                    , B.pack [fromIntegral (bodyLen `shiftR` s .&. 0xFF) | s <- [24, 16, 8, 0]]
                    ]
        feed chunks = go B.empty chunks
          where
            go pending [] = (fst (parseStream pending), B.null (snd (parseStream pending)))
            go pending (c : cs) = let (ps, rest) = parseStream (pending <> c); (more, done) = go rest cs in (ps ++ more, done)
        counters ps = [n | Frame n _ <- ps]
        corrupt ps = [why | Corrupt why _ <- ps]
        stream = B.concat [frame 0x7E blobA, frame 0x7F [], frame 0x80 blobB]
        cutIn i j = feed [B.take i stream, B.take (j - i) (B.drop i stream), B.drop j stream]
        whole (ps, done) = counters ps == [0x7E, 0x7F, 0x80] && null (corrupt ps) && done
        cutBadly = [(i, j) | i <- [0 .. B.length stream], j <- [i .. B.length stream], not (whole (cutIn i j))]
    checkShowing "a stream cut into three transfers anywhere" (show cutBadly) (null cutBadly)

    let truncated = B.take 20 (frame 0x81 blobA) <> frame 0x82 blobB
        (afterLoss, _) = feed [truncated]
    checkShowing "frame that lost its tail" (show afterLoss) (counters afterLoss == [0x82] && length (corrupt afterLoss) == 1)

    let (afterJunk, _) = feed [B.pack [0x00, 0x99, 0x42], frame 0x10 blobB]
    checkShowing "junk before a frame" (show afterJunk) (counters afterJunk == [0x10] && length (corrupt afterJunk) == 1)

    let (withReply, _) = feed [frame 0x20 blobB <> B.pack [0x07, 0x20, 0, 1, 0, 0, 0] <> frame 0x21 []]
    checkShowing "status reply between frames" (show withReply) (counters withReply == [0x20, 0x21] && length [() | StatusReply _ <- withReply] == 1)

    forM_ [0x10, 0x1C, 0x20, 0x40] $ \t -> do
        let (withAny, _) = feed [frame 0x30 blobB <> B.pack [0x05, t, 1, 2, 3] <> frame 0x31 []]
        checkShowing ("a reply of type " ++ show t ++ " between frames") (show withAny) (counters withAny == [0x30, 0x31] && null (corrupt withAny) && length withAny == 3)

    check "status reply" $ fmap statusReady (parseStatus (B.pack [0x07, 0x20, 0, 1, 0, 0, 0])) == Just True
    tree properties

properties :: TestTree
properties =
    testGroup
        "properties"
        [ testProperty "a stripe is read back as packed" $
            \s ->
                decodeStripe (encodeStripe s) === s
        , testProperty "any eight bytes are a stripe, packed as read" $
            \(EightBytes bytes) ->
                encodeStripe (decodeStripe bytes) === bytes
        , testProperty "frames back to back, cut into transfers anywhere, are read as sent" $
            \(CutStream frames cuts) ->
                let (packets, rest) = fed (cutAt cuts (streamOf frames))
                 in counterexample (show packets) $
                        [(n, ss) | Frame n ss <- packets] === frames .&&. length packets === length frames .&&. B.null rest
        , testProperty "blobs keep every lit pixel of the stripes they are made of" $
            \(Sent stripes) ->
                sum (map blobPoints (stripesToBlobs 1 maxBound stripes)) === sum (map stPoints stripes)
        ]

-- | Any eight bytes; simplest all nothing, and simpler with a smaller one.
newtype EightBytes = EightBytes B.ByteString
    deriving (Show)

instance Arbitrary EightBytes where
    arbitrary = EightBytes . B.pack <$> vectorOf 8 arbitrary
    shrink (EightBytes bytes) = EightBytes <$> shrinkTo (B.replicate 8 0) smaller bytes
      where
        smaller bs = [B.pack (before ++ [byte'] ++ after) | (before, byte : after) <- zip (inits (B.unpack bs)) (tails (B.unpack bs)), byte' <- shrink byte]

-- | A frame as the camera sends one, with the given counter: its stripes in
-- order down and across the image, none overlapping, and none spelling the
-- header of the frame that follows, which would cut it short (see Note
-- [Camera byte stream]).
genFrame :: Word8 -> Gen (Word8, [Stripe])
genFrame n = (,) n . asSent <$> listOf arbitrary `suchThat` (not . spellsNextHeader n . asSent)

-- | Stripes in the camera's order, without those that overlap one before.
asSent :: [Stripe] -> [Stripe]
asSent = foldr keep [] . sortOn (\s -> (stLine s, stStart s))
  where
    keep s rest = s : dropWhile (\next -> stLine next == stLine s && stStart next < stStart s + stPoints s) rest

spellsNextHeader :: Word8 -> [Stripe] -> Bool
spellsNextHeader n stripes = any header (windows (B.unpack (B.concat (map encodeStripe stripes))))
  where
    windows bytes = [take 4 (drop i bytes) | i <- [0 .. length bytes - 4]]
    header w = case w of
        [counter, 0x10, format, checked] -> counter == n + 1 && format `elem` [0, 5] && foldl xor 0 [counter, 0x10, format, checked] == 0xAA
        _ -> False

shrinkFrame :: (Word8, [Stripe]) -> [(Word8, [Stripe])]
shrinkFrame (n, stripes) = [(n, kept) | kept <- map asSent (shrinkList shrink stripes), not (spellsNextHeader n kept)]

-- | The stripes of a frame as the camera sends one.
newtype Sent = Sent [Stripe]
    deriving (Show)

instance Arbitrary Sent where
    arbitrary = Sent . snd <$> genFrame 0
    shrink (Sent stripes) = Sent . snd <$> shrinkFrame (0, stripes)

-- | A few frames, their counters going up from anywhere, and where to cut
-- their bytes into transfers. Simplest no frames at all; simpler with
-- fewer frames from the end, so the counters still follow on; with fewer
-- cuts; and with frames of fewer stripes.
data CutStream = CutStream [(Word8, [Stripe])] [Int]
    deriving (Eq, Show)

instance Arbitrary CutStream where
    arbitrary = do
        first <- arbitrary
        count <- chooseInt (0, 4)
        frames <- mapM (\i -> scale (`div` 4) (genFrame (first + fromIntegral i))) [0 .. count - 1]
        CutStream frames . sort <$> listOf (chooseInt (0, B.length (streamOf frames)))
    shrink = shrinkTo (CutStream [] []) steps
      where
        steps (CutStream frames cuts) =
            [cutting cuts (take k frames) | k <- [0 .. length frames - 1]]
                ++ [CutStream frames cuts' | cuts' <- shrinkList (const []) cuts]
                ++ [cutting cuts (before ++ [frame'] ++ after) | (before, frame : after) <- zip (inits frames) (tails frames), frame' <- shrinkFrame frame]
        cutting cuts frames = CutStream frames (filter (<= B.length (streamOf frames)) cuts)

-- | The bytes of frames back to back.
streamOf :: [(Word8, [Stripe])] -> B.ByteString
streamOf = B.concat . map frameBytes

-- | A frame's bytes: header, stripes, and how many bytes of stripes.
frameBytes :: (Word8, [Stripe]) -> B.ByteString
frameBytes (n, stripes) =
    B.concat
        [ B.pack [n, 0x10, 0x05, n `xor` 0x10 `xor` 0x05 `xor` 0xAA]
        , body
        , B.pack [fromIntegral (B.length body `shiftR` s .&. 0xFF) | s <- [24, 16, 8, 0]]
        ]
  where
    body = B.concat (map encodeStripe stripes)

cutAt :: [Int] -> B.ByteString -> [B.ByteString]
cutAt cuts stream = zipWith (\from to -> B.take (to - from) (B.drop from stream)) (0 : cuts) (cuts ++ [B.length stream])

-- | The packets read from transfers one after another, and what is left
-- over.
fed :: [B.ByteString] -> ([Packet], B.ByteString)
fed = go B.empty
  where
    go pending [] = parseStream pending
    go pending (c : cs) = let (ps, rest) = parseStream (pending <> c); (more, left) = go rest cs in (ps ++ more, left)
