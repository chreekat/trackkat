-- | Wire format of the TrackIR 5 revision 3 (USB id 131D:0159).
--
-- Ported from linuxtrack's @tir_hw.c@ and @tir_img.c@ (MIT licence,
-- Copyright (c) 2009 Tulthix, uglyDwarf).
--
-- Commands to a v3 camera are 24-byte packets padded with random bytes, with
-- the payload scattered through the padding. The camera's replies and frame
-- data are not obfuscated.
module TrackKat.Protocol (
    -- * Commands
    Command (..),
    Seed,
    newSeed,
    encode,
    decode,

    -- * Replies
    Status (..),
    parseStatus,
    isDevInfo,
) where

import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Data.Array.ST (STUArray, getElems, newArray, readArray, writeArray)
import Data.Bits (shiftL, shiftR, testBit, xor, (.&.), (.|.))
import qualified Data.ByteString as B
import Data.STRef (STRef, modifySTRef', newSTRef, readSTRef)
import Data.Word (Word64, Word8)

-- | A command carries an opcode and zero, one or three argument bytes.
-- Only the low nibble of 'Cmd2''s argument survives encoding.
data Command
    = Cmd1 Word8
    | Cmd2 Word8 Word8
    | Cmd4 Word8 Word8 Word8 Word8
    deriving (Eq, Show)

-- | State of the padding generator. The camera doesn't care what the padding
-- is, so this only needs to be cheap, not good.
type Seed = Word64

newSeed :: Word64 -> Seed
newSeed = id

-- splitmix64
next :: STRef s Seed -> ST s Word64
next ref = do
    modifySTRef' ref (+ 0x9E3779B97F4A7C15)
    z0 <- readSTRef ref
    let z1 = (z0 `xor` (z0 `shiftR` 30)) * 0xBF58476D1CE4E5B9
        z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94D049BB133111EB
    pure (z2 `xor` (z2 `shiftR` 31))

-- | Uniform-ish integer in [lo, hi].
range :: STRef s Seed -> Int -> Int -> ST s Int
range ref lo hi = do
    r <- next ref
    pure (lo + fromIntegral (r `mod` fromIntegral (hi - lo + 1)))

-- | Encode a command as the 24 bytes to send, and the next seed.
encode :: Seed -> Command -> (B.ByteString, Seed)
encode seed cmd = runST $ do
    ref <- newSTRef seed
    pkt <- newArray (0, 23) 0 :: ST s (STUArray s Int Word8)
    forM_ [1 .. 23] $ \i -> next ref >>= writeArray pkt i . fromIntegral
    let at = readArray pkt
        set = writeArray pkt
        -- Hide the 4-bit index r1 in bit 3..0 of bytes 1..4.
        hideIndex r1 = forM_ (zip [1 .. 4] [3, 2, 1, 0]) $ \(i, b) -> do
            x <- at i
            let bit = if testBit r1 b then 1 `shiftL` b else 0
            set i ((x .&. complementBit8 b) .|. bit)
    case cmd of
        Cmd1 op -> set 0 op
        Cmd2 op v -> do
            set 0 op
            r1 <- range ref 2 14
            r2 <- range ref 0 3
            hideIndex r1
            k <- at 16
            x <- at r1
            set r1 (((v `shiftL` 4) .|. (x .&. 0x0F)) `xor` (k .&. 0xF0))
            y <- at (r1 + 1)
            set (r1 + 1) ((fromIntegral r2 `shiftL` 6) .|. (y .&. 0x3F))
        Cmd4 op a b c -> do
            set 0 op
            r1 <- range ref 6 14
            hideIndex r1
            k16 <- at 16
            k18 <- at 18
            k19 <- at 19
            set r1 (k16 `xor` a)
            set (r1 - 1) (k19 `xor` b)
            set (r1 + 1) (k18 `xor` c)
    -- Finally scramble the opcode with two padding bytes, whose indices go in
    -- byte 17.
    hi <- range ref 1 15
    lo <- range ref 1 15
    op <- at 0
    ph <- at hi
    pl <- at lo
    set 0 (op `xor` 0x69 `xor` ph `xor` pl)
    set 17 (fromIntegral (hi `shiftL` 4 + lo))
    bytes <- getElems pkt
    s' <- readSTRef ref
    pure (B.pack bytes, s')
  where
    complementBit8 :: Int -> Word8
    complementBit8 b = 0xFF `xor` (1 `shiftL` b)

-- | Inverse of 'encode', given which shape of command was sent. Only used to
-- test 'encode'; the camera does its own decoding.
decode :: Int -> B.ByteString -> Maybe Command
decode arity pkt
    | B.length pkt /= 24 = Nothing
    | otherwise =
        let at = B.index pkt
            sel = fromIntegral (at 17) :: Int
            op = at 0 `xor` 0x69 `xor` at (sel `shiftR` 4) `xor` at (sel .&. 0x0F)
            r1 =
                foldl
                    (\acc (i, b) -> if testBit (at i) b then acc .|. (1 `shiftL` b) else acc)
                    0
                    (zip [1 .. 4] [3, 2, 1, 0]) ::
                    Int
         in case arity of
                1 -> Just (Cmd1 op)
                2 -> Just (Cmd2 op ((at r1 `xor` (at 16 .&. 0xF0)) `shiftR` 4))
                4 -> Just (Cmd4 op (at r1 `xor` at 16) (at (r1 - 1) `xor` at 19) (at (r1 + 1) `xor` at 18))
                _ -> Nothing

data Status = Status
    { statusReady :: Bool
    , statusRaw :: B.ByteString
    }
    deriving (Show)

-- | Recognise a status reply (sent in response to @Cmd2 0x1A 0x07@).
parseStatus :: B.ByteString -> Maybe Status
parseStatus bs
    | B.length bs > 3 && B.index bs 0 >= 0x07 && B.index bs 1 == 0x20 =
        Just (Status (B.index bs 3 == 1) bs)
    | otherwise = Nothing

-- | Recognise the device-info reply (sent in response to @Cmd1 0x17@).
isDevInfo :: B.ByteString -> Bool
isDevInfo bs = B.length bs > 2 && B.index bs 1 == 0x40
