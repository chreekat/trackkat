-- | The camera's byte stream cut into frames, and stripes made into blobs.
module Test.Frame (tests) where

import Control.Monad (forM_)
import Data.Bits (shiftR, xor, (.&.))
import qualified Data.ByteString as B
import Test.Tasty (TestTree)

import Test.Check
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
        cutAt i j = feed [B.take i stream, B.take (j - i) (B.drop i stream), B.drop j stream]
        whole (ps, done) = counters ps == [0x7E, 0x7F, 0x80] && null (corrupt ps) && done
        cutBadly = [(i, j) | i <- [0 .. B.length stream], j <- [i .. B.length stream], not (whole (cutAt i j))]
    checkShowing "a stream cut into three transfers anywhere" (show cutBadly) (null cutBadly)

    let truncated = B.take 20 (frame 0x81 blobA) <> frame 0x82 blobB
        (afterLoss, _) = feed [truncated]
    checkShowing "frame that lost its tail" (show afterLoss) (counters afterLoss == [0x82] && length (corrupt afterLoss) == 1)

    let (afterJunk, _) = feed [B.pack [0x00, 0x99, 0x42], frame 0x10 blobB]
    checkShowing "junk before a frame" (show afterJunk) (counters afterJunk == [0x10] && length (corrupt afterJunk) == 1)

    let (withReply, _) = feed [frame 0x20 blobB <> B.pack [0x07, 0x20, 0, 1, 0, 0, 0] <> frame 0x21 []]
    checkShowing "status reply between frames" (show withReply) (counters withReply == [0x20, 0x21] && length [() | StatusReply _ <- withReply] == 1)

    check "status reply" $ fmap statusReady (parseStatus (B.pack [0x07, 0x20, 0, 1, 0, 0, 0])) == Just True
