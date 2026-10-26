module Main (main) where

import Control.Monad (forM_, unless)
import Data.Bits (shiftR, xor, (.&.))
import qualified Data.ByteString as B
import Data.IORef
import System.Exit (exitFailure)

import TrackKat.Frame
import TrackKat.Protocol

main :: IO ()
main = do
  failures <- newIORef (0 :: Int)
  let check name ok = unless ok $ do
        putStrLn ("FAIL: " ++ name)
        modifyIORef' failures (+ 1)

  -- Command obfuscation round-trips for many seeds.
  let cmds = [(1, Cmd1 0x13), (2, Cmd2 0x1A 0x07), (4, Cmd4 0x19 0x05 0x01 0x18), (4, Cmd4 0x23 0x3B 0x8E 0xFF)]
  forM_ [0 .. 2000 :: Int] $ \i -> forM_ cmds $ \(arity, cmd) -> do
    let (pkt, _) = encode (newSeed (fromIntegral i * 7919)) cmd
    check ("length " ++ show cmd) (B.length pkt == 24)
    check ("round trip " ++ show (i, cmd)) (decode arity pkt == Just cmd)

  -- Stripe bit-packing round-trips at field extremes.
  forM_ [Stripe 0 0 1 0 0, Stripe 479 639 1023 0xFFFFF 0x7FFF, Stripe 123 456 7 4321 999] $ \s ->
    check ("stripe " ++ show s) (decodeStripe (encodeStripe s) == s)

  -- A synthetic frame with two blobs: one spanning three lines, one single stripe.
  let blobA = [Stripe 100 200 4 (6 * 50) (4 * 50), Stripe 101 199 6 (15 * 50) (6 * 50), Stripe 102 200 4 (6 * 50) (4 * 50)]
      blobB = [Stripe 300 50 5 (10 * 20) (5 * 20)]
      payload = B.concat (map encodeStripe (blobA ++ blobB))
      header = B.pack [0x07, 0x10, 0x05, 0x07 `xor` 0x10 `xor` 0x05 `xor` 0xAA]
      len = B.length payload
      trailer = B.pack [fromIntegral (len `shiftR` s .&. 0xFF) | s <- [24, 16, 8, 0]]
  case parseTransfer (B.concat [header, payload, trailer]) of
    [Frame 7 ss] -> do
      check "stripes decoded" (ss == blobA ++ blobB)
      case stripesToBlobs 1 1000 ss of
        [a, b] -> do
          check ("blob A centre " ++ show a) (near (blobX a) 201.5 && near (blobY a) 101)
          check ("blob B centre " ++ show b) (near (blobX b) 52 && near (blobY b) 300)
          check "blob A pixels" (blobPoints a == 14)
        bs -> check ("two blobs, got " ++ show bs) False
    other -> check ("frame parse, got " ++ show other) False

  check "bad checksum rejected" $ case parseTransfer (B.pack [1, 0x10, 5, 0, 0, 0, 0, 0]) of
    [Corrupt _ _] -> True
    _ -> False

  check "status reply" $ fmap statusReady (parseStatus (B.pack [0x07, 0x20, 0, 1, 0, 0, 0])) == Just True

  n <- readIORef failures
  if n == 0 then putStrLn "all tests passed" else exitFailure
  where
    near a b = abs (a - b) < 1e-9
