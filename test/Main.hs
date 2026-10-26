module Main (main) where

import Control.Monad (forM_, unless)
import Data.Bits (shiftR, xor, (.&.))
import qualified Data.ByteString as B
import Data.IORef
import System.Exit (exitFailure)

import TrackKat.Frame
import TrackKat.Linear
import TrackKat.Pose
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

  -- Pose: project known head poses and recover them.
  let deg = (* (pi / 180))
      rotYXZ y p r = rodrigues (V3 0 (deg y) 0) `mulMM` rodrigues (V3 (deg p) 0 0) `mulMM` rodrigues (V3 0 0 (deg r))
      reference = Pose identity (V3 0 0 700)
      poses =
        [ (y, p, r, dt0)
        | (y, p, r) <- [(0, 0, 0), (20, 0, 0), (-35, 10, 0), (10, -15, 8), (0, 0, -20), (40, 20, 10)]
        , dt0 <- [V3 0 0 0, V3 30 (-20) 50]
        ]
  forM_ poses $ \(y, p, r, dt) -> do
    let truth = Pose (rotYXZ y p r) (V3 0 0 700 ^+^ dt)
        img = (project defaultIntrinsics truth (mLeft trackClip), project defaultIntrinsics truth (mRight trackClip), project defaultIntrinsics truth (mTop trackClip))
        -- Start from a pose a few degrees and mm off, as frame-to-frame tracking would.
        guess = Pose (rotYXZ (y + 3) (p - 2) (r + 2)) (V3 0 0 710 ^+^ dt)
    case solvePose defaultIntrinsics trackClip (Just guess) img of
      Nothing -> check ("solve " ++ show (y, p, r)) False
      Just (sol, err) -> do
        let hp = relativePose trackClip reference sol
            V3 ex ey ez = (mulMV (poseR truth) (mPivot trackClip) ^+^ poseT truth) ^-^ (mPivot trackClip ^+^ V3 0 0 700)
        check ("reprojection " ++ show (y, p, r, err)) (err < 1e-3)
        check ("angles " ++ show (y, p, r) ++ " got " ++ show hp)
          (all (< 0.01) [abs (yaw hp - y), abs (pitch hp - p), abs (roll hp - r)])
        check ("translation " ++ show hp) (all (< 0.1) [abs (tx hp - ex), abs (ty hp - ey), abs (tz hp - ez)])

  -- With no previous pose, start from "facing the camera"; fine for modest angles.
  forM_ [(15, 5, 0), (-10, -10, 5)] $ \(y, p, r) -> do
    let truth = Pose (rotYXZ y p r) (V3 40 (-30) 650)
        img = (project defaultIntrinsics truth (mLeft trackClip), project defaultIntrinsics truth (mRight trackClip), project defaultIntrinsics truth (mTop trackClip))
    case solvePose defaultIntrinsics trackClip Nothing img of
      Just (sol, _) -> let hp = relativePose trackClip reference sol
                        in check ("cold start " ++ show (y, p, r) ++ " got " ++ show hp)
                             (all (< 0.01) [abs (yaw hp - y), abs (pitch hp - p), abs (roll hp - r)])
      Nothing -> check "cold start solved" False

  -- Headset clip, post leaning back: from a cold start the solver should pick
  -- the right one of the two mirror-image solutions.
  let headset = maybe (error "bad test triangle") id (triangle 45 120 150 150)
  forM_ [(0, 0, 0), (12, -8, 0), (-10, 10, 5)] $ \(y, p, r) -> do
    let truth = Pose (rotYXZ y p r) (V3 20 (-150) 700)
        img = (project defaultIntrinsics truth (mLeft headset), project defaultIntrinsics truth (mRight headset), project defaultIntrinsics truth (mTop headset))
    case solvePose defaultIntrinsics headset Nothing img of
      Just (sol, _) -> let hp = relativePose headset reference sol
                        in check ("headset cold start " ++ show (y, p, r) ++ " got " ++ show hp)
                             (all (< 0.01) [abs (yaw hp - y), abs (pitch hp - p), abs (roll hp - r)])
      Nothing -> check "headset cold start solved" False

  check "identify" $ identify [(297, 154), (254, 122), (249, 154)] == Just ((249, 154), (297, 154), (254, 122))

  n <- readIORef failures
  if n == 0 then putStrLn "all tests passed" else exitFailure
  where
    near a b = abs (a - b) < 1e-9
