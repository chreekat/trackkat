module Main (main) where

import Control.Monad (forM_, unless)
import Data.Bits (shiftR, xor, (.&.))
import qualified Data.ByteString as B
import Data.IORef
import Data.Maybe (isNothing)
import System.Exit (exitFailure)

import TrackKat.Frame
import TrackKat.Linear
import TrackKat.Pose
import TrackKat.Protocol
import TrackKat.Tracker

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
    case fst (parseStream (B.concat [header, payload, trailer])) of
        [Frame 7 ss] -> do
            check "stripes decoded" (ss == blobA ++ blobB)
            case stripesToBlobs 1 1000 ss of
                [a, b] -> do
                    check ("blob A centre " ++ show a) (near (blobX a) 201.5 && near (blobY a) 101)
                    check ("blob B centre " ++ show b) (near (blobX b) 52 && near (blobY b) 300)
                    check "blob A pixels" (blobPoints a == 14)
                bs -> check ("two blobs, got " ++ show bs) False
        other -> check ("frame parse, got " ++ show other) False

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
    forM_ [0 .. B.length stream] $ \i -> forM_ [i .. B.length stream] $ \j -> do
        let (ps, done) = feed [B.take i stream, B.take (j - i) (B.drop i stream), B.drop j stream]
        check ("stream split at " ++ show (i, j)) (counters ps == [0x7E, 0x7F, 0x80] && null (corrupt ps) && done)

    let truncated = B.take 20 (frame 0x81 blobA) <> frame 0x82 blobB
        (afterLoss, _) = feed [truncated]
    check ("frame that lost its tail " ++ show afterLoss) (counters afterLoss == [0x82] && length (corrupt afterLoss) == 1)

    let (afterJunk, _) = feed [B.pack [0x00, 0x99, 0x42], frame 0x10 blobB]
    check ("junk before a frame " ++ show afterJunk) (counters afterJunk == [0x10] && length (corrupt afterJunk) == 1)

    let (withReply, _) = feed [frame 0x20 blobB <> B.pack [0x07, 0x20, 0, 1, 0, 0, 0] <> frame 0x21 []]
    check
        ("status reply between frames " ++ show withReply)
        (counters withReply == [0x20, 0x21] && length [() | StatusReply _ <- withReply] == 1)

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
                let hp = relativePose trackClip cameraLevel reference sol
                    V3 ex ey ez = (mulMV (poseR truth) (mPivot trackClip) ^+^ poseT truth) ^-^ (mPivot trackClip ^+^ V3 0 0 700)
                check ("reprojection " ++ show (y, p, r, err)) (err < 1e-3)
                check
                    ("angles " ++ show (y, p, r) ++ " got " ++ show hp)
                    (all (< 0.01) [abs (yaw hp - y), abs (pitch hp - p), abs (roll hp - r)])
                check ("translation " ++ show hp) (all (< 0.1) [abs (tx hp - ex), abs (ty hp - ey), abs (tz hp - ez)])

    -- With no previous pose, start from "facing the camera"; fine for modest angles.
    forM_ [(15, 5, 0), (-10, -10, 5)] $ \(y, p, r) -> do
        let truth = Pose (rotYXZ y p r) (V3 40 (-30) 650)
            img = (project defaultIntrinsics truth (mLeft trackClip), project defaultIntrinsics truth (mRight trackClip), project defaultIntrinsics truth (mTop trackClip))
        case solvePose defaultIntrinsics trackClip Nothing img of
            Just (sol, _) ->
                let hp = relativePose trackClip cameraLevel reference sol
                 in check
                        ("cold start " ++ show (y, p, r) ++ " got " ++ show hp)
                        (all (< 0.01) [abs (yaw hp - y), abs (pitch hp - p), abs (roll hp - r)])
            Nothing -> check "cold start solved" False

    -- Headset clip, post leaning back: from a cold start the solver should pick
    -- the right one of the two mirror-image solutions.
    let headset = maybe (error "bad test triangle") id (triangle 45 120 150 150)
    forM_ [(0, 0, 0), (12, -8, 0), (-10, 10, 5)] $ \(y, p, r) -> do
        let truth = Pose (rotYXZ y p r) (V3 20 (-150) 700)
            img = (project defaultIntrinsics truth (mLeft headset), project defaultIntrinsics truth (mRight headset), project defaultIntrinsics truth (mTop headset))
        case solvePose defaultIntrinsics headset Nothing img of
            Just (sol, _) ->
                let hp = relativePose headset cameraLevel reference sol
                 in check
                        ("headset cold start " ++ show (y, p, r) ++ " got " ++ show hp)
                        (all (< 0.01) [abs (yaw hp - y), abs (pitch hp - p), abs (roll hp - r)])
            Nothing -> check "headset cold start solved" False

    -- Level axes from turning the head left and right, seen by a camera tipped
    -- 13 degrees down and 4 to its right.
    let tilt = rodrigues (V3 0 0 (deg 4)) `mulMM` rodrigues (V3 (deg 13) 0 0)
        seenFrom rLevel tLevel = Pose (tilt `mulMM` rLevel) (mulMV tilt tLevel)
        neutral = seenFrom identity (V3 0 0 700)
        observe truth =
            let img = (project defaultIntrinsics truth (mLeft headset), project defaultIntrinsics truth (mRight headset), project defaultIntrinsics truth (mTop headset))
             in fst <$> solvePose defaultIntrinsics headset (Just truth) img
        relativeRot pose = poseR pose `mulMM` transpose (poseR neutral)
        sweep = [seenFrom (rotYXZ y 0 0) (V3 0 0 700) | y <- [-40, -30 .. 40]]
    case levelFromYaws . map relativeRot =<< traverse observe sweep of
        Nothing -> check "level from yaws" False
        Just level -> do
            let (down, right) = cameraTilt level
            check ("camera tilt " ++ show (down, right)) (abs (down - 13) < 0.01 && abs (right - 4) < 0.01)
            forM_ [(20, 10, 5), (-30, -15, 0), (0, 0, 25)] $ \(y, p, r) ->
                case observe (seenFrom (rotYXZ y p r) (V3 0 0 700)) of
                    Nothing -> check "levelled pose solved" False
                    Just sol -> do
                        let hp = relativePose headset level neutral sol
                        check
                            ("levelled pose " ++ show (y, p, r) ++ " got " ++ show hp)
                            (all (< 0.01) [abs (yaw hp - y), abs (pitch hp - p), abs (roll hp - r)])
    check "too little turning" $ isNothing (levelFromYaws [identity, rotYXZ 2 0 0])

    -- Tracker: requests wait for a tracked frame; losing the clip is reported.
    let square (u, v) =
            [ Stripe line (round u - 2) 5 (10 * 50) (5 * 50)
            | line <- [round v - 2 .. round v + 2]
            ]
        clipAt pose = concatMap (square . project defaultIntrinsics pose) [mLeft headset, mRight headset, mTop headset]
        facing = Pose identity (V3 0 0 700)
        turned = Pose (rotYXZ 20 0 0) (V3 0 0 700)
        t0 = newTracker defaultIntrinsics headset
        (t1, firstOut, firstEvents) = step 0 (clipAt facing) t0
        (t2, lostOut, lostEvents) = step 0.1 (take 5 (clipAt facing)) (request Recenter t1)
        (_, laterOut, laterEvents) = step 0.2 (clipAt turned) t2
    check ("first frame recenters: " ++ show (firstOut, firstEvents)) $ case (firstOut, firstEvents) of
        (Tracked hp _ _, [Recentered]) -> all ((< 0.01) . abs) [yaw hp, pitch hp, roll hp]
        _ -> False
    check ("one blob is lost: " ++ show (lostOut, lostEvents)) $ case (lostOut, lostEvents) of
        (Lost 1, []) -> True
        _ -> False
    check ("pending recenter waits for tracking: " ++ show (laterOut, laterEvents)) $ case (laterOut, laterEvents) of
        (Tracked hp _ _, [Recentered]) -> all ((< 0.01) . abs) [yaw hp, pitch hp, roll hp]
        _ -> False

    check "identify" $ identify [(297, 154), (254, 122), (249, 154)] == Just ((249, 154), (297, 154), (254, 122))

    n <- readIORef failures
    if n == 0 then putStrLn "all tests passed" else exitFailure
  where
    near a b = abs (a - b) < 1e-9
