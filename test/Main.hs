module Main (main) where

import Control.Monad (forM_, unless)
import Data.Bits (shiftR, xor, (.&.))
import qualified Data.ByteString as B
import Data.IORef
import Data.Maybe (isNothing)
import System.Exit (exitFailure)

import TrackKat.Config
import TrackKat.Frame
import TrackKat.GamePose
import TrackKat.Linear
import TrackKat.Pose
import TrackKat.Protocol
import TrackKat.Response
import TrackKat.Stats
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

    -- Settings and key bindings.
    check "default config round-trips" $ parseConfig (renderConfig defaultConfig) == Right defaultConfig
    check "bindings" $
        parseConfig "# comment\nrecenter = ctrl + shift+f12\n\npause=Pause # TrackIR style\n"
            == Right defaultConfig{recenterKey = Binding [Ctrl, Shift] "F12", pauseKey = Binding [] "Pause"}
    check "binding codes" $
        fmap (\b -> (modifierMask b, virtualKey b, renderBinding b)) (parseBinding "Alt+Ctrl+NumPad5") == Right (3, 0x65, "Ctrl+Alt+NumPad5")
    check "unknown key reported" $ parseConfig "recenter = F99\n" == Left "line 1: unknown key \"F99\""
    check "unknown setting reported" $ parseConfig "\nfoo = F1\n" == Left "line 2: unknown setting \"foo\""

    -- Game conventions: yaw and roll positive to the left, pitch positive down.
    check "game pose signs" $
        toGamePose (HeadPose{yaw = 90, pitch = 90, roll = 90, tx = 1, ty = 2, tz = 3})
            == GamePose (realToFrac (-pi / 2 :: Double)) (realToFrac (pi / 2 :: Double)) (realToFrac (pi / 2 :: Double)) 1 (-2) 3

    -- Performance window: means, maxima and the summary line.
    let w = sawGCs 2 0.00015 (sawCorrupt (sawFrame 0.0002 0.009 12 (sawFrame 0.0004 0.008 40 emptyWindow)))
    check ("stats summary: " ++ summary w) $
        summary w == "latency 0.30 ms (max 0.40)  gap max 9 ms  stripes max 40  GC 2 (max 0.15 ms)  corrupt 1"
    check "empty stats" $ summary emptyWindow == "latency 0.00 ms (max 0.00)  gap max 0 ms  stripes max 0  GC 0 (max 0.00 ms)"

    -- Response curves.
    let deadzone = Curve [(0.5, 0), (1.5, 1)]
        lookBehind = Curve [(10, 10), (45, 120)]
        close a b = abs (a - b) < 1e-9
    check "curve deadzone" $ all (uncurry close) [(applyCurve deadzone 0.3, 0), (applyCurve deadzone 1, 0.5), (applyCurve deadzone (-1), -0.5), (applyCurve deadzone 10, 9.5)]
    check "curve gain" $ all (uncurry close) [(applyCurve lookBehind 5, 5), (applyCurve lookBehind 45, 120), (applyCurve lookBehind (-45), -120), (applyCurve lookBehind 52, 142)]

    -- Shaping: jumps a head can't make are held off unless they persist;
    -- smoothing settles still jitter and follows steps.
    let linear = Curve [(1, 1)]
        plain = Response NoSmoothing linear linear linear linear linear linear LeanWithScreen
        smooth = plain{smoothing = Smoothing 1 0.05}
        at y = HeadPose{yaw = y, pitch = 0, roll = 0, tx = 0, ty = 0, tz = 0}
        run r frames = go newShaper (zip [0, 1 / 120 ..] frames)
          where
            go _ [] = []
            go sh ((t, p) : rest) = let (out, sh') = shape r t p sh in yaw out : go sh' rest
    check ("one-frame spike ignored: " ++ show (run plain (map at [0, 1, 60, 2, 3]))) $
        run plain (map at [0, 1, 60, 2, 3]) == [0, 1, 1, 2, 3]
    check ("persistent jump accepted: " ++ show (run plain (map at [0, 60, 61, 62, 63]))) $
        run plain (map at [0, 60, 61, 62, 63]) == [0, 0, 0, 62, 63]
    let jitter = run smooth (map at (take 240 (cycle [0.4, -0.4])))
        steady = run smooth (map at (replicate 240 10))
    check ("smoothing settles jitter: " ++ show (maximum (map abs (drop 120 jitter)))) $ maximum (map abs (drop 120 jitter)) < 0.05
    check "smoothing reaches a held value" $ abs (last steady - 10) < 1e-6

    -- Settings for smoothing and curves.
    check "curve and smoothing settings" $
        fmap response (parseConfig "yaw = 10:10 45:120\nsmoothing = off\n")
            == Right ((response defaultConfig){smoothing = NoSmoothing, yawCurve = lookBehind})
    let smoothingOf = fmap (smoothing . response) . parseConfig
        closeSmoothing (Smoothing a b) (Smoothing c d) = close a c && close b d
        closeSmoothing a b = a == b
    check "default config round-trips" $ parseConfig (renderConfig defaultConfig) == Right defaultConfig
    check "smoothing endpoints" $
        closeSmoothing (tuned 0 0) (Smoothing 20 0.001) && closeSmoothing (tuned 100 100) (Smoothing 0.2 1)
    check ("default smoothing: " ++ show (smoothing (response defaultConfig))) $ case smoothing (response defaultConfig) of
        Smoothing mc b -> abs (mc - 0.317) < 0.001 && abs (b - 0.0224) < 0.0001
        NoSmoothing -> False
    check "steadiness and responsiveness" $ smoothingOf "steadiness = 100\nresponsiveness = 0" == Right (tuned 100 0)
    check "smoothing off whatever the order" $ smoothingOf "smoothing = off\nsteadiness = 80" == Right NoSmoothing
    check "smoothing back on" $ smoothingOf "smoothing = off\nsmoothing = on\nsteadiness = 80" == Right (tuned 80 45)
    check "steadiness out of range" $ parseConfig "\nsteadiness = 101" == Left "line 2: steadiness is a number from 0 to 100"
    check "responsiveness not a number" $ parseConfig "responsiveness = lots" == Left "line 1: responsiveness is a number from 0 to 100"
    check "old smoothing form rejected" $ parseConfig "smoothing = 1 0.05" == Left "line 1: smoothing is on or off"
    check "curve inputs must increase" $ parseConfig "pitch = 5:5 3:9" == Left "line 1: curve inputs must be positive and increasing"
    check "curve points must be numbers" $ parseConfig "z = 1:x" == Left "line 1: expected input:output, not \"1:x\""

    let reloaded text = describeChanges defaultConfig <$> parseConfig text
    check "nothing changed" $ reloaded "# just a comment\n" == Right "Settings reloaded."
    check ("changes described: " ++ show (reloaded "pause = Ctrl+F9\nsteadiness = 80\nyaw = 10:10 45:120\n")) $
        reloaded "pause = Ctrl+F9\nsteadiness = 80\nyaw = 10:10 45:120\n"
            == Right "Settings reloaded: pause = Ctrl+F9, steadiness = 80, yaw = 10:10 45:120"
    check "smoothing turned off" $ reloaded "smoothing = off\nsteadiness = 80\n" == Right "Settings reloaded: smoothing = off"

    -- Leaning with the view: the view turned 160 degrees to the right, to
    -- about 5 o'clock, and a lean to the user's left goes toward the
    -- aircraft's right (out of the right side) and a little forward.
    let lookingBack = toGamePose (leanWithView HeadPose{yaw = 160, pitch = 0, roll = 0, tx = 10, ty = 5, tz = 0})
    check ("lean out of the right side " ++ show lookingBack) $
        gpYaw lookingBack < 0
            && abs (gpX lookingBack - realToFrac (10 * cos (deg 160))) < 1e-4
            && gpX lookingBack < -9
            && gpZ lookingBack < -3
            && gpY lookingBack == -5
    let lookingRight = leanWithView HeadPose{yaw = 90, pitch = 0, roll = 0, tx = 0, ty = 0, tz = -10}
    check ("lean toward the screen while looking right goes right " ++ show lookingRight) $
        abs (tx lookingRight + 10) < 1e-9 && abs (tz lookingRight) < 1e-9
    let ahead = leanWithView HeadPose{yaw = 0, pitch = 30, roll = 0, tx = 4, ty = 0, tz = -7}
    check "looking ahead, leaning is unchanged" $ abs (tx ahead - 4) < 1e-12 && abs (tz ahead + 7) < 1e-12
    -- The lean turns by the yaw the game shows, after the curves.
    let viewing = plain{yawCurve = Curve [(1, 6)], lean = LeanWithView}
        (shapedBack, _) = shape viewing 0 HeadPose{yaw = 160 / 6, pitch = 0, roll = 0, tx = 10, ty = 0, tz = 0} newShaper
    check ("lean follows the curved yaw " ++ show shapedBack) $
        abs (yaw shapedBack - 160) < 1e-9 && abs (tx shapedBack - 10 * cos (deg 160)) < 1e-9 && abs (tz shapedBack + 10 * sin (deg 160)) < 1e-9
    check "lean setting" $ fmap (lean . response) (parseConfig "lean = screen\n") == Right LeanWithScreen
    check "lean default" $ lean (response defaultConfig) == LeanWithView
    check "bad lean" $ parseConfig "lean = sideways" == Left "line 1: lean is view or screen"

    check "identify" $ identify [(297, 154), (254, 122), (249, 154)] == Just ((249, 154), (297, 154), (254, 122))

    n <- readIORef failures
    if n == 0 then putStrLn "all tests passed" else exitFailure
  where
    near a b = abs (a - b) < 1e-9
