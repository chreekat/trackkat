module Main (main) where

import Control.Monad (forM_, unless)
import Data.Bits (shiftR, xor, (.&.))
import qualified Data.ByteString as B
import Data.Either (isLeft)
import Data.IORef
import Data.List (mapAccumL)
import Data.Maybe (isNothing)
import System.Exit (exitFailure)

import TrackKat.Calibration
import TrackKat.Config
import TrackKat.Drift
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
        neck = V3 0 160 50
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
                let hp = relativePose cameraLevel neck reference sol
                    V3 ex ey ez = (mulMV (poseR truth) neck ^+^ poseT truth) ^-^ (neck ^+^ V3 0 0 700)
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
                let hp = relativePose cameraLevel neck reference sol
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
                let hp = relativePose cameraLevel neck reference sol
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
                        let hp = relativePose level neck neutral sol
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
        t0 = newTracker defaultIntrinsics headset defaultCalibration
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
            == Right defaultConfig{recenterBindings = [Key [Ctrl, Shift] "F12"], pauseBindings = [Key [] "Pause"]}
    check "binding codes" $ case parseBinding "Alt+Ctrl+NumPad5" of
        Right b@(Key mods k) -> (modifierMask mods, virtualKey k, renderBinding b) == (3, 0x65, "Ctrl+Alt+NumPad5")
        _ -> False
    let warthog = Joystick "Throttle - HOTAS Warthog"
    check "joystick button" $
        parseConfig "recenter = Throttle - HOTAS Warthog  button 23\npause = F9, Joystick - HOTAS Warthog (2) Button 4, Ctrl+P\n"
            == Right
                defaultConfig
                    { recenterBindings = [JoystickButton (warthog 1) 23]
                    , pauseBindings = [Key [] "F9", JoystickButton (Joystick "Joystick - HOTAS Warthog" 2) 4, Key [Ctrl] "P"]
                    }
    forM_
        [ [JoystickButton (warthog 1) 1]
        , [JoystickButton (warthog 3) 128, Key [Alt] "F1"]
        , [JoystickButton (Joystick "Controller (Xbox One For Windows)" 1) 7]
        , [JoystickButton (Joystick "T.16000M (HID)" 2) 256, JoystickButton (Joystick "VPC Stick MT-50CM2 + 3D" 1) 31]
        ]
        $ \bs -> check ("bindings round-trip: " ++ renderBindings bs) $ parseBindings (renderBindings bs) == Right bs
    check "rendered button" $ renderBindings [Key [] "F12", JoystickButton (warthog 2) 23] == "F12, Throttle - HOTAS Warthog (2) button 23"
    check "button out of range" $ parseConfig "pause = Stick button 257" == Left "line 1: joystick buttons are numbered from 1 to 256"
    check "button zero" $ parseBinding "Stick button 0" == Left "joystick buttons are numbered from 1 to 256"
    check "button without number" $ parseBinding "Stick button" == Left "joystick buttons are numbered from 1 to 256"
    check "button without joystick" $
        parseBinding "button 3" == Left "a joystick button needs the joystick's name, like Throttle - HOTAS Warthog button 23"
    check "empty binding in a list" $ parseBindings "F12, " == Left "empty key binding"
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
    check "button change described" $
        reloaded "recenter = F12, Stick button 2\n" == Right "Settings reloaded: recenter = F12, Stick button 2"
    check "smoothing turned off" $ reloaded "smoothing = off\nsteadiness = 80\n" == Right "Settings reloaded: smoothing = off"

    -- Learning from everyday movement: the camera tipped 13 degrees down and
    -- 4 to its right watches a head turn about a neck pivot, with a little
    -- noise in every pose. Ninety seconds of glancing left and right while
    -- slowly nodding, then twenty of nodding alone.
    let truePivot = V3 (-60) 90 70
        neckAt = V3 0 50 800
        wobble :: Int -> Double -> Double
        wobble i k = let x = sin (fromIntegral i * 12.9898 + k * 78.233) * 43758.5453 in x - fromIntegral (floor x :: Int) - 0.5
        -- The head turned as given, its pivot at the given place in level axes.
        headAt place y p r = seenFrom (rotYXZ y p r) (place ^-^ mulMV (rotYXZ y p r) truePivot)
        noisy i (Pose r t) =
            Pose
                (rodrigues (V3 (deg 0.2 * wobble i 1) (deg 0.2 * wobble i 2) (deg 0.2 * wobble i 3)) `mulMM` r)
                (t ^+^ V3 (wobble i 4) (wobble i 5) (2 * wobble i 6))
        glancing sway =
            [ (time, noisy i (headAt (neckAt ^+^ sway time) y p 0))
            | i <- [0 .. 120 * 110 :: Int]
            , let time = fromIntegral i / 120 :: Double
                  (y, p)
                    | time < 90 = (35 * sin (2 * pi * time / 2.5), 8 * sin (2 * pi * time / 7))
                    | otherwise = (0, 15 * sin (2 * pi * time / 2))
            ]
        runLearner frames = foldl (\(l, rs) (time, pose) -> let (l', new) = learn time pose l in (l', rs ++ new)) (newLearner defaultCalibration, []) frames
        (learnt, refinements) = runLearner (glancing (const (V3 0 0 0)))
        c = learnerCalibration learnt
        (tiltDown, tiltRight) = cameraTilt (estimatedLevel c)
        (appliedDown, appliedRight) = cameraTilt (appliedLevel learnt)
        straight = headAt neckAt 0 0 0
    check ("learnt level " ++ show (tiltDown, tiltRight)) $ abs (tiltDown - 13) < 0.5 && abs (tiltRight - 4) < 0.5
    check ("applied level " ++ show (appliedDown, appliedRight)) $ abs (appliedDown - 13) < 0.5 && abs (appliedRight - 4) < 0.5
    check ("learnt pivot " ++ show (estimatedPivot c, pivotTurns c)) $ norm (estimatedPivot c ^-^ truePivot) < 5
    check ("applied pivot " ++ show (appliedPivot learnt)) $ norm (appliedPivot learnt ^-^ truePivot) < 5
    check ("refinements reported " ++ show (length refinements)) $
        LevelRefined `elem` refinements && PivotRefined `elem` refinements
    forM_ [(30, 0), (-40, 0), (0, 15), (25, -10)] $ \(y, p) -> do
        let hp = relativePose (appliedLevel learnt) (appliedPivot learnt) straight (headAt neckAt y p 0)
            moved = norm (V3 (tx hp) (ty hp) (tz hp))
        check ("turning alone doesn't move " ++ show (y, p) ++ " got " ++ show hp) $
            moved < 3 && abs (yaw hp - y) < 0.5 && abs (pitch hp - p) < 0.5 && abs (roll hp) < 0.5
    let unlearnt = relativePose cameraLevel (appliedPivot (newLearner defaultCalibration)) straight (headAt neckAt 30 0 0)
    check ("without learning, turning moves " ++ show unlearnt) $ norm (V3 (tx unlearnt) (ty unlearnt) (tz unlearnt)) > 20
    -- A clip worn rolled 15 degrees sees a pivot square below and behind the
    -- head off to one side in its own axes; described at the centre, it is
    -- square again.
    let worn = rotYXZ 0 0 15
        inClip = mulMV (transpose worn) (V3 0 150 60)
    check ("pivot in clip axes is off to one side " ++ show inClip) $ let V3 x _ _ = inClip in abs x > 30
    check "pivot described square to the head" $ norm (inLevelAxes cameraLevel (Pose worn (V3 0 0 700)) inClip ^-^ V3 0 150 60) < 1e-9

    -- Swaying the body while glancing mustn't drag the pivot far.
    let (swaying, _) = runLearner (glancing (\time -> V3 (30 * sin (2 * pi * time / 3.7)) 0 (20 * sin (2 * pi * time / 5.3))))
    check ("pivot while swaying " ++ show (estimatedPivot (learnerCalibration swaying))) $
        norm (estimatedPivot (learnerCalibration swaying) ^-^ truePivot) < 15

    -- Saving and loading.
    let saved = renderCalibration c
        closeV a b = norm (a ^-^ b) < 1e-3
    case parseCalibration saved of
        Left err -> check ("calibration loads: " ++ err) False
        Right loaded -> do
            check "calibration round-trips" $ renderCalibration loaded == saved
            check "loaded level" $ angleBetween (levelUp (estimatedLevel loaded)) (levelUp (estimatedLevel c)) < 1e-3
            check "loaded pivot" $ closeV (estimatedPivot loaded) (estimatedPivot c)
            check "loaded evidence" $ abs (levelTurning loaded - levelTurning c) < 1e-3 && abs (pivotTurns loaded - pivotTurns c) < 1e-3
            check "loaded calibration applies at once" $ closeV (appliedPivot (newLearner loaded)) (estimatedPivot c)
    check "empty calibration file" $ fmap renderCalibration (parseCalibration "") == Right (renderCalibration defaultCalibration)
    check "bad up" $ isLeft (parseCalibration "up = 0 0 0\n")
    check "bad line" $ isLeft (parseCalibration "# fine\nnonsense\n")
    check "unknown calibration setting" $ either id (const "") (parseCalibration "tilt = 3\n") == "line 1: unknown setting \"tilt\""

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

    -- Drift: the centre following where the head rests. Poses at 120 a
    -- second along a path, with a little noise.
    let noise i =
            HeadPose (0.1 * wobble i 7) (0.1 * wobble i 8) (0.1 * wobble i 9) (0.5 * wobble i 10) (0.5 * wobble i 11) (0.5 * wobble i 12)
        add a b = HeadPose (yaw a + yaw b) (pitch a + pitch b) (roll a + roll b) (tx a + tx b) (ty a + ty b) (tz a + tz b)
        atCentre = HeadPose 0 0 0 0 0 0
        along :: Double -> Double -> (Double -> HeadPose) -> [(Double, HeadPose)]
        along from to path = [(t, add (path t) (noise i)) | i <- [round (from * 120) .. round (to * 120) - 1 :: Int], let t = fromIntegral i / 120]
        -- From 0 to 1 between two times.
        ease a b t = let u = max 0 (min 1 ((t - a) / (b - a))) in u * u * (3 - 2 * u)
        runDrift setting d0 = mapAccumL (\d (t, hp) -> let (o, d', _) = driftCentre setting t hp d in (d', (t, hp, o))) d0
        offsets = map (\(_, hp, o) -> [yaw hp - yaw o, pitch hp - pitch o, roll hp - roll o, tx hp - tx o, ty hp - ty o, tz hp - tz o])
        biggestStep rows = maximum (0 : concat (zipWith (zipWith (\a b -> abs (a - b))) rows (drop 1 rows)))
        meanOf f rows = sum (map f rows) / fromIntegral (length rows)
        during a b rows = [r | r@(t, _, _) <- rows, t >= a, t < b]
        out f (_, _, o) = f o

    -- Shifting 30 mm sideways in the chair, then resting there.
    let shifting = along 0 150 (\t -> atCentre{tx = 30 * ease 10 15 t})
        (_, shifted) = runDrift Drift newDrifter shifting
        restingX = meanOf (out tx) (during 140 150 shifted)
    check ("a shift is absorbed " ++ show restingX) $ abs restingX < 2
    check ("half a minute after the shift, the centre hasn't moved " ++ show (meanOf (out tx) (during 29 30 shifted))) $
        abs (meanOf (out tx) (during 29 30 shifted) - 30) < 1
    check ("the centre moves smoothly " ++ show (biggestStep (offsets shifted))) $ biggestStep (offsets shifted) < 0.01
    -- Shaped for a game, the drift is the only difference, and it is smooth.
    let shapedAll rows = snd (mapAccumL (\sh (t, hp) -> let (p, sh') = shape (response defaultConfig) t hp sh in (sh', [yaw p, pitch p, roll p, tx p, ty p, tz p])) newShaper rows)
        gameDifference = zipWith (zipWith (-)) (shapedAll [(t, hp) | (t, hp, _) <- shifted]) (shapedAll [(t, o) | (t, _, o) <- shifted])
    check ("the game sees no jump " ++ show (biggestStep gameDifference)) $ biggestStep gameDifference < 0.05

    -- Watching something at 4 o'clock for a minute, then looking ahead again.
    let looking = along 0 105 (\t -> atCentre{yaw = 20 * (ease 30 31 t - ease 91 92 t), tx = -8 * (ease 30 31 t - ease 91 92 t)})
        (_, looked) = runDrift Drift newDrifter looking
        yawOffset = maximum [abs (yaw hp - yaw o) | (_, hp, o) <- looked]
    check ("a held look doesn't move the centre " ++ show yawOffset) $ yawOffset < 0.01
    check ("back from a look, ahead is centred " ++ show (meanOf (out yaw) (during 100 105 looked))) $
        abs (meanOf (out yaw) (during 100 105 looked)) < 0.1
    -- A lean held still for a quarter of a minute, then back.
    let leaning = along 0 70 (\t -> atCentre{tx = 30 * (ease 30 30.5 t - ease 45 45.5 t)})
        (_, leant) = runDrift Drift newDrifter leaning
    check ("a held lean doesn't move the centre " ++ show (maximum (map (abs . (!! 3)) (offsets leant)))) $
        maximum (map (abs . (!! 3)) (offsets leant)) < 0.5

    -- A chair turned 2.5 degrees, and the head resting a little tilted.
    let chairTurned = along 0 400 (const atCentre{yaw = 2.5, roll = 1.5})
        (_, turnedOut) = runDrift Drift newDrifter chairTurned
    check ("a turned chair is absorbed " ++ show (meanOf (out yaw) (during 390 400 turnedOut))) $
        abs (meanOf (out yaw) (during 390 400 turnedOut)) < 0.2 && abs (meanOf (out roll) (during 390 400 turnedOut)) < 0.2
    check ("rotation waits for a minute of rest " ++ show (meanOf (out yaw) (during 59 60 turnedOut))) $
        abs (meanOf (out yaw) (during 59 60 turnedOut) - 2.5) < 0.05

    -- Paused, or with the clip out of sight, the centre stays put; and
    -- when tracking resumes it carries on without a jump.
    let (drifting, beforePause) = runDrift Drift newDrifter (along 0 40 (const atCentre{tx = 30}))
        (pausedDrifter, whilePaused) = runDrift NoDrift drifting (along 40 100 (const atCentre{tx = 30}))
        (_, afterPause) = runDrift Drift pausedDrifter (along 100 110 (const atCentre{tx = 30}))
        offsetAt rows = case reverse rows of
            (_, hp, o) : _ -> tx hp - tx o
            [] -> 0
    check ("drifting before the pause " ++ show (offsetAt beforePause)) $ offsetAt beforePause > 5
    check "paused, the centre stays" $ all (\(_, hp, o) -> abs (tx hp - tx o - offsetAt beforePause) < 1e-9) whilePaused
    check ("after a pause, the centre carries on " ++ show (offsetAt afterPause)) $
        offsetAt afterPause > offsetAt beforePause && biggestStep (offsets (beforePause ++ whilePaused ++ afterPause)) < 0.01
    let (_, regained) = runDrift Drift (interrupted drifting) (along 100 110 (const atCentre{tx = 30}))
    check ("after losing the clip, the centre carries on " ++ show (offsetAt regained)) $
        offsetAt regained > offsetAt beforePause && biggestStep (offsets (beforePause ++ regained)) < 0.01
    -- Recentering starts afresh: the pose passes through unchanged.
    let (_, fresh) = runDrift Drift newDrifter (along 0 10 (const atCentre{tx = 30}))
    check "recentered, nothing has drifted" $ all (all ((< 1e-12) . abs)) (offsets fresh)
    check "a fresh centre is described" $ describeDrift (driftOffset newDrifter) == "Centre: where you recentered."
    -- Turned off, nothing drifts.
    let (_, undrifted) = runDrift NoDrift newDrifter shifting
    check "drift off" $ all (all ((< 1e-12) . abs)) (offsets undrifted)
    -- News comes now and then.
    let newsCount = length [() | Just _ <- snd (mapAccumL (\d (t, hp) -> let (_, d', n) = driftCentre Drift t hp d in (d', n)) newDrifter shifting)]
    check ("drift mentioned now and then " ++ show newsCount) $ newsCount >= 2 && newsCount <= 3

    check "drift setting" $ fmap drift (parseConfig "drift = off\n") == Right NoDrift
    check "drift default" $ drift defaultConfig == Drift
    check "bad drift" $ parseConfig "drift = maybe" == Left "line 1: drift is on or off"
    check "drift change described" $
        (describeChanges defaultConfig <$> parseConfig "drift = off\n") == Right "Settings reloaded: drift = off"
    check "drift edited" $ editSettings [("drift", "off")] "lean = view\n" == Right "lean = view\ndrift = off\n"
    check "drift shown" $ lookup "drift" (settingValues "") == Just "on"

    -- Editing the settings file from trackkat's window.
    let userFile = "# mine\npause=Pause # TrackIR style\nsmoothing = off\nsteadiness = 70\npause = F9\n"
    check ("values as written: " ++ show (settingValues userFile)) $
        take 5 (settingValues userFile)
            == [("recenter", "F12"), ("pause", "F9"), ("smoothing", "off"), ("steadiness", "70"), ("responsiveness", "45")]
    check "names in file order" $ settingNames == map fst (settingValues "")
    check "unchanged values leave the file alone" $ editSettings (settingValues userFile) userFile == Right userFile
    check ("edit keeps comments: " ++ show (editSettings [("pause", "Ctrl+F9"), ("yaw", "1:5")] userFile)) $
        editSettings [("pause", "Ctrl+F9"), ("yaw", "1:5")] userFile
            == Right "# mine\npause=Pause # TrackIR style\nsmoothing = off\nsteadiness = 70\npause = Ctrl+F9\nyaw = 1:5\n"
    check "edit keeps a line's comment" $
        editSettings [("pause", "F8")] "pause=Pause # TrackIR style" == Right "pause = F8 # TrackIR style\n"
    check "edit names a bad value" $
        editSettings [("steadiness", "101")] userFile == Left "steadiness: steadiness is a number from 0 to 100"
    check "edit reports the rest of the file" $
        editSettings [("pause", "F8")] "bogus\n" == Left "line 1: expected name = value"
    check "binding from codes" $ bindingFromCodes 0x6 0x78 == Just (Key [Ctrl, Shift] "F9")
    check "binding from an unnamed key" $ bindingFromCodes 0 0xBA == Nothing

    check "identify" $ identify [(297, 154), (254, 122), (249, 154)] == Just ((249, 154), (297, 154), (254, 122))

    n <- readIORef failures
    if n == 0 then putStrLn "all tests passed" else exitFailure
  where
    near a b = abs (a - b) < 1e-9
