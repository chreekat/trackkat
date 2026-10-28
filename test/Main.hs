{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Exception (IOException, evaluate, try)
import Control.Monad (forM_, unless)
import Data.Bits (shiftR, xor, (.&.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import Data.Either (isLeft)
import Data.IORef
import Data.List (mapAccumL, sortOn)
import Data.Maybe (isNothing)
import GHC.Stats (gc, gcdetails_live_bytes, getRTSStats, getRTSStatsEnabled)
import System.Directory (createDirectory, doesFileExist, getTemporaryDirectory, removeDirectoryRecursive, removePathForcibly)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.FilePath (pathSeparator, (</>))
import System.IO (readFile')
import System.Mem (performMajorGC)

import Codec.Archive.Zip (addEntryToArchive, emptyArchive, fromArchive, toEntry)

import TrackKat.Calibration
import TrackKat.Config
import TrackKat.Frame
import TrackKat.GamePose
import TrackKat.Linear
import TrackKat.Pose
import TrackKat.Protocol
import TrackKat.Release
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
        t0 = newTracker defaultIntrinsics headset MaskLights defaultCalibration
        (t1, firstOut, firstEvents) = step 0 (clipAt facing) t0
        (t2, lostOut, lostEvents) = step 0.1 (take 5 (clipAt facing)) (request Recenter t1)
        (_, laterOut, laterEvents) = step 0.2 (clipAt turned) t2
    check ("first frame recenters: " ++ show (firstOut, firstEvents)) $ case (firstOut, firstEvents) of
        (Tracked{outPose = hp}, [Recentered]) -> all ((< 0.01) . abs) [yaw hp, pitch hp, roll hp]
        _ -> False
    check ("one blob is lost: " ++ show (lostOut, lostEvents)) $ case (lostOut, lostEvents) of
        (Lost 1 0, []) -> True
        _ -> False
    check ("pending recenter waits for tracking: " ++ show (laterOut, laterEvents)) $ case (laterOut, laterEvents) of
        (Tracked{outPose = hp}, [Recentered]) -> all ((< 0.01) . abs) [yaw hp, pitch hp, roll hp]
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
    check "mouse buttons" $
        parseBindings "MouseMiddle, ctrl+mouse4, Alt+Shift+Mouse5" == Right [Key [] "MouseMiddle", Key [Ctrl] "Mouse4", Key [Alt, Shift] "Mouse5"]
    check "mouse buttons round-trip" $
        let bs = [Key [Ctrl] "Mouse4", Key [] "MouseMiddle", Key [Win] "Mouse5"] in parseBindings (renderBindings bs) == Right bs
    check "mouse button codes" $ map virtualKey ["MouseMiddle", "Mouse4", "Mouse5"] == [4, 5, 6]
    check "mouse button chosen in the window" $ bindingFromCodes 0x2 0x05 == Just (Key [Ctrl] "Mouse4")
    forM_ ["MouseLeft", "Ctrl+MouseRight", "mouse1", "Mouse2"] $ \b ->
        check ("left and right mouse buttons refused: " ++ b) $
            parseBinding b == Left "the left and right mouse buttons can't be bound; use MouseMiddle, Mouse4 or Mouse5"
    check "mouse button refused in a config" $
        parseConfig "recenter = MouseLeft\n" == Left "line 1: the left and right mouse buttons can't be bound; use MouseMiddle, Mouse4 or Mouse5"
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
    let deadzone = Points [(0.5, 0), (1.5, 1)]
        lookBehind = Points [(10, 10), (45, 120)]
        close a b = abs (a - b) < 1e-9
    check "curve deadzone" $ all (uncurry close) [(applyCurve deadzone 0.3, 0), (applyCurve deadzone 1, 0.5), (applyCurve deadzone (-1), -0.5), (applyCurve deadzone 10, 9.5)]
    check "curve gain" $ all (uncurry close) [(applyCurve lookBehind 5, 5), (applyCurve lookBehind 45, 120), (applyCurve lookBehind (-45), -120), (applyCurve lookBehind 52, 142)]

    -- Multipliers with a centre precision.
    let expos = [Multiplier m p r | m <- [0.5, 2.5, 6, 7], p <- [0, 1, 25, 40, 75, 100], r <- [yawRange, pitchRange, leanRange]]
        across e = [reach e * t / 50 | t <- [-150 .. 150]]
    forM_ expos $ \e -> do
        let f = applyCurve (Expo e)
            xs = across e
        check ("expo never turns back " ++ show e) $ and (zipWith (<=) (map f xs) (drop 1 (map f xs)))
        check ("expo the same both sides " ++ show e) $ all (\x -> close (f (-x)) (negate (f x))) xs
        check ("expo reaches the end of the view " ++ show e) $ close (f (reach e)) (viewRange e) && close (f (-reach e)) (negate (viewRange e))
        check ("expo continues past the reach " ++ show e) $ abs (f (reach e * 1.000001) - viewRange e) < 1e-3
        check ("expo gentler near centre " ++ show e) $
            if precision e == 0
                then all (\x -> close (f x) (factor e * x)) xs
                else all (\x -> abs (f x) < factor e * abs x) [x | x <- xs, x /= 0, abs x < reach e]
    check "expo at the centre" $ applyCurve (Expo (Multiplier 6 40 yawRange)) 0 == 0
    check "expo reach" $ close (reach (Multiplier 6 40 yawRange)) 30

    -- Shaping: jumps a head can't make are held off unless they persist;
    -- smoothing settles still jitter and follows steps.
    let linear = Points [(1, 1)]
        plain = Response NoSmoothing linear linear linear linear linear linear LeanWithScreen
        smooth = plain{smoothing = Smoothing 1 0.05}
        at y = HeadPose{yaw = y, pitch = 0, roll = 0, tx = 0, ty = 0, tz = 0}
        run r frames = go newShaper (zip [0, 1 / 120 ..] frames)
          where
            go _ [] = []
            go sh ((t, p) : rest) = let (out, sh') = shape r NoTrim t p sh in yaw out : go sh' rest
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
    let curvesOf = fmap ((\r -> [yawCurve r, pitchCurve r, rollCurve r, xCurve r, yCurve r, zCurve r]) . response) . parseConfig
    check "curves with precision" $
        curvesOf "yaw = 1:6, precision 40\npitch = 2:14 , precision 0\nroll = 1:4\nx = 1:3, precision 100\ny = 1:4,precision 5\n"
            == Right
                [ Expo (Multiplier 6 40 yawRange)
                , Expo (Multiplier 7 0 pitchRange)
                , Points [(1, 4)]
                , Expo (Multiplier 3 100 leanRange)
                , Expo (Multiplier 4 5 leanRange)
                , Points [(1, 2.5)]
                ]
    check "precision with several points" $
        parseConfig "yaw = 1:6 2:9, precision 40" == Left "line 1: a centre precision goes with one input:output point, both above 0, like 1:6, precision 40"
    check "precision with no multiplier" $
        parseConfig "yaw = 1:0, precision 40" == Left "line 1: a centre precision goes with one input:output point, both above 0, like 1:6, precision 40"
    check "precision out of range" $ parseConfig "yaw = 1:6, precision 101" == Left "line 1: precision is a number from 0 to 100"
    check "not a precision" $ parseConfig "yaw = 1:6, expo 40" == Left "line 1: expected precision and a number, not \"expo 40\""
    check "precision with no points" $ parseConfig "yaw = , precision 40" == Left "line 1: a curve needs at least one input:output point"
    check "two commas" $ parseConfig "yaw = 1:6, precision 4, 5" == Left "line 1: a curve has at most one comma, before its precision"
    let withExpos =
            defaultConfig
                { response =
                    (response defaultConfig)
                        { yawCurve = Expo (Multiplier 6 40 yawRange)
                        , zCurve = Expo (Multiplier 2.5 12.5 leanRange)
                        , rollCurve = deadzone
                        }
                }
    check "curves round-trip" $ parseConfig (renderConfig withExpos) == Right withExpos
    check "precision written" $
        editSettings [("yaw", "1:6, precision 40")] "yaw = 1:6 # mine\n" == Right "yaw = 1:6, precision 40 # mine\n"
    check "precision described" $
        (describeChanges defaultConfig <$> parseConfig "yaw = 1:6, precision 40\n") == Right "Settings reloaded: yaw = 1:6, precision 40"

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
        runLearner = runLearnerWith headset
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
    let unlearnt = relativePose cameraLevel (appliedPivot (newLearner headset defaultCalibration)) straight (headAt neckAt 30 0 0)
    check ("without learning, turning moves " ++ show unlearnt) $ norm (V3 (tx unlearnt) (ty unlearnt) (tz unlearnt)) > 20
    check ("level described " ++ describeLevel c) $ take 13 (describeLevel c) == "Level learnt:"
    check ("pivot described " ++ describePivot (Just straight) c) $ take 18 (describePivot (Just straight) c) == "Neck pivot learnt:"
    -- Nothing learnt, as after clearing the calibration, is said so.
    forM_ [Nothing, Just straight] $ \centre ->
        check ("nothing learnt described " ++ show centre) $
            [describeLevel defaultCalibration, describePivot centre defaultCalibration]
                == ["Level: not learnt yet; turn your head left and right.", "Neck pivot: not learnt yet; turn your head left and right."]
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
            check "loaded calibration applies at once" $ closeV (appliedPivot (newLearner headset loaded)) (estimatedPivot c)
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
    let viewing = plain{yawCurve = Points [(1, 6)], lean = LeanWithView}
        (shapedBack, _) = shape viewing NoTrim 0 HeadPose{yaw = 160 / 6, pitch = 0, roll = 0, tx = 10, ty = 0, tz = 0} newShaper
    check ("lean follows the curved yaw " ++ show shapedBack) $
        abs (yaw shapedBack - 160) < 1e-9 && abs (tx shapedBack - 10 * cos (deg 160)) < 1e-9 && abs (tz shapedBack + 10 * sin (deg 160)) < 1e-9
    check "lean setting" $ fmap (lean . response) (parseConfig "lean = screen\n") == Right LeanWithScreen
    check "lean default" $ lean (response defaultConfig) == LeanWithView
    check "bad lean" $ parseConfig "lean = sideways" == Left "line 1: lean is view or screen"

    -- Whose NPClient games load.
    check "trackkat's NPClient by default" $ npclient defaultConfig == TrackKatNPClient
    check "npclient setting" $ fmap npclient (parseConfig "npclient = trackir\n") == Right TrackIRNPClient
    check "bad npclient" $ parseConfig "npclient = opentrack" == Left "line 1: npclient is trackkat or trackir"
    let withTrackIR = defaultConfig{npclient = TrackIRNPClient}
    check "npclient round-trips" $ parseConfig (renderConfig withTrackIR) == Right withTrackIR
    check "npclient rendered" $ "npclient = trackkat" `elem` lines (renderConfig defaultConfig)
    check "npclient change described" $
        describeChanges defaultConfig withTrackIR == "Settings reloaded: npclient = trackir"
    check "npclient editable" $ editSettings [("npclient", "trackir")] "npclient = trackkat\n" == Right "npclient = trackir\n"

    -- Trimming: where the view looks becomes forward, until recentering.
    let trimmed t y p = fst (shape plain t 0 HeadPose{yaw = y, pitch = p, roll = 3, tx = 4, ty = 5, tz = 6} newShaper)
        axes hp = [yaw hp, pitch hp, roll hp, tx hp, ty hp, tz hp]
        lookingAt = trimmed NoTrim (-40) 20
        once = trimTo lookingAt
    check ("trim takes the view " ++ show once) $ once == Trim (-40) 20
    check "trim adds to the view" $ axes (trimmed once 5 (-5)) == [-35, 15, 3, 4, 5, 6]
    let twice = trimTo (trimmed once (-10) 10)
    check ("trims add up " ++ show twice) $ twice == Trim (-50) 30
    check "untrimmed after recentering" $ axes (trimmed NoTrim 5 (-5)) == [5, -5, 3, 4, 5, 6]
    check "trimmed yaw wraps" $ yaw (trimmed (Trim 170 0) 30 0) == -160 && yaw (trimmed (Trim (-170) 0) (-30) 0) == 160
    check "trimmed pitch stops straight up or down" $ pitch (trimmed (Trim 0 70) 0 40) == 90 && pitch (trimmed (Trim 0 (-70)) 0 (-40)) == -90
    check "trim of a view turned past behind" $ trimTo HeadPose{yaw = 200, pitch = 100, roll = 0, tx = 0, ty = 0, tz = 0} == Trim (-160) 90
    let (trimmedLean, _) = shape viewing (Trim 150 0) 0 HeadPose{yaw = 10 / 6, pitch = 0, roll = 0, tx = 10, ty = 0, tz = 0} newShaper
    check ("lean follows the trimmed yaw " ++ show trimmedLean) $
        abs (yaw trimmedLean - 160) < 1e-9 && abs (tx trimmedLean - 10 * cos (deg 160)) < 1e-9 && abs (tz trimmedLean + 10 * sin (deg 160)) < 1e-9
    check "trim unbound by default" $ trimBindings defaultConfig == []
    check "trim bindings" $
        fmap trimBindings (parseConfig "trim = Ctrl+F12, Stick button 3\n") == Right [Key [Ctrl] "F12", JoystickButton (Joystick "Stick" 1) 3]
    check "empty trim" $ parseConfig "trim =\n" == Right defaultConfig && parseConfig "trim =   # none\n" == Right defaultConfig
    check "empty trim rendered" $ "trim =" `elem` lines (renderConfig defaultConfig)
    let withTrim = defaultConfig{trimBindings = [Key [] "F11"]}
    check "trim round-trips" $ parseConfig (renderConfig withTrim) == Right withTrim
    check "trim change described" $ describeChanges defaultConfig withTrim == "Settings reloaded: trim = F11"

    -- Drift is no longer a setting, but files that set it still load,
    -- with a note, and editing leaves the line alone.
    let oldFile = "lean = view\ndrift = on\n"
    check "drift ignored" $ parseConfig oldFile == Right defaultConfig && parseConfig "drift = maybe" == Right defaultConfig
    check "drift noted" $ retiredSettings oldFile == ["line 2: drift is no longer a setting; ignoring it."]
    check "no notes" $ retiredSettings (renderConfig defaultConfig) == []
    check "drift not rendered" $ "drift" `notElem` settingNames
    check "drift left alone" $
        editSettings (settingValues oldFile ++ [("lean", "screen")]) oldFile == Right "lean = screen\ndrift = on\n"
    check "drift not editable" $ isLeft (editSettings [("drift", "off")] oldFile)

    -- Editing the settings file from trackkat's window.
    let userFile = "# mine\npause=Pause # TrackIR style\nsmoothing = off\nsteadiness = 70\npause = F9\n"
    check ("values as written: " ++ show (settingValues userFile)) $
        take 6 (settingValues userFile)
            == [("recenter", "F12"), ("pause", "F9"), ("trim", ""), ("smoothing", "off"), ("steadiness", "70"), ("responsiveness", "45")]
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
    check "trim edited" $ editSettings [("trim", "Ctrl+F12")] userFile == Right (userFile ++ "trim = Ctrl+F12\n")
    check "trim unbound again" $ editSettings [("trim", "")] "trim = F1 # mine\n" == Right "trim = # mine\n"
    -- The clip's shape.
    check "clip default" $ clipDistances (clip defaultConfig) == (70, 110, 110)
    check "clip default rendered" $ "clip = 70 110 110" `elem` lines (renderConfig defaultConfig)
    let bent = clipShape 70 112 108
    check "clip parsed, with commas or spaces" $
        all (\t -> fmap clip (parseConfig t) == maybe (Left "") Right bent) ["clip = 70 112 108\n", "clip = 70,112,108\n", "clip = 70, 112 ,108\n"]
    check "clip round-trips" $ case bent of
        Just b -> parseConfig (renderConfig defaultConfig{clip = b}) == Right defaultConfig{clip = b}
        Nothing -> False
    check "clip not a triangle" $ parseConfig "clip = 70 10 10\n" == Left "line 1: the clip's three distances don't make a triangle"
    check "clip needs three distances" $ isLeft (parseConfig "clip = 70 110\n") && isLeft (parseConfig "clip = 70 -110 -110\n")
    check "clip change described" $ (describeChanges defaultConfig <$> parseConfig "clip = 70 112 108\n") == Right "Settings reloaded: clip = 70 112 108"
    check "clip edited" $ editSettings [("clip", "70 112 108")] userFile == Right (userFile ++ "clip = 70 112 108\n")
    check "clip edit checked" $ isLeft (editSettings [("clip", "1 2 30")] userFile)
    check "binding from codes" $ bindingFromCodes 0x6 0x78 == Just (Key [Ctrl, Shift] "F9")
    check "binding from an unnamed key" $ bindingFromCodes 0 0xBA == Nothing

    check "identify" $ identify id [(297, 154), (254, 122), (249, 154)] == Just ((249, 154), (297, 154), (254, 122))

    -- The clip's sides are the wearer's; the model's are the camera's.
    case clipShape 70 100 120 of
        Just cl ->
            let m = clipModel cl
             in check "clip sides are the wearer's" $
                    abs (norm (mTop m ^-^ mLeft m) - 120) < 1e-9 && abs (norm (mTop m ^-^ mRight m) - 100) < 1e-9
        Nothing -> check "asymmetric clip" False

    -- Versions: numbers compared in order, what built them ignored.
    let version = parseVersion
        v1 = version "0.1.9"
        v2 = version "0.1.10+abc1234"
    check "version read" $ fmap versionNumbers v2 == Just [0, 1, 10]
    check "versions compare by number" $ ((<) <$> v1 <*> v2) == Just True
    check "the commit doesn't count" $ version "0.1.10" == v2
    check "version rendered" $ fmap renderVersion v2 == Just "0.1.10+abc1234"
    check "version described" $ fmap describeVersion v2 == Just "0.1.10 (abc1234)"
    check "version folder" $ fmap versionFolder v2 == Just "0.1.10"
    check "folder names have no commit" $ isNothing (folderVersion "0.1.10+abc") && folderVersion "0.1.10" == v2
    check "not versions" $ all (isNothing . version) ["", "dev", "0..1", "1.x", "0.1+", "0.1+a b", "-1.2", "1.2.3.partial"]

    -- The metadata published with each release.
    let sha = replicate 63 'a' ++ "B"
        meta = "{\"version\": \"0.1.10+abc1234\", \"url\": \"https://example.com/t\\u0072ackkat-windows.zip\",\n \"sha256\": \"" ++ sha ++ "\", \"size\": 123, \"notes\": [true, null]}"
    case parseRelease meta of
        Right r -> do
            check "release version" $ Just (releaseVersion r) == v2
            check "release url, unescaped" $ releaseUrl r == "https://example.com/trackkat-windows.zip"
            check "release digest, lower case" $ releaseSha256 r == replicate 63 'a' ++ "b"
        Left err -> check ("release metadata: " ++ err) False
    check "metadata missing a field" $ isLeft (parseRelease "{\"version\": \"0.1.2\", \"url\": \"https://x\"}")
    check "metadata with a bad digest" $ isLeft (parseRelease ("{\"version\": \"0.1.2\", \"url\": \"https://x\", \"sha256\": \"" ++ replicate 64 'g' ++ "\"}"))
    check "metadata that isn't JSON" $ all (isLeft . parseRelease) ["", "[1]", "{\"version\": }", "{\"a\": 1} x"]
    check "digests compare ignoring case" $ sameDigest "ABC" "abc" && not (sameDigest "abc" "abd")

    -- Where metadata and zips come from: HTTPS, or for testing a file.
    tmp <- getTemporaryDirectory
    check "a full path" $ parseLocation (tmp </> "x.json") == Right (LocalFile (tmp </> "x.json"))
    check "https metadata" $ parseLocation "https://github.com/x.json" == Right (Web "https://github.com/x.json")
    check "plain http refused" $ isLeft (parseLocation "http://github.com/x.json")
    check "relative path refused" $ isLeft (parseLocation "x.json")
    check "metadata from the web names a zip on the web" $
        releaseLocation (Web "https://a/x.json") "https://b/x.zip" == Right (Web "https://b/x.zip")
            && isLeft (releaseLocation (Web "https://a/x.json") "/etc/passwd")
    check "metadata from a file names a file beside it" $
        releaseLocation (LocalFile (tmp </> "x.json")) "x.zip" == Right (LocalFile (tmp </> "x.zip"))
            && isLeft (releaseLocation (LocalFile (tmp </> "x.json")) "https://b/x.zip")

    -- Installed or not, and what the window offers.
    let root = tmp </> "Programs" </> "trackkat"
        installedAt v = placement root (root </> v)
        vs = mapM version ["0.1.9", "0.1.10", "0.1.8"]
    check "an installed version's folder" $ fmap Installed v1 == Just (installedAt "0.1.9")
    check "a trailing separator" $ fmap Installed v1 == Just (placement (root ++ [pathSeparator]) (root </> "0.1.9" ++ [pathSeparator]))
    check "a download is portable" $ placement root (tmp </> "Downloads" </> "trackkat-windows") == Portable
    check "the install root itself is portable" $ placement root root == Portable
    check "an unfinished folder is portable" $ installedAt "0.1.9.partial" == Portable
    check "a development build offers nothing" $ offer Nothing Portable [] == NoOffer
    check "portable, nothing installed: install" $ (offer v1 Portable <$> Just []) == fmap OfferInstall v1
    check "portable, older installed: install" $ (offer v2 Portable <$> mapM version ["0.1.9"]) == fmap OfferInstall v2
    check "portable, as new installed: switch" $ (offer v1 Portable <$> mapM version ["0.1.9"]) == fmap OfferInstalled v1
    check "portable, newer installed: switch" $ (offer v1 Portable <$> vs) == fmap OfferInstalled v2
    check "installed, newest: nothing" $ (offer v2 (installedAt "0.1.10") <$> vs) == Just NoOffer
    check "installed, newer ready: update" $ (offer v1 (installedAt "0.1.9") <$> vs) == fmap OfferUpdate v2
    check "a build in a newer version's folder doesn't update to it" $ (offer v1 (installedAt "0.1.10") <$> vs) == Just NoOffer
    check "an old version hands over to the newest" $ (newerInstalled (installedAt "0.1.8") <$> vs) == Just v2
    check "the newest doesn't hand over" $ (newerInstalled (installedAt "0.1.10") <$> vs) == Just Nothing
    check "a portable copy doesn't hand over" $ (newerInstalled Portable <$> vs) == Just Nothing

    -- Cleaning up older versions, games' DLLs first.
    let names = ["0.1.8", "0.1.9", "0.1.10", "0.1.11", "0.1.11.partial", "notes", "0.1.7+x"]
    check "stale folders" $ staleFolders (installedAt "0.1.10") names == ["0.1.8", "0.1.9", "0.1.11.partial"]
    check "portable copies clean nothing" $ null (staleFolders Portable names)
    check "deletion order" $
        deletionOrder ["README.md", "trackkat.exe", "NPClient.dll", "libusb-1.0.dll", "NPClient64.dll"]
            == ["NPClient64.dll", "NPClient.dll", "README.md", "libusb-1.0.dll", "trackkat.exe"]
    check "every release file has a place" $ length (deletionOrder releaseFiles) == length releaseFiles

    -- Settings files left beside trackkat.exe by older versions.
    check "settings moved" $ leftover True False == Just MoveToSettings
    check "settings left when both" $ leftover True True == Just IgnoreBeside
    check "no settings beside" $ isNothing (leftover False True) && isNothing (leftover False False)

    -- Unpacking a release's zip.
    let work = tmp </> "trackkat-test-unzip"
        zipOf files = fromArchive (foldr (addEntryToArchive . (\(n, b) -> toEntry n 0 (BL.fromStrict (C.pack b)))) emptyArchive files)
        unpack name files = do
            BL.writeFile (work </> (name ++ ".zip")) (zipOf files)
            extractRelease (work </> (name ++ ".zip")) (work </> name)
    removePathForcibly work
    createDirectory work
    good <- unpack "good" [("trackkat.exe", "exe"), ("README.md", replicate 5000 'x')]
    exe <- readFile' (work </> "good" </> "trackkat.exe")
    readme <- readFile' (work </> "good" </> "README.md")
    check ("release unpacked " ++ show good) $ good == Right () && exe == "exe" && readme == replicate 5000 'x'
    for <-
        mapM
            (\(n, fs) -> (,) n <$> unpack n fs)
            [ ("escape", [("trackkat.exe", "x"), ("../evil.exe", "x")])
            , ("folders", [("trackkat.exe", "x"), ("sub/a.dll", "x")])
            , ("backslash", [("trackkat.exe", "x"), ("..\\evil.exe", "x")])
            , ("no-exe", [("README.md", "x")])
            ]
    forM_ for $ \(n, r) -> check ("zip refused: " ++ n) (isLeft r)
    escaped <- doesFileExist (tmp </> "evil.exe")
    check "nothing written outside" (not escaped)
    B.writeFile (work </> "junk.zip") (C.pack "not a zip")
    junk <- extractRelease (work </> "junk.zip") (work </> "junk")
    check "a file that isn't a zip" (isLeft junk)
    removeDirectoryRecursive work

    -- The example settings file in the repository is the one trackkat
    -- writes when there is none. TRACKKAT_ACCEPT=1 rewrites it.
    let example = "trackkat.conf.example"
    lookupEnv "TRACKKAT_ACCEPT" >>= \case
        Just _ -> writeFile example (renderConfig defaultConfig)
        Nothing -> do
            written <- try (readFile' example)
            check (example ++ " is the default settings file; TRACKKAT_ACCEPT=1 cabal test rewrites it") $
                either (\e -> const False (e :: IOException)) (== renderConfig defaultConfig) written

    -- The tray's cat, whose icons tools/make-icon.sh makes, is the
    -- program's.
    tray <- B.readFile "icons/tray.svg"
    program <- B.readFile "icons/trackkat.svg"
    let catPath svg = B.takeWhile (/= fromIntegral (fromEnum '"')) (B.drop 4 (snd (B.breakSubstring (C.pack " d=\"") svg)))
    check "the tray's cat is the program's" (not (B.null (catPath tray)) && catPath tray == catPath program)

    -- Following the clip through what the camera shows: reflectors fading
    -- at a slant, running together, breaking up, and stray reflections.
    -- Frames come at 120 a second, rendered as discs of lit pixels.
    let at120 :: Double -> [Double]
        at120 seconds = [fromIntegral i / 120 | i <- [0 .. round (seconds * 120) :: Int]]
        -- The clip as first made, worn so its top reflector shows 15 px
        -- above the lower pair, and with its post bent, 40 px above.
        straightWorn = -49
        bentWorn = -23
        allLit = (1, 1, 1)
        heights m pose = let ps = map (project defaultIntrinsics pose) [mLeft m, mRight m, mTop m] in (ps, identify id ps == Just (ps !! 0, ps !! 1, ps !! 2))
        summed rows = (lostCount rows, worstError rows)

    -- The label swap seen at pitch -8: with the camera above, the top
    -- reflector dips to the lower pair's level and labelling by height in
    -- the image swaps two reflectors, giving a wrong pose that fits just
    -- as well. Labelling by prediction keeps following.
    let nodding = [(t, headSeen straightWorn (-4) (-6 * (1 - cos (2 * pi * t / 1.5))) 0, allLit) | t <- at120 5]
        nodded = trackPath clipStraight nodding
        swapped = length [() | (_, pose, _) <- nodding, not (snd (heights clipStraight pose))]
    check ("height labelling swaps while nodding: " ++ show swapped) $ swapped > 100
    check ("nodding through the swap " ++ show (summed nodded)) $ lostCount nodded == 0 && fst (worstError nodded) < 1 && snd (worstError nodded) < 10

    -- An impossible jump is not believed: a frame of the head 40 degrees
    -- away is lost, until enough time has passed for the head to get there.
    let jumping = [(t, headSeen bentWorn (if t < 0.5 then 0 else 40) 0 0, allLit) | t <- at120 1.2]
        jumped = trackPath clipBent jumping
    check "a jump is lost at first" $ all isLost [o | (t, _, o, _) <- jumped, t >= 0.5, t < 0.6]
    check ("then believed " ++ show (summed (after 1 jumped))) $ lostCount (after 1 jumped) == 0 && fst (worstError jumped) < 1

    -- The top reflector fading from view for a moment while the head
    -- turns: the two lower ones are tracked, the turn followed.
    let fading = [(t, headSeen bentWorn (10 * sin (2 * pi * t / 3)) 0 0, (1, 1, max 0 (min 1 (abs (t - 1.7) / 0.5 - 0.4)))) | t <- at120 3]
        faded = trackPath clipBent fading
    check ("tracked through fading " ++ show (summed faded)) $ lostCount faded == 0 && fst (worstError faded) < 1
    check "two of three reflectors reported" $ any ((== 1) . seenMissing) (seens faded)

    -- Out of sight for longer, it is lost, and found again on its return.
    let hiding = [(t, headSeen bentWorn (5 * sin (2 * pi * t / 3)) 0 0, (1, 1, if t > 1 && t < 3 then 0 else 1)) | t <- at120 4]
        hidden = trackPath clipBent hiding
    check "with one reflector gone for a second, lost" $ all isLost [o | (t, _, o, _) <- hidden, t > 2.05, t < 3]
    check ("back after hiding " ++ show (summed (after 3 hidden))) $ lostCount (after 3 hidden) == 0 && fst (worstError hidden) < 1

    -- A reflection that's no reflector, while one reflector fades out.
    let reflecting = [(t, headSeen bentWorn (8 * sin (2 * pi * t / 2)) 0 0, (1, if t > 1 && t < 1.6 then 0 else 1, 1)) | t <- at120 2.5]
        stray = ((340, 288), 3)
        reflected = trackFrames clipBent [(t, pose, renderDiscs (stray : reflectorDiscs clipBent lit pose)) | (t, pose, lit) <- reflecting]
    check ("a stray reflection ignored " ++ show (summed reflected)) $ lostCount reflected == 0 && fst (worstError reflected) < 1.5
    check "the stray reported" $ all ((== 1) . seenStrays) (seens reflected)
    let alone = headSeen bentWorn 0 0 0
    check "two reflectors and a stray aren't three reflectors" $ case trackFrames clipBent [(0, alone, renderDiscs (stray : reflectorDiscs clipBent (1, 0, 1) alone))] of
        [(_, _, Lost 3 0, _)] -> True
        _ -> False

    -- Lights close to the camera laid out like the clip, as a phone's can
    -- be: no head can be there, so the clip is lost, not followed there.
    let phone pose = [(project defaultIntrinsics (Pose (poseR pose) (190 *^ normalize (poseT pose))) p, 5) | p <- [mLeft clipBent, mRight clipBent, mTop clipBent]]
        phoned = trackFrames clipBent [(t, pose, renderDiscs (if t < 1 then reflectorDiscs clipBent allLit pose else phone pose)) | t <- at120 3, let pose = headSeen bentWorn 0 0 0]
    check "lights by the camera aren't the clip" $ all isLost [o | (t, _, o, _) <- phoned, t >= 1]

    -- A reflector broken in two by a line too dim, and a fleck beside
    -- another, as at a low threshold.
    let broken = [(t, pose, (if t > 0.5 then breakAt (project defaultIntrinsics pose (mLeft clipBent)) else id) (renderDiscs (fleck pose : reflectorDiscs clipBent allLit pose))) | t <- at120 2.5, let pose = headSeen bentWorn (8 * sin (2 * pi * t / 2)) 0 0]
        fleck pose = let (u, v) = project defaultIntrinsics pose (mTop clipBent) in ((u + 4, v + 9), 2.5)
        pieces = trackFrames clipBent broken
    check ("a reflector in pieces " ++ show (summed pieces)) $ lostCount pieces == 0 && fst (worstError pieces) < 1

    -- Rolling over far enough that the top reflector is no longer highest.
    -- (The straight clip's top reflector, being lower, needs less roll.)
    let rolling = [(t, headSeen straightWorn 0 0 (60 * sin (pi * t / 4)), allLit) | t <- at120 4]
        rolled = trackPath clipStraight rolling
    check "height labelling swaps when rolled over" $ any (\(_, pose, _) -> not (snd (heights clipStraight pose))) rolling
    check ("rolling over " ++ show (summed rolled)) $
        lostCount rolled == 0 && fst (worstError (unmerged rolled)) < 1 && fst (worstError rolled) < 4

    -- Turned far, the top reflector runs into a lower one: split by shape.
    let turning = [(t, headSeen bentWorn (90 * t / 8) 0 0, allLit) | t <- at120 8]
        turnedFar = trackPath clipBent turning
    check "merged blobs reported" $ any ((> 0) . seenMerged) (seens turnedFar)
    check ("turned far, through merging " ++ show (summed turnedFar)) $ lostCount turnedFar == 0 && fst (worstError turnedFar) < 4 && snd (worstError turnedFar) < 10

    -- Lost for a second and a half while turned, and back with the top
    -- reflector faint.
    let returning = [(t, headSeen bentWorn (min 45 (30 * t)) 0 0, if t > 2 && t < 3.5 then (0, 0, 0) else (1, 1, if t < 3.5 then 1 else 0.5)) | t <- at120 4]
        returned = trackPath clipBent returning
    check ("found again when back " ++ show (summed (after 3.5 returned))) $ lostCount (after 3.5 returned) == 0 && fst (worstError returned) < 1

    -- A slow sweep through everything a head does, for both clips: the
    -- straight one worn as it was crosses the pitch where the labels swapped.
    forM_ [("bent", clipBent, bentWorn), ("straight", clipStraight, straightWorn)] $ \(name, m, wornAt) -> do
        let sweeping = [(t, headSeen wornAt (40 * sin (2 * pi * t / 7)) (25 * sin (2 * pi * t / 5.3)) (20 * sin (2 * pi * t / 4.1)), allLit) | t <- at120 20]
            swept = trackPath m sweeping
            sweptCorrections = corrections (snd (trackFramesEvents m (rendered m sweeping)))
            stepSizes = zipWith (\(_, _, _, a) (_, _, _, b) -> maybe 0 fst (poseError <$> a <*> b)) swept (drop 1 swept)
        check ("sweep with the " ++ name ++ " clip " ++ show (summed swept, maximum stepSizes, sweptCorrections)) $
            null sweptCorrections
                && lostCount swept == 0
                && fst (worstError (unmerged swept)) < 1
                && fst (worstError swept) < 4
                && snd (worstError swept) < 20
                && maximum stepSizes < 3

    -- Mirror images. A brief loss of the clip while nodding through where
    -- its triangle squarely faces the camera: found again, it is solved
    -- onto the mirror image, on which yaw and pitch are reversed, and
    -- tracked there. Glancing about shows the mistake, and it is corrected.
    let glance from t = if t < from then 0 else 25 * sin (2 * pi * (t - from) / 2.5)
        slipTurn t = (glance 3 t, max 0 (min 20 (20 * (t - 1) / 1.5)))
        slipping = [(t, uncurry (headSeen 30) (slipTurn t) 0, if t > 1.5 && t < 2.2 then (0, 0, 0) else allLit) | t <- at120 12]
        (slipTracker, slipRows) = trackNoisy clipBent 0.2 slipping
        slipped = map fst slipRows
        slipFixed = lastMirrored slipped
    check ("found again on the mirror image " ++ show (length (filter isMirrored slipped))) $ any isMirrored (after 2.2 slipped)
    check ("mirror image corrected after " ++ show (slipFixed - 3) ++ " s of glancing") $ slipFixed < 3 + 2.5
    check ("corrected once " ++ show (corrections slipRows)) $ length (corrections slipRows) == 1
    -- Corrected, what is reported is what it would have been had the clip
    -- not been lost.
    let unhidden = map fst (snd (trackNoisy clipBent 0.2 [(t, pose, allLit) | (t, pose, _) <- slipping]))
        reported rows = [(t, (yaw hp, pitch hp)) | (t, _, Tracked{outPose = hp}, _) <- after (slipFixed + 0.1) rows]
        reportDifference = maximum (0 : zipWith (\(_, (y, p)) (_, (y', p')) -> max (abs (y - y')) (abs (p - p'))) (reported slipped) (reported unhidden))
    check ("then followed " ++ show (summed (after (slipFixed + 0.1) slipped), reportDifference)) $
        lostCount (after 2.3 slipped) == 0
            && fst (worstError (after (slipFixed + 0.1) slipped)) < 1.5
            && reportDifference < 0.5
    check ("pivot learnt while tracking is plausible " ++ show (estimatedPivot (trackerCalibration slipTracker))) $
        plausibleCalibration clipBent (trackerCalibration slipTracker)

    -- Started on the mirror image: the post bent upright enough that, worn
    -- so, the top reflector is nearer the camera than the lower ones, while
    -- a cold start takes it to lean back. Glancing about doesn't bring the
    -- tracking round to the head's pose by itself.
    let uprightTurn t = (glance 0.5 t, 5 * sin (2 * pi * t / 4))
        upright = [(t, uncurry (headSeen 60) (uprightTurn t) 0, allLit) | t <- at120 10]
        (_, uprightRows) = trackNoisy clipBent 0.2 upright
        uprighted = map fst uprightRows
        uprightFixed = lastMirrored uprighted
    check "a cold start takes the mirror image" $ all isMirrored (take 60 uprighted)
    check ("started on the mirror image, corrected after " ++ show (uprightFixed - 0.5) ++ " s of glancing") $ uprightFixed < 0.5 + 2.5
    check ("and followed " ++ show (summed (after (uprightFixed + 0.1) uprighted), outputError uprightTurn (after (uprightFixed + 0.1) uprighted))) $
        lostCount uprighted == 0
            && fst (worstError (after (uprightFixed + 0.1) uprighted)) < 1.5
            && outputError uprightTurn (after (uprightFixed + 0.1) uprighted) < 3

    -- Swaying the body while glancing is no reason to switch. (Where the
    -- two poses meet, tracking can touch the mirror image for a frame or
    -- two, as it did before it could tell them apart.)
    let swayingHead = [(t, Pose r (placed ^+^ mulMV (rodrigues (V3 (deg 12) 0 0)) (V3 (30 * sin (2 * pi * t / 3.7)) 0 (20 * sin (2 * pi * t / 5.3)))), allLit) | t <- at120 20, let Pose r placed = headSeen 30 (glance 0 t) (8 * sin (2 * pi * t / 7)) 0]
        (_, swayRows) = trackNoisy clipBent 0.2 swayingHead
    check ("swaying isn't corrected " ++ show (corrections swayRows, length (filter isMirrored (map fst swayRows)))) $
        null (corrections swayRows) && length (filter isMirrored (map fst swayRows)) < 12

    -- A lit fireplace behind the user, as the camera saw one: a steady core
    -- and flickering flames around it. Finding the clip afresh needs
    -- exactly three big blobs, and the fire's core is always a fourth;
    -- learnt as a still light and masked, it no longer stops that.
    let fireAt = (156, 297)
        -- The clip high in the image, its lower pair about 60 pixels
        -- apart around (300, 100), as on the user's camera.
        raised (Pose r p) = Pose r (p ^+^ V3 (-26) (-165) 0)
        fidgeting t = raised (headSeen bentWorn (3 * sin (2 * pi * t / 3)) (2 * sin (2 * pi * t / 4.3)) 0)
        lit seen = if seen then allLit else (0, 0, 0)
        -- (time, head, reflectors lit, fire lit) to rendered frames
        withFire place rows =
            [ (t, pose, renderDiscs (heads ++ (if fire then occluded heads (fireDiscs place i) else [])))
            | (i, (t, pose, l, fire)) <- zip [0 :: Int ..] rows
            , let heads = reflectorDiscs clipBent l pose
            ]
        firstTracked from rows = case [t | (t, _, Tracked{}, _) <- rows, t >= from] of
            t : _ -> Just t
            [] -> Nothing
        ignored rows = [p | (_, evs) <- rows, LightIgnored p <- evs]
        forgotten rows = [(t, p) | ((t, _, _, _), evs) <- rows, LightForgotten p <- evs]
        moved rows = [(t, p) | ((t, _, _, _), evs) <- rows, LightMoved p <- evs]
        nearFire p = distance p fireAt < 10
        coldFrames = withFire fireAt [(t, fidgeting t, allLit, True) | t <- at120 8]
        coldWith masking = trackFramesEventsWith masking clipBent coldFrames
        (coldTracker, coldRows) = coldWith MaskLights
        cold = map fst coldRows
        coldStart = firstTracked 0 cold
        coldUnmasked = firstTracked 0 (map fst (snd (coldWith NoMasking)))
    check ("the clip where the user's camera saw it " ++ show (map (project defaultIntrinsics (fidgeting 0)) [mLeft clipBent, mRight clipBent, mTop clipBent])) True
    check ("with the fire lit, the clip found " ++ show coldStart ++ " s after a cold start, ignoring " ++ show (ignored coldRows)) $
        maybe False (< 3.5) coldStart && all nearFire (ignored coldRows) && length (ignored coldRows) == 1
    check ("then followed " ++ show (summed (after 3.5 cold))) $ lostCount (after 3.5 cold) == 0 && fst (worstError cold) < 1.5
    check ("the masked fire reported " ++ show (take 1 (seens (after 3.5 cold)))) $ all ((> 0) . seenMasked) (seens (after 3.5 cold))
    check ("without the mask, never found " ++ show coldUnmasked) $ isNothing coldUnmasked
    let roles = [(role, distance (blobX b, blobY b) fireAt < 30) | (b, role) <- trackerBlobs coldTracker]
    check ("the last frame's blobs: the reflectors, and the fire masked " ++ show roles) $
        length [() | (ReflectorBlob, _) <- roles] == 3 && and [role == MaskedBlob | (role, True) <- roles] && all (\(role, byFire) -> byFire || role == ReflectorBlob) roles

    -- Lost for a second and a half with the fire lit, and found again.
    let refinding = trackFramesEvents clipBent (withFire fireAt [(t, fidgeting t, lit (t < 5 || t > 6.5), True) | t <- at120 8])
        refound = fmap (subtract 6.5) (firstTracked 6.5 (map fst (snd refinding)))
    check ("found again with the fire lit after " ++ show refound ++ " s") $ maybe False (< 0.05) refound

    -- The head passes in front of the fire, hiding it, and over its area:
    -- the reflectors there are not masked.
    let passPose t = leaning (max 0 (min 1 (min (t - 5) (9 - t))))
        leaning f = let Pose r p = raised (headSeen bentWorn 0 0 0) in Pose r (p ^+^ V3 (-130 * f) 0 0)
        passFire = project defaultIntrinsics (leaning 1) (mLeft clipBent)
        (_, passRows) = trackFramesEvents clipBent (withFire passFire [(t, passPose t, allLit, True) | t <- at120 10.5])
        passed = map fst passRows
        passIgnored = [p | ((t, _, _, _), evs) <- passRows, t < 5, LightIgnored p <- evs]
    check ("the fire at " ++ show passFire ++ " learnt before the head passes " ++ show passIgnored) $
        length passIgnored == 1 && all (\p -> distance p passFire < 10) passIgnored
    check ("the head passing in front of the fire " ++ show (summed (after 4 passed), maximum (0 : map seenMissing (seens (after 4 passed))))) $
        lostCount (after 4 passed) == 0 && all ((== 0) . seenMissing) (seens (after 4 passed)) && fst (worstError (after 4 passed)) < 1.5
    check ("the fire not taken to have moved with the head " ++ show (moved passRows, forgotten passRows)) $ null (moved passRows) && null (forgotten passRows)

    -- The fire goes out: it is forgotten after a while.
    let at30 :: Double -> [Double]
        at30 seconds = [fromIntegral i / 30 | i <- [0 .. round (seconds * 30) :: Int]]
        (_, outRows) = trackFramesEvents clipBent (withFire fireAt [(t, fidgeting t, (0, 0, 0), t < 5) | t <- at30 60])
    check ("the fire learnt alone, then forgotten after it goes out " ++ show (ignored outRows, forgotten outRows)) $
        map nearFire (ignored outRows) == [True] && case forgotten outRows of
            [(t, p)] -> nearFire p && t > 45 && t < 55
            _ -> False
    -- Before it was learnt, the fire and its flames were at times taken
    -- for the clip, as three big blobs; masked, never.
    check ("the fire masked isn't taken for the clip " ++ show [(t, s) | ((t, _, Tracked{outSeen = s}, _), _) <- outRows, t > 3]) $ all isLost [o | ((t, _, o, _), _) <- outRows, t > 3.5]
    let (_, unmaskedRows) = trackFramesEventsWith NoMasking clipBent (withFire fireAt [(t, fidgeting t, (0, 0, 0), True) | t <- at30 10])
    check "without the mask, nothing learnt or masked" $
        null (ignored unmaskedRows) && and [m == 0 | ((_, _, Lost{outMasked = m}, _), _) <- unmaskedRows]

    -- A head held still for a minute by the fire, from a cold start: its
    -- reflectors, still with the fire for as long as it takes to learn it,
    -- are learnt with it but found in it at once, and not announced; then,
    -- being matched, they are never masked.
    let (_, stillRows) = trackFramesEvents clipBent (withFire fireAt [(t, raised (headSeen bentWorn 0 0 0), allLit, True) | t <- at30 60])
        stilled = map fst stillRows
    check ("a still head isn't masked " ++ show (summed stilled, ignored stillRows)) $
        lostCount stilled <= 93 && lostCount (after 3.5 stilled) == 0 && map nearFire (ignored stillRows) == [True]

    -- The clip's own reflectors aren't learnt as lights, however long the
    -- head is held still with some of them out of sight or with the clip
    -- tracked wrongly: looking down at a phone with only the lower pair or
    -- one reflector in view, lost and then showing one alone, held still
    -- while tracked, and held still on the mirror image. Each centroid
    -- wanders by up to half a pixel. Afterwards, glancing about with all
    -- three in view, the clip is found and followed.
    let lookingDown = raised (headSeen bentWorn 5 (-25) 0)
        glancingAt t = raised (headSeen bentWorn (glance 0 t) 0 0)
        -- (what is in view, how long it is held, the head and reflectors
        -- lit meanwhile, whether the clip was tracked before)
        holds =
            [ ("lost, then the lower pair in view", 60, \t -> (lookingDown, if t < 5 then (0, 0, 0) else (1, 1, 0)), True)
            , ("the top reflector in view", 60, \_ -> (lookingDown, (0, 0, 1)), True)
            , ("lost, then one reflector alone", 60, \t -> (lookingDown, if t < 5 then (0, 0, 0) else (1, 0, 0)), True)
            , ("all three tracked", 60, \_ -> (fidgeting 0, allLit), True)
            ]
        heldScene held holding trackedBefore =
            [ (t, pose, l)
            | t <- at30 (held + 6)
            , let (pose, l)
                    | trackedBefore && t < 2 = (fidgeting t, allLit)
                    | t < held + 2 = holding t
                    | otherwise = (glancingAt (t - held - 2), allLit)
            ]
    forM_ holds $ \(name, held, holding, trackedBefore) -> do
        let (_, heldRows) = trackNoisy clipBent 1 (heldScene held holding trackedBefore)
            back = after (held + 3) (map fst heldRows)
        check ("held still for " ++ show (round held :: Int) ++ " s with " ++ name ++ ", no light learnt " ++ show (ignored heldRows, forgotten heldRows)) $
            null (ignored heldRows)
        check ("and afterwards followed " ++ show (summed back)) $ lostCount back == 0 && fst (worstError back) < 1.5
    let (_, mirrorRows) = trackNoisy clipBent 1 [(t, headSeen 60 0 0 0, allLit) | t <- at30 60]
    check ("held still on the mirror image " ++ show (length (filter isMirrored (map fst mirrorRows))) ++ ", no light learnt " ++ show (ignored mirrorRows)) $
        null (ignored mirrorRows)

    -- A lamp the size of a reflector is learnt as quickly as a fire while
    -- the clip is tracked elsewhere with all three reflectors in view.
    let lamp = ((156, 297), 10)
        (_, lampRows) = trackFramesEvents clipBent [(t, fidgeting t, renderDiscs (lamp : reflectorDiscs clipBent allLit (fidgeting t))) | t <- at30 8]
    check ("a lamp the size of a reflector learnt beside the tracked clip " ++ show [(t, p) | ((t, _, _, _), evs) <- lampRows, LightIgnored p <- evs]) $
        case [(t, p) | ((t, _, _, _), evs) <- lampRows, LightIgnored p <- evs] of
            [(t, p)] -> t < 3.5 && nearFire p
            _ -> False

    -- From a cold start, nothing tells a reflector from a light: the lower
    -- pair held still in view is learnt. Then the top reflector shows, the
    -- clip is found in what was learnt, and as the head turns, the lights
    -- are seen to move with it and are forgotten at once.
    let coldPair = [(t, pose, l) | t <- at30 40, let (pose, l) = if t < 30 then (fidgeting 0, (1, 1, 0)) else if t < 32 then (fidgeting 0, allLit) else (glancingAt (t - 32), allLit)]
        (_, coldPairRows) = trackNoisy clipBent 1 coldPair
        coldPairTracked = map fst coldPairRows
    check ("the lower pair learnt from a cold start " ++ show (ignored coldPairRows) ++ ", forgotten as it moves " ++ show (moved coldPairRows)) $
        length (ignored coldPairRows) == 2 && length (moved coldPairRows) == 2 && all (\(t, _) -> t > 32 && t < 33) (moved coldPairRows)
    check ("the clip found in them and followed " ++ show (summed (after 30.5 coldPairTracked))) $
        lostCount (after 30.5 coldPairTracked) == 0 && fst (worstError (after 30.5 coldPairTracked)) < 1.5

    -- Tracking for hours holds on to no more than tracking for a minute:
    -- the memory in use stays flat from 2000 frames to 20000, with the
    -- head nearly still facing the camera (where its pose and the mirror
    -- image meet), turning left and right, and gone.
    statsOn <- getRTSStatsEnabled
    check "memory statistics are on, for the tests below (+RTS -T)" statsOn
    let fireLit i t pose = (t, renderDiscs (heads ++ occluded heads (fireDiscs fireAt i)))
          where
            heads = reflectorDiscs clipBent allLit pose
        stillFrame i = let t = fromIntegral i / 120 in fireLit i t (fidgeting t)
        turningFrame i = let t = fromIntegral i / 120 in fireLit i t (raised (headSeen bentWorn (25 * sin (2 * pi * t / 3)) (8 * sin (2 * pi * t / 5)) 0))
        goneFrame i =
            let t = fromIntegral i / 120
             in if t < 10 then fireLit i t (fidgeting t) else (t, renderDiscs (fireDiscs fireAt i))
    forM_ [("held nearly still", stillFrame), ("turning", turningFrame), ("gone", goneFrame)] $ \(name, frameAt) -> do
        lives <- if statsOn then liveWhileTracking frameAt [2000, 20000] else pure []
        check ("memory in use after 2000 and 20000 frames with the head " ++ name ++ ": " ++ show lives) $ case lives of
            [short, long] -> statsOn && long - short < 100000
            _ -> False
    -- Shaping while paused, its output unused, holds on to no more for
    -- 20000 frames than for 2000.
    let swaying i = let t = fromIntegral i / 120 in HeadPose{yaw = 30 * sin t, pitch = 10 * sin (t / 2), roll = 0, tx = 20 * sin (t / 3), ty = 0, tz = 0}
        shapeUnused i sh = pure (snd (shape (response defaultConfig) NoTrim (fromIntegral i / 120) (swaying i) sh))
    shapedLives <- if statsOn then liveWhileStepping shapeUnused newShaper [2000, 20000] else pure []
    check ("memory in use after shaping 2000 and 20000 frames while paused: " ++ show shapedLives) $ case shapedLives of
        [short, long] -> statsOn && long - short < 100000
        _ -> False

    -- Learning from mirror-image poses can't put the pivot in front of the
    -- clip, where it seems to be on them.
    let mirroredPoses = [(t, mp) | (t, truth, _) <- slipping, Just mp <- [mirrorPose defaultIntrinsics clipBent truth]]
        mirrorLearnt = learnerCalibration (fst (runLearnerWith clipBent mirroredPoses))
    check ("pivot not learnt from mirror images " ++ show (estimatedPivot mirrorLearnt)) $ plausibleCalibration clipBent mirrorLearnt

    -- A saved pivot in front of and above the clip was learnt from mirror
    -- images; it is forgotten on loading, and the level with it.
    let polluted = "up = 0 -0.9 0.4\nturning = 3000\npivot = 0 -60 -40\npivot windows = 120\npivot information = 500 0 0 500 0 500\n"
    check "a saved pivot in front of the clip is forgotten" $ case screenCalibration clipBent <$> parseCalibration polluted of
        Right (Left _) -> True
        _ -> False
    check "a plausible saved pivot is kept" $ case screenCalibration headset <$> parseCalibration saved of
        Right (Right kept) -> renderCalibration kept == saved
        _ -> False

    n <- readIORef failures
    if n == 0 then putStrLn "all tests passed" else exitFailure
  where
    near a b = abs (a - b) < 1e-9

-- | Discs of lit pixels, as (centre, radius), as the camera's stripes. Runs
-- that touch on a line are one stripe, so discs that touch make one blob.
renderDiscs :: [((Double, Double), Double)] -> [Stripe]
renderDiscs discs = concatMap line [0 .. frameHeight - 1]
  where
    line y = map (stripe y) (joinRuns (sortOn fst (concatMap (run y) discs)))
    run y ((u, v), r) =
        let dy = fromIntegral y - v
            halfWidth = sqrt (r * r - dy * dy)
            a = max 0 (ceiling (u - halfWidth))
            b = min (frameWidth - 1) (floor (u + halfWidth))
         in [(a, b) | r * r >= dy * dy, a <= b]
    joinRuns ((a, b) : (c, d) : rest) | c <= b + 1 = joinRuns ((a, max b d) : rest)
    joinRuns (x : rest) = x : joinRuns rest
    joinRuns [] = []
    stripe y (a, b) = let n = b - a + 1 in Stripe y a n (100 * n * (n - 1) `div` 2) (100 * n)

-- | The pixels of one image line near a point dropped, breaking a reflector
-- there in two.
breakAt :: (Double, Double) -> [Stripe] -> [Stripe]
breakAt (u, v) = filter (\s -> not (stLine s == round v && abs (fromIntegral (stStart s) - u) < 15))

-- | The headset clip as first made, and with its post bent.
clipStraight, clipBent :: Model
clipStraight = maybe (error "clipStraight") id (triangle 45 70 116 116)
clipBent = maybe (error "clipBent") id (triangle 45 70 110 110)

-- | The clip seen by a camera tipped 12 degrees down, on a head about 860 mm
-- away turned by the given yaw, pitch and roll in degrees about a neck
-- 130 mm below and 90 mm behind the clip. The clip is worn pitched by the
-- given angle in degrees: how high the top reflector shows depends on it.
headSeen :: Double -> Double -> Double -> Double -> Pose
headSeen worn y p r =
    Pose (tilt `mulMM` turned `mulMM` rotYXZ 0 worn 0) (mulMV tilt (clipAt ^+^ neck ^-^ mulMV turned neck))
  where
    deg = (* (pi / 180))
    rotYXZ a b c = rodrigues (V3 0 (deg a) 0) `mulMM` rodrigues (V3 (deg b) 0 0) `mulMM` rodrigues (V3 0 0 (deg c))
    tilt = rodrigues (V3 (deg 12) 0 0)
    turned = rotYXZ y p r
    neck = V3 0 130 90
    clipAt = V3 0 180 845

-- | The reflectors' discs in the image for a pose, each with its radius
-- scaled by how lit it is (0 for unseen). At 830 mm the lower ones light
-- about 350 pixels and the top one 265, as on the user's camera.
reflectorDiscs :: Model -> (Double, Double, Double) -> Pose -> [((Double, Double), Double)]
reflectorDiscs m (litL, litR, litT) pose =
    [ (project defaultIntrinsics pose p, lit * radiusAt1m * 1000 / z)
    | (p, radiusAt1m, lit) <- [(mLeft m, 8.8, litL), (mRight m, 8.8, litR), (mTop m, 7.6, litT)]
    , let V3 _ _ z = mulMV (poseR pose) p ^+^ poseT pose
    , lit > 0
    ]

-- | Track frames of (time, true pose, stripes) from a fresh tracker: each
-- frame's time, true pose, outcome, and the pose tracked.
trackFrames :: Model -> [(Double, Pose, [Stripe])] -> [(Double, Pose, Outcome, Maybe Pose)]
trackFrames m = map fst . snd . trackFramesEvents m

-- | Track frames as 'trackFrames' does, with each frame's events, and the
-- tracker at the end.
trackFramesEvents :: Model -> [(Double, Pose, [Stripe])] -> (Tracker, [((Double, Pose, Outcome, Maybe Pose), [Event])])
trackFramesEvents = trackFramesEventsWith (lightMasking defaultConfig)

-- | Track frames as 'trackFramesEvents' does, still lights masked or not.
trackFramesEventsWith :: Masking -> Model -> [(Double, Pose, [Stripe])] -> (Tracker, [((Double, Pose, Outcome, Maybe Pose), [Event])])
trackFramesEventsWith masking m = mapAccumL frame (newTracker defaultIntrinsics m masking defaultCalibration)
  where
    frame t (time, truth, stripes) = let (t', o, evs) = step time stripes t in (t', ((time, truth, o, trackerPose t'), evs))

-- | Track the i-th frame of (time, stripes) for i from 0, as the app does,
-- keeping only the tracker: the bytes of memory in use, after a major
-- collection, once each given number of frames has been tracked.
liveWhileTracking :: (Int -> (Double, [Stripe])) -> [Int] -> IO [Integer]
liveWhileTracking frameAt = liveWhileStepping track (newTracker defaultIntrinsics clipBent MaskLights defaultCalibration)
  where
    track i t = do
        let (time, stripes) = frameAt i
            (t', o, evs) = step time stripes t
        _ <- evaluate (length (show o) + length evs)
        pure t'

-- | Take steps 0, 1, ... from a state, keeping only the latest state,
-- evaluated as far as a strict IORef update would: the bytes of memory in
-- use, after a major collection, once each given number of steps has been
-- taken.
liveWhileStepping :: (Int -> a -> IO a) -> a -> [Int] -> IO [Integer]
liveWhileStepping next = go 0
  where
    go _ _ [] = pure []
    go i a (mark : rest)
        | i == mark = do
            performMajorGC
            live <- fromIntegral . gcdetails_live_bytes . gc <$> getRTSStats
            (live :) <$> go i a rest
        | otherwise = do
            a' <- next i a
            a' `seq` go (i + 1) a' (mark : rest)

-- | A lit fire's discs in the i-th frame, around its core's place: the
-- core, lighting 600 to 870 pixels, its centroid jittering by a pixel or
-- so, and up to four flames of 6 to 100 pixels flickering within 20
-- pixels of it, each there in about half the frames.
fireDiscs :: (Double, Double) -> Int -> [((Double, Double), Double)]
fireDiscs (u, v) i =
    ((u + 2 * noise 0 - 1, v + 2 * noise 1 - 1), sqrt ((600 + 270 * noise 2) / pi))
        : [ ((u + d * cos a, v + d * sin a), sqrt ((6 + 94 * noise (k + 0.4)) / pi))
          | k <- [3 .. 6]
          , noise (k + 0.1) > 0.5
          , let a = negate pi * noise (k + 0.2)
                d = 18 + 2 * noise (k + 0.3)
          ]
  where
    noise k = let x = sin (fromIntegral i * 12.9898 + k * 78.233) * 43758.5453 in x - fromIntegral (floor x :: Int)

-- | The discs of a light not hidden by the head the given reflectors are
-- on: none within 70 pixels of one.
occluded :: [((Double, Double), Double)] -> [((Double, Double), Double)] -> [((Double, Double), Double)]
occluded heads = filter (\(p, _) -> all (\(q, _) -> distance p q > 70) heads)

distance :: (Double, Double) -> (Double, Double) -> Double
distance (a, b) (c, d) = sqrt ((a - c) ^ (2 :: Int) + (b - d) ^ (2 :: Int))

-- | Track a head along a path of (time, true pose, how lit each reflector is).
trackPath :: Model -> [(Double, Pose, (Double, Double, Double))] -> [(Double, Pose, Outcome, Maybe Pose)]
trackPath m path = trackFrames m (rendered m path)

-- | Frames of a head along a path, as 'trackPath' sees them.
rendered :: Model -> [(Double, Pose, (Double, Double, Double))] -> [(Double, Pose, [Stripe])]
rendered m path = [(t, pose, renderDiscs (reflectorDiscs m lit pose)) | (t, pose, lit) <- path]

-- | Track a head along a path as 'trackPath' does, with each reflector's
-- centre moved by up to half the given pixels, differently each frame, as
-- noise moves a blob's centroid.
trackNoisy :: Model -> Double -> [(Double, Pose, (Double, Double, Double))] -> (Tracker, [((Double, Pose, Outcome, Maybe Pose), [Event])])
trackNoisy m amp path =
    trackFramesEvents m [(t, pose, renderDiscs (zipWith (jitter i) [0 ..] (reflectorDiscs m lit pose))) | (i, (t, pose, lit)) <- zip [0 :: Int ..] path]
  where
    jitter i j ((u, v), r) = ((u + amp * noise i j, v + amp * noise i (j + 0.5)), r)
    noise i k = let x = sin (fromIntegral i * 12.9898 + k * 78.233) * 43758.5453 in x - fromIntegral (floor x :: Int) - 0.5 :: Double

-- | Times of the frames where a pose was corrected.
corrections :: [((Double, Pose, Outcome, Maybe Pose), [Event])] -> [Double]
corrections rows = [t | ((t, _, _, _), evs) <- rows, any isCorrected evs]
  where
    isCorrected Corrected = True
    isCorrected _ = False

-- | Whether the pose tracked is the mirror image of the true pose rather
-- than the true pose.
isMirrored :: (Double, Pose, Outcome, Maybe Pose) -> Bool
isMirrored (_, truth, _, tracked) = case (mirrorPose defaultIntrinsics clipBent truth, tracked) of
    (Just other, Just p) -> fst (poseError other p) < fst (poseError truth p) && fst (poseError truth p) > 3 && fst (poseError truth other) > 10
    _ -> False

-- | The time of the last frame tracked on the mirror image, or 0.
lastMirrored :: [(Double, Pose, Outcome, Maybe Pose)] -> Double
lastMirrored rows = maximum (0 : [t | r@(t, _, _, _) <- rows, isMirrored r])

-- | The worst error, in degrees, of the yaw and pitch reported, against
-- the head's true yaw and pitch at each time.
outputError :: (Double -> (Double, Double)) -> [(Double, Pose, Outcome, Maybe Pose)] -> Double
outputError turned rows = maximum (0 : [max (abs (yaw hp - y)) (abs (pitch hp - p)) | (t, _, Tracked{outPose = hp}, _) <- rows, let (y, p) = turned t])

-- | Learn from (time, pose) pairs from scratch, for a head wearing the
-- given clip: the learner, and what it reported.
runLearnerWith :: Model -> [(Double, Pose)] -> (Learner, [Refinement])
runLearnerWith m = foldl (\(l, rs) (time, pose) -> let (l', new) = learn time pose l in (l', rs ++ new)) (newLearner m defaultCalibration, [])

-- | Degrees of turning and mm of movement between two poses.
poseError :: Pose -> Pose -> (Double, Double)
poseError a b = (norm (rotationVector (poseR a `mulMM` transpose (poseR b))) * 180 / pi, norm (poseT a ^-^ poseT b))

lostCount :: [(Double, Pose, Outcome, Maybe Pose)] -> Int
lostCount rows = length [() | (_, _, Lost{}, _) <- rows]

isLost :: Outcome -> Bool
isLost Lost{} = True
isLost Tracked{} = False

-- | The worst error of the poses tracked: degrees, mm.
worstError :: [(Double, Pose, Outcome, Maybe Pose)] -> (Double, Double)
worstError rows = (maximum (0 : map fst errors), maximum (0 : map snd errors))
  where
    errors = [poseError truth p | (_, truth, Tracked{}, Just p) <- rows]

seens :: [(Double, Pose, Outcome, Maybe Pose)] -> [Seen]
seens rows = [s | (_, _, Tracked{outSeen = s}, _) <- rows]

-- | The frames where no reflectors shared a blob.
unmerged :: [(Double, Pose, Outcome, Maybe Pose)] -> [(Double, Pose, Outcome, Maybe Pose)]
unmerged rows = [r | r@(_, _, Tracked{outSeen = Seen{seenMerged = 0}}, _) <- rows]

-- | The frames from the given time on.
after :: Double -> [(Double, Pose, Outcome, Maybe Pose)] -> [(Double, Pose, Outcome, Maybe Pose)]
after t0 = filter (\(t, _, _, _) -> t >= t0)
