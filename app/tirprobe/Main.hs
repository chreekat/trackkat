{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

-- | tirprobe: talk to a TrackIR 5 and print what it sees.
module Main (main) where

import Control.Concurrent.Async (withAsync)
import Control.Exception (catch)
#if defined(mingw32_HOST_OS)
import Control.Exception (IOException, displayException, try)
#endif
import Control.Monad (forM_, unless, when)
import Data.IORef
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import GHC.Clock (getMonotonicTime, getMonotonicTimeNSec)
#if defined(mingw32_HOST_OS)
import System.Directory (doesFileExist)
#endif
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO
import Text.Printf (printf)
import Text.Read (readMaybe)

import TrackKat.Build (buildVersion)
import TrackKat.Calibration
import TrackKat.Camera
import TrackKat.Config
import TrackKat.Frame
#if defined(mingw32_HOST_OS)
import TrackKat.Install (configFile, findPaths)
#endif
import TrackKat.Number (parseNumber, renderNumber)
import TrackKat.Pose
import TrackKat.Recording
import TrackKat.Tracker
import qualified TrackKat.USB as USB

data Options = Options
    { optSettings :: Settings
    , optRaw :: Bool
    , optList :: Bool
    , optBlobs :: Bool
    , optClip :: Maybe ClipShape
    -- ^ from @--clip@, or else trackkat.conf's
    , optFocal :: Double
    , optMasking :: Maybe Masking
    -- ^ from @--no-mask@, or else trackkat.conf's
    , optRecord :: Maybe FilePath
    , optReplay :: Maybe FilePath
    , optEveryFrame :: Bool
    }

usage :: String
usage =
    unlines
        [ "usage: tirprobe [--blobs] [--clip LR,LT,RT] [--focal F] [--threshold N]"
        , "                [--ir 5|6|7] [--no-status-led] [--no-mask] [--raw]"
        , "                [--record FILE]"
        , "       tirprobe --replay FILE [--blobs] [--every-frame] [--clip LR,LT,RT]"
        , "                [--no-mask]"
        , "       tirprobe --list"
        , ""
        , "Shows head pose relative to where you were when you last pressed Enter."
        , "Type c and Enter, then turn your head left and right for a few seconds,"
        , "to calibrate which way is up. Type m and Enter the moment something"
        , "goes wrong, to mark it in a recording. Like trackkat, tirprobe learns"
        , "which way is up and where your neck pivots, but starts afresh each"
        , "time: it doesn't read or write trackkat.calibration."
        , ""
        , "The clip's shape and whether to ignore still lights come from trackkat's"
        , "settings, %APPDATA%\\trackkat\\trackkat.conf, as trackkat uses them,"
        , "unless --clip or --no-mask says otherwise. tirprobe never writes it."
        , ""
        , "  --blobs           show the raw blob positions instead of the pose,"
        , "                    marking those masked as part of a still light"
        , "  --clip LR,LT,RT   headset clip: reflector spacing in mm, as you wear"
        , "                    it: left-right, left-top, right-top, with the top"
        , "                    reflector's post leaning back (default: clip in"
        , "                    trackkat.conf, else 70,110,110)"
        , "  --focal F         camera focal length in pixels (default 660)"
        , "  --threshold N     pixel brightness cut-off, 30..253 (default 140)"
        , "  --ir N            IR illuminator brightness, 5..7 (default 7)"
        , "  --no-status-led   leave the green lights off"
        , "  --no-mask         don't learn and ignore bright lights that stay put,"
        , "                    whatever trackkat.conf's ignore lights says"
        , "  --raw             also hex-dump every USB transfer"
        , "  --record FILE     write every frame the camera sends to FILE, with"
        , "                    the settings used and when Enter, c Enter and"
        , "                    m Enter were pressed, so the tracking can be"
        , "                    replayed exactly; about 6 MB a minute"
        , "  --replay FILE     track a recording instead of the camera, printing"
        , "                    what tirprobe printed, each line after the time"
        , "                    in seconds; --clip and --no-mask change what the"
        , "                    recording used"
        , "  --every-frame     with --replay, print every frame's line, not ten"
        , "                    a second"
        , "  --list            list all USB devices and whether we can open them"
        , ""
        , "Press Ctrl-C to stop."
        ]

parseArgs :: [String] -> Options -> Either String Options
parseArgs [] o = Right o
parseArgs ("--raw" : r) o = parseArgs r o{optRaw = True}
parseArgs ("--list" : r) o = parseArgs r o{optList = True}
parseArgs ("--blobs" : r) o = parseArgs r o{optBlobs = True}
parseArgs ("--record" : path : r) o = parseArgs r o{optRecord = Just path}
parseArgs ("--replay" : path : r) o = parseArgs r o{optReplay = Just path}
parseArgs ("--every-frame" : r) o = parseArgs r o{optEveryFrame = True}
parseArgs ("--focal" : n : r) o | Just v <- readMaybe n, v > 0 = parseArgs r o{optFocal = v}
parseArgs ("--clip" : spec : r) o
    | [Just lr, Just lt, Just rt] <- map parseNumber (splitOn ',' spec)
    , Just cl <- clipShape lr lt rt =
        parseArgs r o{optClip = Just cl}
    | otherwise = Left "--clip wants three distances in mm, like 134,129,129, that form a triangle"
parseArgs ("--no-mask" : r) o = parseArgs r o{optMasking = Just NoMasking}
parseArgs ("--no-status-led" : r) o = parseArgs r o{optSettings = (optSettings o){statusLed = False}}
parseArgs ("--threshold" : n : r) o | Just v <- readMaybe n = parseArgs r o{optSettings = (optSettings o){threshold = v}}
parseArgs ("--ir" : n : r) o | Just v <- readMaybe n = parseArgs r o{optSettings = (optSettings o){irBrightness = v}}
parseArgs (a : _) _ = Left ("unrecognised argument: " ++ a)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    args <- getArgs
    opts <- case parseArgs args (Options defaultSettings False False False Nothing (focal defaultIntrinsics) Nothing Nothing Nothing False) of
        Right o -> pure o
        Left err -> hPutStrLn stderr (err ++ "\n\n" ++ usage) >> exitFailure
    let logger = putStrLn . ("# " ++)
    when (optList opts) $ do
        USB.withContext listAll
        exitSuccess
    forM_ (optReplay opts) $ \path -> do
        replayFile opts path
        exitSuccess
    (config, source) <- userConfig
    let clipUsed = fromMaybe (clip config) (optClip opts)
        masking = fromMaybe (lightMasking config) (optMasking opts)
    logger $
        "clip "
            ++ describeClip clipUsed
            ++ case (optClip opts, source) of
                (Just _, _) -> " from --clip"
                (Nothing, FromFile path) -> " from " ++ path
                (Nothing, NoFile why) -> " (default; " ++ why ++ ")"
    case (optMasking opts, source) of
        (Nothing, FromFile path) | masking == NoMasking -> logger ("not ignoring still lights: ignore lights = off in " ++ path)
        _ -> pure ()
    let used = config{clip = clipUsed, lightMasking = masking}
    (USB.withContext $ \ctx -> withCamera ctx logger (optSettings opts) (run opts args used))
        `catch` \e -> do
            hPutStrLn stderr (explain e)
            exitFailure

explain :: OpenError -> String
explain NoCamera = "No TrackIR 5 (USB 131D:0159) found. Is it plugged in?"
explain (WrongDriver err) =
    "Found the TrackIR, but it is not using the WinUSB driver ("
        ++ err
        ++ ").\n"
        ++ "Use Zadig to switch it to WinUSB (see README)."
explain InUse =
    "The TrackIR is in use by another program. Quit the NaturalPoint\n"
        ++ "TrackIR software (check the system tray) and try again."
explain (OpenFailed e) = "USB error: " ++ show e

listAll :: USB.Context -> IO ()
listAll ctx = do
    devs <- USB.listDevices ctx
    forM_ devs $ \d -> do
        status <-
            if USB.devOpenResult d == 0
                then pure "opens OK (WinUSB or similar)"
                else ("cannot open: " ++) <$> USB.errorName (USB.devOpenResult d)
        printf
            "%04X:%04X  %s%s\n"
            (USB.devVendor d)
            (USB.devProduct d)
            status
            (if USB.devVendor d == vendorId then "   <-- NaturalPoint" else "" :: String)

-- | Where the settings came from: trackkat.conf, or the defaults, and why.
data Source = FromFile FilePath | NoFile String

-- | trackkat's settings, read from where trackkat reads them, for its clip
-- shape and still-light masking; the defaults if there are none, or they
-- don't parse. trackkat.conf is only read, never created.
userConfig :: IO (Config, Source)
#if defined(mingw32_HOST_OS)
userConfig =
    try (configFile <$> findPaths) >>= \case
        Left e -> pure (defaultConfig, NoFile ("can't find trackkat's settings folder: " ++ displayException (e :: IOException)))
        Right path -> do
            exists <- doesFileExist path
            if not exists
                then pure (defaultConfig, NoFile ("no " ++ path))
                else
                    readSettings path >>= \case
                        Right (_, c) -> pure (c, FromFile path)
                        Left err -> pure (defaultConfig, NoFile (path ++ ": " ++ err))
#else
userConfig = pure (defaultConfig, NoFile "trackkat.conf is read only on Windows")
#endif

-- | A clip's distances as --clip takes them.
describeClip :: ClipShape -> String
describeClip cl = let (lr, lt, rt) = clipDistances cl in intercalate "," (map renderNumber [lr, lt, rt])

splitOn :: Char -> String -> [String]
splitOn c str = case break (== c) str of
    (a, []) -> [a]
    (a, _ : rest) -> a : splitOn c rest

-- | Track with the given settings' clip shape and still-light masking,
-- recording if asked to.
run :: Options -> [String] -> Config -> Camera -> IO ()
run opts args config cam = do
    clock <- getMonotonicTimeNSec
    let start =
            Start
                { startNotes =
                    [ "tirprobe " ++ fromMaybe "development build" buildVersion ++ concatMap (' ' :) args
                    , "camera threshold " ++ show (threshold (optSettings opts)) ++ ", IR " ++ show (irBrightness (optSettings opts))
                    ]
                , startIntrinsics = defaultIntrinsics{focal = optFocal opts}
                , startSettings = settingValues (renderConfig config)
                , startCalibration = FreshCalibration
                , startClock = clock
                }
    case (optRecord opts, startTracker start) of
        (_, Left err) -> hPutStrLn stderr ("can't track with these settings: " ++ err) >> exitFailure
        (Nothing, Right tracker) -> track opts cam notRecording tracker
        (Just path, Right tracker) -> do
            putStrLn ("# recording to " ++ path ++ "; m Enter marks when something goes wrong")
            withRecorder path start (\rec -> track opts cam rec tracker)

-- | Track the camera's frames from the given tracker, recording them.
track :: Options -> Camera -> Recorder -> Tracker -> IO ()
track opts cam rec tracker0 = do
    putStrLn "# tracking; Enter recenters, c Enter calibrates level, m Enter marks, Ctrl-C stops"
    lastPrint <- newIORef 0
    frames <- newIORef (0 :: Int)
    tracker <- newIORef tracker0
    -- What the user did since the last frame, newest first.
    inputs <- newIORef []
    marks <- newIORef (0 :: Int)
    let input i = do
            now <- getMonotonicTimeNSec
            atomicModifyIORef' inputs (\is -> ((now, i) : is, ()))
        console = do
            eof <- isEOF
            unless eof $ do
                line <- getLine
                case line of
                    "" -> input (Press Recenter)
                    "c" -> input (Press Calibrate)
                    "m" -> do
                        n <- atomicModifyIORef' marks (\m -> (m + 1, m + 1))
                        putStrLn ("# mark " ++ show n)
                        input (Mark n)
                    _ -> putStrLn "# Enter recenters; c Enter calibrates level; m Enter marks"
                console
    let trackFrame n stripes = do
            now <- getMonotonicTimeNSec
            pending <- atomicModifyIORef' inputs (\is -> ([], reverse is))
            (t', outcome, events) <- trackLive rec pending now n stripes =<< readIORef tracker
            writeIORef tracker $! t'
            mapM_ (putStrLn . describeEvent (trackerCentre t')) events
            pure (outcome, trackerBlobs t')
    let loop lights next = do
            (_, pkts) <- next
            forM_ pkts $ \p -> do
                when (optRaw opts) $ putStrLn ("raw " ++ describeRaw p)
                case p of
                    Frame n stripes -> do
                        -- Tracking in blobs mode too, to learn still lights.
                        (tracked, blobs) <- trackFrame n stripes
                        when (not (optBlobs opts)) $ lights (if isTracked tracked then On else Off) Off
                        modifyIORef' frames (+ 1)
                        now <- getMonotonicTime
                        t0 <- readIORef lastPrint
                        when (now - t0 >= 0.1) $ do
                            count <- readIORef frames
                            writeIORef frames 0
                            writeIORef lastPrint now
                            let fps = fromIntegral count / (now - t0) :: Double
                            putStrLn $
                                if optBlobs opts
                                    then summary n fps blobs
                                    else showOutcome tracked
                    Corrupt why bytes -> do
                        now <- getMonotonicTimeNSec
                        record rec (CorruptEntry now why)
                        putStrLn ("# corrupt packet: " ++ why ++ "\n# " ++ hex bytes)
                    _ -> pure ()
            loop lights next
    withAsync console $ \_ -> withLights cam $ \lights -> withPackets cam (loop lights)

-- | Track a recording, printing what tracking it live printed, each line
-- after the time in seconds since the recording started.
replayFile :: Options -> FilePath -> IO ()
replayFile opts path = do
    let orFail = either (\err -> hPutStrLn stderr (path ++ ": " ++ err) >> exitFailure) pure
    recording <- orFail =<< readRecording path
    let recorded = recStart recording
    config <- orFail (parseConfig (unlines [n ++ " = " ++ v | (n, v) <- startSettings recorded]))
    let used = config{clip = fromMaybe (clip config) (optClip opts), lightMasking = fromMaybe (lightMasking config) (optMasking opts)}
        start = recorded{startSettings = settingValues (renderConfig used)}
    mapM_ (putStrLn . ("# " ++)) (startNotes recorded)
    putStrLn $
        "# clip "
            ++ describeClip (clip used)
            ++ maybe " as recorded" (const " from --clip") (optClip opts)
            ++ case lightMasking used of
                MaskLights -> ", ignoring still lights"
                NoMasking -> ", not ignoring still lights"
    replayed <- orFail (replay recording{recStart = start})
    let at t line = printf "%10.3f  %s" (secondsSince start t) line :: String
        -- As live: a frame's line ten times a second, counting the frames
        -- since the last.
        go _ [] = pure ()
        go printed@(lastPrint, frames) (Replayed e result : rest) = case (e, result) of
            (FrameEntry t n _, Just (tracker, outcome, events)) -> do
                mapM_ (putStrLn . at t . describeEvent (trackerCentre tracker)) events
                let now = secondsSince start t
                    due = optEveryFrame opts || now - lastPrint >= 0.1
                    fps = fromIntegral (frames + 1) / (now - lastPrint) :: Double
                when due . putStrLn . at t $
                    if optBlobs opts
                        then summary n fps (trackerBlobs tracker)
                        else showOutcome outcome
                go (if due then (now, 0) else (lastPrint, frames + 1)) rest
            (FrameEntry{}, Nothing) -> go printed rest
            (InputEntry t (Press Recenter), _) -> putStrLn (at t "# Enter pressed") >> go printed rest
            (InputEntry t (Press Calibrate), _) -> putStrLn (at t "# c Enter pressed") >> go printed rest
            (InputEntry t (Mark m), _) -> putStrLn (at t ("# mark " ++ show m)) >> go printed rest
            (CorruptEntry t why, _) -> putStrLn (at t ("# corrupt packet: " ++ why)) >> go printed rest
            (Unreadable n why, _) -> hPutStrLn stderr (path ++ ": line " ++ show n ++ ": " ++ why ++ "; stopping there")
    go (-1 / 0, 0 :: Int) replayed

isTracked :: Outcome -> Bool
isTracked Tracked{} = True
isTracked Lost{} = False

-- | Describe an event, given the clip's pose at the centre.
describeEvent :: Maybe Pose -> Event -> String
describeEvent _ Recentered = "# recentered"
describeEvent _ CalibrationStarted = "# calibrating: turn your head left and right"
describeEvent _ CalibrationFailed = "# calibration failed: not enough turning, try again"
describeEvent _ (Calibrated c) =
    let (down, right) = cameraTilt (estimatedLevel c)
     in printf "# calibrated: camera tipped %.1f degrees down, %.1f to its right" down right
describeEvent _ (Refined LevelRefined c) = "# " ++ describeLevel c
describeEvent centre (Refined PivotRefined c) = "# " ++ describePivot centre c
describeEvent _ Corrected = "# corrected a mirror-image pose"
describeEvent _ (LightIgnored p) = "# " ++ describeLightIgnored p
describeEvent _ (LightForgotten p) = "# " ++ describeLightForgotten p
describeEvent _ (LightMoved p) = "# " ++ describeLightMoved p

showOutcome :: Outcome -> String
showOutcome (Lost n masked) =
    "lost: " ++ show n ++ " blob" ++ (if n == 1 then "" else "s") ++ ", need 3" ++ (if masked > 0 then ", " ++ describeMasked masked else "")
showOutcome (Tracked hp dist err seen) =
    printf
        "yaw %+6.1f  pitch %+6.1f  roll %+6.1f   x %+5.0f  y %+5.0f  z %+5.0f mm   distance %4.0f mm  fit %.2f px"
        (yaw hp)
        (pitch hp)
        (roll hp)
        (tx hp)
        (ty hp)
        (tz hp)
        dist
        err
        ++ describeSeen seen

-- | A frame's blobs, those masked as part of a still light marked.
summary :: (Integral a) => a -> Double -> [(Blob, BlobRole)] -> String
summary n fps blobs =
    printf
        "frame %3d  %5.1f fps  %d blob%s "
        (fromIntegral n :: Int)
        (min fps 999)
        (length blobs)
        (if length blobs == 1 then "" else "s")
        ++ concatMap showBlob blobs
  where
    -- Elongation, length to width: about 1 for one reflector, more for two
    -- run together.
    showBlob (b, role) = printf " (%6.1f, %6.1f) %4dpx %3.1f:1%s" (blobX b) (blobY b) (blobPoints b) (blobElongation b) (if role == MaskedBlob then " masked" else "")

describeRaw :: Packet -> String
describeRaw (Frame n ss) = "frame " ++ show n ++ " " ++ show ss
describeRaw (StatusReply b) = "status " ++ hex b
describeRaw (DevInfoReply b) = "devinfo " ++ hex b
describeRaw (Unknown b) = "unknown " ++ hex b
describeRaw (Corrupt why b) = "corrupt (" ++ why ++ ") " ++ hex b
