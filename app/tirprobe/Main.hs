{-# LANGUAGE LambdaCase #-}

-- | tirprobe: talk to a TrackIR 5 and print what it sees.
module Main (main) where

import Control.Concurrent.Async (withAsync)
import Control.Exception (catch)
import Control.Monad (forM_, unless, when)
import Data.IORef
import GHC.Clock (getMonotonicTime)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO
import Text.Printf (printf)
import Text.Read (readMaybe)

import TrackKat.Camera
import TrackKat.Frame
import TrackKat.Pose
import TrackKat.Tracker
import qualified TrackKat.USB as USB

data Options = Options
    { optSettings :: Settings
    , optRaw :: Bool
    , optList :: Bool
    , optBlobs :: Bool
    , optModel :: Model
    , optFocal :: Double
    }

usage :: String
usage =
    unlines
        [ "usage: tirprobe [--blobs] [--clip LR,LT,RT] [--focal F] [--threshold N]"
        , "                [--ir 5|6|7] [--no-status-led] [--raw]"
        , "       tirprobe --list"
        , ""
        , "Shows head pose relative to where you were when you last pressed Enter."
        , "Type c and Enter, then turn your head left and right for a few seconds,"
        , "to calibrate which way is up."
        , ""
        , "  --blobs           show the raw blob positions instead of the pose"
        , "  --clip LR,LT,RT   headset clip: reflector spacing in mm, left-right,"
        , "                    left-top, right-top, with the top reflector's post"
        , "                    leaning back (default: 70,116,116)"
        , "  --focal F         camera focal length in pixels (default 660)"
        , "  --threshold N     pixel brightness cut-off, 30..253 (default 140)"
        , "  --ir N            IR illuminator brightness, 5..7 (default 7)"
        , "  --no-status-led   leave the green lights off"
        , "  --raw             also hex-dump every USB transfer"
        , "  --list            list all USB devices and whether we can open them"
        , ""
        , "Press Ctrl-C to stop."
        ]

parseArgs :: [String] -> Options -> Either String Options
parseArgs [] o = Right o
parseArgs ("--raw" : r) o = parseArgs r o{optRaw = True}
parseArgs ("--list" : r) o = parseArgs r o{optList = True}
parseArgs ("--blobs" : r) o = parseArgs r o{optBlobs = True}
parseArgs ("--focal" : n : r) o | Just v <- readMaybe n, v > 0 = parseArgs r o{optFocal = v}
parseArgs ("--clip" : spec : r) o
    | [Just lr, Just lt, Just rt] <- map readMaybe (splitOn ',' spec)
    , Just m <- triangle 45 lr lt rt =
        parseArgs r o{optModel = m}
    | otherwise = Left "--clip wants three distances in mm, like 134,129,129, that form a triangle"
parseArgs ("--no-status-led" : r) o = parseArgs r o{optSettings = (optSettings o){statusLed = False}}
parseArgs ("--threshold" : n : r) o | Just v <- readMaybe n = parseArgs r o{optSettings = (optSettings o){threshold = v}}
parseArgs ("--ir" : n : r) o | Just v <- readMaybe n = parseArgs r o{optSettings = (optSettings o){irBrightness = v}}
parseArgs (a : _) _ = Left ("unrecognised argument: " ++ a)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    args <- getArgs
    opts <- case parseArgs args (Options defaultSettings False False False defaultClip (focal defaultIntrinsics)) of
        Right o -> pure o
        Left err -> hPutStrLn stderr (err ++ "\n\n" ++ usage) >> exitFailure
    let logger = putStrLn . ("# " ++)
    when (optList opts) $ do
        USB.withContext listAll
        exitSuccess
    (USB.withContext $ \ctx -> withCamera ctx logger (optSettings opts) (run opts))
        `catch` \e -> do
            hPutStrLn stderr (explain e)
            exitFailure

explain :: OpenError -> String
explain NoCamera = "No TrackIR 5 (USB 131D:0159) found. Is it plugged in?"
explain (WrongDriver err) =
    "Found the TrackIR, but it is not using the WinUSB driver ("
        ++ err
        ++ ").\n\
           \Use Zadig to switch it to WinUSB (see README)."
explain InUse =
    "The TrackIR is in use by another program. Quit the NaturalPoint\n\
    \TrackIR software (check the system tray) and try again."
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

-- | The headset clip this was developed with.
defaultClip :: Model
defaultClip = maybe (error "defaultClip") id (triangle 45 70 116 116)

splitOn :: Char -> String -> [String]
splitOn c str = case break (== c) str of
    (a, []) -> [a]
    (a, _ : rest) -> a : splitOn c rest

run :: Options -> Camera -> IO ()
run opts cam = do
    putStrLn "# tracking; Enter recenters, c Enter calibrates level, Ctrl-C stops"
    lastPrint <- newIORef 0
    frames <- newIORef (0 :: Int)
    tracker <- newIORef (newTracker defaultIntrinsics{focal = optFocal opts} (optModel opts))
    let console = do
            eof <- isEOF
            unless eof $ do
                line <- getLine
                case line of
                    "" -> modifyIORef' tracker (request Recenter)
                    "c" -> modifyIORef' tracker (request Calibrate)
                    _ -> putStrLn "# Enter recenters; c Enter calibrates level"
                console
    let track stripes = do
            now <- getMonotonicTime
            (outcome, events) <- atomicModifyIORef' tracker $ \t ->
                let (t', o, evs) = step now stripes t in (t', (o, evs))
            mapM_ (putStrLn . describeEvent) events
            pure outcome
    let loop lights next = do
            (_, pkts) <- next
            forM_ pkts $ \p -> do
                when (optRaw opts) $ putStrLn ("raw " ++ describeRaw p)
                case p of
                    Frame n stripes -> do
                        tracked <- if optBlobs opts then pure (Lost 0) else track stripes
                        when (not (optBlobs opts)) $ lights (if isTracked tracked then On else Off) Off
                        modifyIORef' frames (+ 1)
                        now <- getMonotonicTime
                        t0 <- readIORef lastPrint
                        when (now - t0 >= 0.1) $ do
                            count <- readIORef frames
                            writeIORef frames 0
                            writeIORef lastPrint now
                            let fps = fromIntegral count / (now - t0) :: Double
                                blobs = stripesToBlobs 15 2500 stripes
                            putStrLn $
                                if optBlobs opts
                                    then summary n fps blobs
                                    else showOutcome tracked
                    Corrupt why bytes -> putStrLn ("# corrupt packet: " ++ why ++ "\n# " ++ hex bytes)
                    _ -> pure ()
            loop lights next
    withAsync console $ \_ -> withLights cam $ \lights -> withPackets cam (loop lights)

isTracked :: Outcome -> Bool
isTracked Tracked{} = True
isTracked Lost{} = False

describeEvent :: Event -> String
describeEvent Recentered = "# recentered"
describeEvent CalibrationStarted = "# calibrating: turn your head left and right"
describeEvent CalibrationFailed = "# calibration failed: not enough turning, try again"
describeEvent (Calibrated l) =
    let (down, right) = cameraTilt l
     in printf "# calibrated: camera tipped %.1f degrees down, %.1f to its right" down right

showOutcome :: Outcome -> String
showOutcome (Lost n) = "lost: " ++ show n ++ " blob" ++ (if n == 1 then "" else "s") ++ ", need 3"
showOutcome (Tracked hp dist err) =
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

summary :: (Integral a) => a -> Double -> [Blob] -> String
summary n fps blobs =
    printf
        "frame %3d  %5.1f fps  %d blob%s "
        (fromIntegral n :: Int)
        (min fps 999)
        (length blobs)
        (if length blobs == 1 then "" else "s")
        ++ concatMap showBlob blobs
  where
    showBlob b = printf " (%6.1f, %6.1f) %4dpx" (blobX b) (blobY b) (blobPoints b)

describeRaw :: Packet -> String
describeRaw (Frame n ss) = "frame " ++ show n ++ " " ++ show ss
describeRaw (StatusReply b) = "status " ++ hex b
describeRaw (DevInfoReply b) = "devinfo " ++ hex b
describeRaw (Unknown b) = "unknown " ++ hex b
describeRaw (Corrupt why b) = "corrupt (" ++ why ++ ") " ++ hex b
