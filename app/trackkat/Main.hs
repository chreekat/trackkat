{-# LANGUAGE LambdaCase #-}

-- | trackkat: track the head and serve the pose to games.
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (ExceptionInLinkedThread (..), withAsync)
import Control.Exception (Handler (..), catches, displayException)
import Control.Monad (forM_, forever, unless, when)
import Data.IORef
import GHC.Clock (getMonotonicTime)
import System.Directory (doesFileExist)
import System.Environment (getExecutablePath)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, (</>))
import System.IO
import Text.Printf (printf)

import TrackKat.Camera
import TrackKat.Config
import TrackKat.Frame (Packet (..))
import TrackKat.GamePose
import TrackKat.Pose
import TrackKat.Tracker
import qualified TrackKat.USB as USB
import TrackKat.Windows

-- | The headset clip this was developed with.
defaultClip :: Model
defaultClip = maybe (error "defaultClip") id (triangle 45 70 116 116)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    dir <- takeDirectory <$> getExecutablePath
    first <- singleInstance
    unless first $ do
        putStrLn "trackkat is already running."
        exitFailure
    config <- loadConfig (dir </> "trackkat.conf")
    openShared >>= either (\e -> putStrLn ("Can't create shared memory for games (error " ++ show e ++ ").") >> exitFailure) pure
    publish still
    setNPClientPath dir >>= \case
        Right () -> putStrLn ("Games will load NPClient from " ++ dir)
        Left e -> putStrLn ("Can't point games at NPClient (registry error " ++ show e ++ ").")
    startHotkeys (recenterKey config) (pauseKey config) >>= \case
        Right () -> pure ()
        Left e -> putStrLn ("Can't watch the keyboard for hotkeys (error " ++ show e ++ ").")
    printf
        "Recenter: %s   Pause: %s   (Enter also recenters, c Enter calibrates level)\n"
        (renderBinding (recenterKey config))
        (renderBinding (pauseKey config))
    session <- newSession
    withAsync (console session) $ \_ -> forever $ do
        (USB.withContext $ \ctx -> withCamera ctx (putStrLn . ("# " ++)) defaultSettings (track session))
            `catches` [ Handler $ \e -> putStrLn (explain e)
                      , Handler $ \e -> putStrLn ("USB error: " ++ displayException (e :: USB.USBError))
                      , Handler $ \(ExceptionInLinkedThread _ e) -> putStrLn ("Camera stopped: " ++ displayException e)
                      , Handler $ \CameraSilent -> putStrLn "The camera went quiet, perhaps after sleep."
                      ]
        putStrLn "Trying the camera again in 3 seconds."
        threadDelay 3000000

-- | Read the settings file, creating it with the defaults if there is none.
loadConfig :: FilePath -> IO Config
loadConfig path = do
    exists <- doesFileExist path
    if not exists
        then do
            writeFile path (renderConfig defaultConfig)
            putStrLn ("Wrote default settings to " ++ path)
            pure defaultConfig
        else
            parseConfig <$> readFile path >>= \case
                Right c -> pure c
                Left err -> do
                    putStrLn (path ++ ": " ++ err)
                    exitFailure

explain :: OpenError -> String
explain NoCamera = "No TrackIR 5 found. Is it plugged in?"
explain (WrongDriver err) = "The TrackIR isn't using the WinUSB driver (" ++ err ++ "). See the README."
explain InUse = "The TrackIR is in use by another program, perhaps NaturalPoint's software."
explain (OpenFailed e) = "USB error: " ++ show e

-- | State that outlives any one connection to the camera.
data Session = Session
    { sTracker :: IORef Tracker
    , sPaused :: IORef (Maybe GamePose)
    -- ^ the pose games see while paused
    , sLatest :: IORef GamePose
    -- ^ the last pose sent
    }

newSession :: IO Session
newSession =
    Session
        <$> newIORef (newTracker defaultIntrinsics defaultClip)
        <*> newIORef Nothing
        <*> newIORef still

console :: Session -> IO ()
console s = do
    eof <- isEOF
    unless eof $ do
        line <- getLine
        case line of
            "" -> modifyIORef' (sTracker s) (request Recenter)
            "c" -> modifyIORef' (sTracker s) (request Calibrate)
            _ -> putStrLn "Enter recenters; c Enter calibrates level"
        console s

-- | Track until the camera goes away.
track :: Session -> Camera -> IO ()
track s cam = do
    putStrLn "Tracking."
    lights <- newLightKeeper cam
    seenRecenters <- newIORef =<< recenterCount
    seenReads <- newIORef =<< gameReads
    lastRead <- newIORef 0
    lastStatus <- newIORef 0
    withPackets cam $ \next -> forever $ do
        packets <- next
        forM_ packets $ \case
            Frame _ stripes -> do
                now <- getMonotonicTime
                handleControls s seenRecenters
                (outcome, events) <- atomicModifyIORef' (sTracker s) $ \t ->
                    let (t', o, evs) = step now stripes t in (t', (o, evs))
                mapM_ (putStrLn . describeEvent) events
                case outcome of
                    Tracked hp _ _ -> writeIORef (sLatest s) (toGamePose hp)
                    Lost _ -> pure ()
                paused <- readIORef (sPaused s)
                publish =<< maybe (readIORef (sLatest s)) pure paused

                readsNow <- gameReads
                before <- atomicModifyIORef' seenReads (\b -> (readsNow, b))
                when (readsNow /= before) $ writeIORef lastRead now
                lastGame <- readIORef lastRead
                let gameConnected = now - lastGame < 1
                lights (case outcome of Tracked{} -> On; Lost _ -> Off) (if gameConnected then On else Off)

                shown <- readIORef lastStatus
                when (now - shown >= 1) $ do
                    writeIORef lastStatus now
                    putStrLn (status outcome paused gameConnected)
            Corrupt why _ -> putStrLn ("# dropped a corrupt packet: " ++ why)
            _ -> pure ()

-- | Act on hotkeys and on recenter requests from games.
handleControls :: Session -> IORef Int -> IO ()
handleControls s seenRecenters = do
    keys <- takeHotkeys
    fromGames <- recenterCount
    before <- atomicModifyIORef' seenRecenters (\b -> (fromGames, b))
    when (RecenterKey `elem` keys || fromGames /= before) $
        modifyIORef' (sTracker s) (request Recenter)
    when (PauseKey `elem` keys) $ do
        paused <- readIORef (sPaused s)
        case paused of
            Just _ -> writeIORef (sPaused s) Nothing >> putStrLn "Resumed."
            Nothing -> do
                writeIORef (sPaused s) . Just =<< readIORef (sLatest s)
                putStrLn "Paused: games see your head held still."

describeEvent :: Event -> String
describeEvent Recentered = "Recentered."
describeEvent CalibrationStarted = "Calibrating: turn your head left and right."
describeEvent CalibrationFailed = "Calibration failed: not enough turning, try again."
describeEvent (Calibrated l) =
    let (down, right) = cameraTilt l
     in printf "Calibrated: camera tipped %.1f degrees down, %.1f to its right." down right

status :: Outcome -> Maybe GamePose -> Bool -> String
status outcome paused gameConnected =
    concat
        [ case outcome of
            Tracked hp _ _ -> printf "yaw %+6.1f  pitch %+6.1f  roll %+6.1f   x %+4.0f  y %+4.0f  z %+4.0f mm" (yaw hp) (pitch hp) (roll hp) (tx hp) (ty hp) (tz hp)
            Lost n -> "lost the clip (" ++ show n ++ " of 3 reflectors)"
        , maybe "" (const "   PAUSED") paused
        , if gameConnected then "   game connected" else ""
        ]
