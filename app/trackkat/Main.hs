{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- | trackkat: track the head and serve the pose to games.
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (ExceptionInLinkedThread (..), withAsync)
import Control.Exception (Handler (..), IOException, catch, catches, displayException)
import Control.Monad (forM_, forever, unless, when)
import Data.IORef
import Data.Time.Clock (UTCTime)
import GHC.Clock (getMonotonicTime)
import GHC.Stats (GCDetails (..), RTSStats (..), getRTSStats, getRTSStatsEnabled)
import System.Directory (doesFileExist, getModificationTime)
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
import TrackKat.Response
import TrackKat.Stats
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
    let settingsFile = dir </> "trackkat.conf"
    (config, stamp) <- loadConfig settingsFile
    openShared >>= either (\e -> putStrLn ("Can't create shared memory for games (error " ++ show e ++ ").") >> exitFailure) pure
    publish still
    setNPClientPath dir >>= \case
        Right () -> putStrLn ("Games will load NPClient from " ++ dir)
        Left e -> putStrLn ("Can't point games at NPClient (registry error " ++ show e ++ ").")
    unwatched <- startHotkeys (recenterBindings config) (pauseBindings config)
    forM_ unwatched $ \case
        (Keyboard, e) -> putStrLn ("Can't watch the keyboard for hotkeys (error " ++ show e ++ ").")
        (Joysticks, e) -> putStrLn ("Can't watch joysticks for buttons (error " ++ show e ++ ").")
    printf
        "Recenter: %s   Pause: %s   (Enter also recenters, c Enter calibrates level)\n"
        (renderBindings (recenterBindings config))
        (renderBindings (pauseBindings config))
    session <- newSession (response config)
    showButtons session
    withAsync (console session) $ \_ -> withAsync (watchSettings settingsFile session stamp config) $ \_ -> withAsync (joysticks session) $ \_ -> forever $ do
        (USB.withContext $ \ctx -> withCamera ctx (putStrLn . ("# " ++)) defaultSettings (track session))
            `catches` [ Handler $ \e -> putStrLn (explain e)
                      , Handler $ \e -> putStrLn ("USB error: " ++ displayException (e :: USB.USBError))
                      , Handler $ \(ExceptionInLinkedThread _ e) -> putStrLn ("Camera stopped: " ++ displayException e)
                      , Handler $ \CameraSilent -> putStrLn "The camera went quiet, perhaps after sleep."
                      ]
        putStrLn "Trying the camera again in 3 seconds."
        threadDelay 3000000

-- | Read the settings file, creating it with the defaults if there is none,
-- along with the modification time of what was read.
loadConfig :: FilePath -> IO (Config, Maybe UTCTime)
loadConfig path = do
    exists <- doesFileExist path
    if not exists
        then do
            writeFile path (renderConfig defaultConfig)
            putStrLn ("Wrote default settings to " ++ path)
            (defaultConfig,) <$> modificationTime path
        else do
            stamp <- modificationTime path
            readSettings path >>= \case
                Right c -> pure (c, stamp)
                Left err -> do
                    putStrLn (path ++ ": " ++ err)
                    exitFailure

-- | Parse the settings file, reading it whole so editors can replace it.
readSettings :: FilePath -> IO (Either String Config)
readSettings path = (parseConfig <$> readFile' path) `catch` \e -> pure (Left (displayException (e :: IOException)))

-- | Nothing while the file is missing.
modificationTime :: FilePath -> IO (Maybe UTCTime)
modificationTime path = (Just <$> getModificationTime path) `catch` \e -> const (pure Nothing) (e :: IOException)

-- | Apply the settings file each time it is saved. A file that is missing
-- or doesn't parse leaves the settings as they were; an editor still
-- writing it changes its time again when done, so it is read again then.
watchSettings :: FilePath -> Session -> Maybe UTCTime -> Config -> IO ()
watchSettings path s = go
  where
    go stamp config = do
        threadDelay 500000
        stamp' <- modificationTime path
        case stamp' of
            Just _
                | stamp' /= stamp ->
                    readSettings path >>= \case
                        Left err -> do
                            putStrLn (path ++ ": " ++ err ++ "; keeping the previous settings.")
                            go stamp' config
                        Right new -> do
                            writeIORef (sResponse s) (response new)
                            rebindHotkeys (recenterBindings new) (pauseBindings new)
                            putStrLn (describeChanges config new)
                            go stamp' new
            _ -> go stamp config

explain :: OpenError -> String
explain NoCamera = "No TrackIR 5 found. Is it plugged in?"
explain (WrongDriver err) = "The TrackIR isn't using the WinUSB driver (" ++ err ++ "). See the README."
explain InUse = "The TrackIR is in use by another program, perhaps NaturalPoint's software."
explain (OpenFailed e) = "USB error: " ++ show e

-- | Whether the last frame showed the clip.
data Clip = ClipSeen | ClipLost

-- | State that outlives any one connection to the camera.
data Session = Session
    { sTracker :: IORef Tracker
    , sResponse :: IORef Response
    -- ^ replaced when the settings file is saved
    , sShaper :: IORef Shaper
    , sClip :: IORef Clip
    , sPaused :: IORef (Maybe GamePose)
    -- ^ the pose games see while paused
    , sLatest :: IORef GamePose
    -- ^ the last pose sent
    , sButtonsShown :: IORef Double
    -- ^ until when joystick button presses are printed, in monotonic seconds
    }

newSession :: Response -> IO Session
newSession r =
    Session
        <$> newIORef (newTracker defaultIntrinsics defaultClip)
        <*> newIORef r
        <*> newIORef newShaper
        <*> newIORef ClipLost
        <*> newIORef Nothing
        <*> newIORef still
        <*> newIORef 0

-- | Recenter on the next frame that shows the clip. If the clip is out of
-- sight, games see the view centred straight away.
recenter :: Session -> IO ()
recenter s = do
    modifyIORef' (sTracker s) (request Recenter)
    readIORef (sClip s) >>= \case
        ClipSeen -> pure ()
        ClipLost -> do
            writeIORef (sShaper s) newShaper
            writeIORef (sLatest s) still

console :: Session -> IO ()
console s = do
    eof <- isEOF
    unless eof $ do
        line <- getLine
        case line of
            "" -> recenter s
            "c" -> modifyIORef' (sTracker s) (request Calibrate)
            "j" -> showButtons s
            _ -> putStrLn "Enter recenters; c Enter calibrates level; j Enter shows joystick buttons"
        console s

-- | Print joystick button presses for the next minute, so they can be
-- copied into the settings file.
showButtons :: Session -> IO ()
showButtons s = do
    now <- getMonotonicTime
    writeIORef (sButtonsShown s) (now + 60)
    putStrLn "For the next minute, joystick buttons you press are named here, ready to bind in trackkat.conf."

-- | Report joysticks coming and going, and the buttons pressed while
-- 'sButtonsShown' says to.
joysticks :: Session -> IO ()
joysticks s = forever $ do
    threadDelay 100000
    events <- takeJoystickEvents
    now <- getMonotonicTime
    shownUntil <- readIORef (sButtonsShown s)
    forM_ events $ \case
        Connected j -> putStrLn ("Joystick connected: " ++ renderJoystick j)
        Disconnected j -> putStrLn ("Joystick disconnected: " ++ renderJoystick j)
        Pressed j n -> when (now < shownUntil) $ putStrLn ("Joystick button pressed: " ++ renderBinding (JoystickButton j n))

-- | Track until the camera goes away.
track :: Session -> Camera -> IO ()
track s cam = do
    putStrLn "Tracking."
    seenRecenters <- newIORef =<< recenterCount
    games <- newIORef (0, 0)
    lastStatus <- newIORef 0
    lastArrival <- newIORef Nothing
    window <- newIORef emptyWindow
    gcsSeen <- newIORef =<< gcCount
    withLights cam $ \lights -> withPackets cam $ \next -> forever $ do
        (arrived, packets) <- next
        forM_ packets $ \case
            Frame _ stripes -> do
                now <- getMonotonicTime
                handleControls s seenRecenters
                (outcome, events) <- atomicModifyIORef' (sTracker s) $ \t ->
                    let (t', o, evs) = step now stripes t in (t', (o, evs))
                mapM_ (putStrLn . describeEvent) events
                -- A new centre or level is a jump the shaper must not smooth.
                when (any resetsShaping events) $ writeIORef (sShaper s) newShaper
                case outcome of
                    Tracked hp _ _ -> do
                        writeIORef (sClip s) ClipSeen
                        r <- readIORef (sResponse s)
                        shaped <- atomicModifyIORef' (sShaper s) $ \sh ->
                            let (p, sh') = shape r now hp sh in (sh', p)
                        writeIORef (sLatest s) (toGamePose shaped)
                    Lost _ -> writeIORef (sClip s) ClipLost
                paused <- readIORef (sPaused s)
                publish =<< maybe (readIORef (sLatest s)) pure paused
                published <- getMonotonicTime
                previous <- atomicModifyIORef' lastArrival (\p -> (Just arrived, p))
                modifyIORef' window (sawFrame (published - arrived) (maybe 0 (arrived -) previous) (length stripes))
                noteGCs gcsSeen window

                -- Checking game processes is a few system calls; twice a
                -- second is plenty.
                (checked, _) <- readIORef games
                when (now - checked >= 0.5) $ writeIORef games . (now,) =<< gamesTracking
                gameConnected <- (> 0) . snd <$> readIORef games
                lights (case outcome of Tracked{} -> On; Lost _ -> Off) (if gameConnected then On else Off)

                shown <- readIORef lastStatus
                when (now - shown >= 1) $ do
                    writeIORef lastStatus now
                    w <- atomicModifyIORef' window (\w -> (emptyWindow, w))
                    putStrLn (status outcome paused gameConnected ++ "   " ++ summary w)
            Corrupt why _ -> do
                putStrLn ("# dropped a corrupt packet: " ++ why)
                modifyIORef' window sawCorrupt
            _ -> pure ()

-- | Garbage collections so far, if the runtime is counting (+RTS -T).
gcCount :: IO Int
gcCount = do
    enabled <- getRTSStatsEnabled
    if enabled then fromIntegral . gcs <$> getRTSStats else pure 0

-- | Count collections since the last call, with the latest one's pause.
-- Sampled every frame, so a pause hidden behind a later one in the same
-- frame is missed.
noteGCs :: IORef Int -> IORef Window -> IO ()
noteGCs seen window = do
    enabled <- getRTSStatsEnabled
    when enabled $ do
        stats <- getRTSStats
        let n = fromIntegral (gcs stats)
        before <- atomicModifyIORef' seen (\b -> (n, b))
        when (n /= before) $
            modifyIORef' window (sawGCs (n - before) (fromIntegral (gcdetails_elapsed_ns (gc stats)) / 1e9))

-- | Act on hotkeys and on recenter requests from games.
handleControls :: Session -> IORef Int -> IO ()
handleControls s seenRecenters = do
    keys <- takeHotkeys
    fromGames <- recenterCount
    before <- atomicModifyIORef' seenRecenters (\b -> (fromGames, b))
    when (RecenterKey `elem` keys || fromGames /= before) $ recenter s
    when (PauseKey `elem` keys) $ do
        paused <- readIORef (sPaused s)
        case paused of
            Just _ -> writeIORef (sPaused s) Nothing >> putStrLn "Resumed."
            Nothing -> do
                writeIORef (sPaused s) . Just =<< readIORef (sLatest s)
                putStrLn "Paused: games see your head held still."

resetsShaping :: Event -> Bool
resetsShaping Recentered = True
resetsShaping (Calibrated _) = True
resetsShaping _ = False

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
