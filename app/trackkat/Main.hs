{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}

-- | trackkat: track the head and serve the pose to games.
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (ExceptionInLinkedThread (..), cancel, link, waitCatch, withAsync, withAsyncBound)
import Control.Concurrent.MVar
import Control.Exception (Handler (..), IOException, bracket_, catch, catches, displayException, try)
import Control.Monad (forM_, forever, unless, when)
import Data.Functor (($>))
import Data.IORef
import Data.Time.Clock (UTCTime)
import GHC.Clock (getMonotonicTime)
import GHC.Stats (GCDetails (..), RTSStats (..), getRTSStats, getRTSStatsEnabled)
import System.Directory (doesFileExist, getModificationTime, renameFile)
import System.Environment (getArgs, getExecutablePath)
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, (</>))
import System.IO
import System.Timeout (timeout)
import Text.Printf (printf)

import TrackKat.Calibration
import TrackKat.Camera
import TrackKat.Config
import TrackKat.Frame (Packet (..))
import TrackKat.GamePose
import TrackKat.Icon (statusIcon, statusTip)
import TrackKat.Pose
import TrackKat.Response
import TrackKat.Stats hiding (Window)
import qualified TrackKat.Stats as Stats
import TrackKat.Status
import TrackKat.Tracker
import qualified TrackKat.Tray as Tray
import qualified TrackKat.USB as USB
import TrackKat.Window
import TrackKat.Windows

-- | How trackkat was started.
data Start
    = ByHand
    | -- | by NPClient, for a game (@--by-game@): in the tray, and quitting
      -- when nothing has used it for 'idleSeconds'
      ByGame

-- | When started by a game, how long trackkat waits, with no game tracking
-- and its window closed, before it quits. Long enough for a game to be
-- restarted.
idleSeconds :: Int
idleSeconds = 60

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    start <-
        getArgs >>= \case
            [] -> pure ByHand
            ["--by-game"] -> pure ByGame
            _ -> putStrLn "usage: trackkat [--by-game]" >> exitFailure
    dir <- takeDirectory <$> getExecutablePath
    first <- singleInstance
    unless first $ do
        shown <- Tray.showRunning
        putStrLn (if shown then "trackkat is already running; showing its window." else "trackkat is already running.")
        exitFailure
    w <- newWindow
    let note = sayIn w
    bracket_ (Tray.startTray >>= either (\e -> note ("Can't show a tray icon (error " ++ show e ++ ").")) pure) Tray.stopTray $ do
        let settingsFile = dir </> "trackkat.conf"
        (config, stamp) <- loadConfig w settingsFile
        openShared >>= either (\e -> note ("Can't create shared memory for games (error " ++ show e ++ ").") >> exitFailure) pure
        publish still
        setNPClientPath dir >>= \case
            Right () -> note ("Games will load NPClient from " ++ dir)
            Left e -> note ("Can't point games at NPClient (registry error " ++ show e ++ ").")
        unwatched <- startHotkeys (hotkeyBindings config)
        forM_ unwatched $ \case
            (Keyboard, e) -> note ("Can't watch the keyboard for hotkeys (error " ++ show e ++ ").")
            (Mouse, e) -> note ("Can't watch the mouse for buttons (error " ++ show e ++ ").")
            (Joysticks, e) -> note ("Can't watch joysticks for buttons (error " ++ show e ++ ").")
        note $
            printf
                "Recenter: %s   Pause: %s   Trim: %s   (Enter also recenters, c Enter calibrates level)"
                (keysAndButtons (recenterBindings config))
                (keysAndButtons (pauseBindings config))
                (keysAndButtons (trimBindings config))
        let calibrationFile = dir </> "trackkat.calibration"
        session <- newSession w config =<< loadCalibration w calibrationFile
        showButtons session
        let visibility = case start of
                ByHand -> OnScreen
                ByGame -> InTray
        -- The window, on an OS thread of its own as GLFW and OpenGL need.
        -- Tracking carries on without it.
        withAsyncBound (runWindow w visibility) $ \window ->
            withAsync (waitCatch window >>= note . windowStopped) $ \_ ->
                withAsync (console session) $ \_ ->
                    withAsync (watchSettings settingsFile session stamp config) $ \_ ->
                        withAsync (joysticks session) $ \_ ->
                            withAsync (saveCalibrations w calibrationFile (sSaving session)) $ \_ ->
                                withAsync (cameraLoop session) $ \camera -> do
                                    link camera
                                    controls start settingsFile session
                                    -- Let the camera go, turning its lights
                                    -- off, but not for long: after sleep a
                                    -- USB read may never return.
                                    _ <- timeout 3000000 (cancel camera)
                                    -- Reading the console can't be
                                    -- interrupted on Windows, so rather
                                    -- than wait for the console thread,
                                    -- leave at once.
                                    hFlush stdout
                                    Tray.quitProcess
  where
    windowStopped = \case
        Right (WindowFailure why) -> "Can't show trackkat's window: " ++ why ++ ". Tracking carries on, with the tray icon."
        Left e -> "trackkat's window stopped: " ++ displayException e ++ ". Tracking carries on, with the tray icon."

-- | Print a line, and add it to the log in trackkat's window.
sayIn :: Window -> String -> IO ()
sayIn w line = putStrLn line >> logLine w line

say :: Session -> String -> IO ()
say = sayIn . sWindow

-- | Connect to the camera and track, again and again if it goes away.
cameraLoop :: Session -> IO ()
cameraLoop s = forever $ do
    showCamera s Searching
    problem <-
        (USB.withContext (\ctx -> withCamera ctx (say s . ("# " ++)) defaultSettings (track s)) $> "The camera stopped.")
            `catches` [ Handler $ pure . explain
                      , Handler $ \e -> pure ("USB error: " ++ displayException (e :: USB.USBError))
                      , Handler $ \(ExceptionInLinkedThread _ e) -> pure ("Camera stopped: " ++ displayException e)
                      , Handler $ \CameraSilent -> pure "The camera went quiet, perhaps after sleep."
                      ]
    say s problem
    showCamera s (CameraDown problem)
    say s "Trying the camera again in 3 seconds."
    threadDelay 3000000

showCamera :: Session -> CameraState -> IO ()
showCamera s c = updateStatus (sWindow s) (\st -> st{stCamera = c})

-- | Act on what is asked of trackkat's window and tray icon, keep the tray
-- icon and the count of games current, and take a key to bind for the
-- window, until the user quits, or, when a game started trackkat, until
-- neither a game nor the window has used it for 'idleSeconds'.
controls :: Start -> FilePath -> Session -> IO ()
controls start path s = do
    shownInTray <- newIORef Nothing
    keyCapture <- newIORef Nothing
    let go checked idleSince = do
            threadDelay 20000
            mapM_ (act path s) =<< takeActions w
            choices <- Tray.takeChoices
            mapM_ choose choices
            captureKeys keyCapture
            st <- currentStatus w
            -- Drawing the icon takes a moment, so only when it changes.
            let tray = (statusIcon st, statusTip st)
            before <- atomicModifyIORef' shownInTray (Just tray,)
            when (before /= Just tray) $ Tray.showStatus st
            now <- getMonotonicTime
            if
                | Tray.QuitChoice `elem` choices -> say s "Quitting."
                -- Checking game processes is a few system calls; twice a
                -- second is plenty.
                | now - checked < 0.5 -> go checked idleSince
                | otherwise -> do
                    games <- gamesTracking
                    updateStatus w (\st' -> st'{stGames = games})
                    open <- windowOpen w
                    case (start, games > 0 || open, idleSince) of
                        (ByHand, _, _) -> go now Nothing
                        (ByGame, True, Nothing) -> go now Nothing
                        (ByGame, True, Just _) -> do
                            say s "A game or the window is using trackkat again; not quitting."
                            go now Nothing
                        (ByGame, False, Nothing) -> do
                            say s (printf "No game or window is using trackkat; quitting in %d s unless one starts." idleSeconds)
                            go now (Just now)
                        (ByGame, False, Just since)
                            | now - since >= fromIntegral idleSeconds ->
                                say s (printf "Quitting: no game or window used trackkat for %d s." idleSeconds)
                            | otherwise -> go now idleSince
    go 0 Nothing
  where
    w = sWindow s
    choose Tray.OpenChoice = openWindow w
    choose Tray.RecenterChoice = recenter s
    choose Tray.PauseChoice = togglePause s
    choose Tray.TrimChoice = trim s
    choose Tray.QuitChoice = pure ()
    -- The keyboard and mouse hooks take the next key or button pressed for
    -- the binding list the window waits on.
    captureKeys capture = do
        wanted <- capturing w
        armed <- readIORef capture
        case (wanted, armed) of
            (Nothing, Nothing) -> pure ()
            (Nothing, Just _) -> stopCapture >> writeIORef capture Nothing
            (Just k, _)
                | armed /= Just k -> captureKey >> writeIORef capture (Just k)
                | otherwise ->
                    takeCaptured >>= \case
                        Nothing -> pure ()
                        Just (Captured b) -> addBinding w k b >> writeIORef capture Nothing
                        Just Unbindable -> do
                            stopCapturing w
                            writeIORef capture Nothing
                            showSettingsMessage w "trackkat can't use that key; choose another."

act :: FilePath -> Session -> Action -> IO ()
act _ s RecenterAction = recenter s
act _ s PauseAction = togglePause s
act _ s TrimAction = trim s
act _ s CalibrateAction = calibrate s
act path s (SaveAction edits) = saveSettings (sWindow s) path edits

-- | Read the settings file, creating it with the defaults if there is none,
-- along with the modification time of what was read, and show it in
-- trackkat's window.
loadConfig :: Window -> FilePath -> IO (Config, Maybe UTCTime)
loadConfig w path = do
    exists <- doesFileExist path
    if not exists
        then do
            writeFile path (renderConfig defaultConfig)
            sayIn w ("Wrote default settings to " ++ path)
            showSettings w (settingValues (renderConfig defaultConfig))
            (defaultConfig,) <$> modificationTime path
        else do
            stamp <- modificationTime path
            readSettings path >>= \case
                Right (text, c) -> do
                    mapM_ (sayIn w . ((path ++ " ") ++)) (retiredSettings text)
                    showSettings w (settingValues text)
                    pure (c, stamp)
                Left err -> do
                    sayIn w (path ++ ": " ++ err)
                    exitFailure

-- | Read what was learnt in earlier sessions, or start afresh.
loadCalibration :: Window -> FilePath -> IO Calibration
loadCalibration w path = do
    exists <- doesFileExist path
    found <-
        if exists
            then (parseCalibration <$> readFile' path) `catch` \e -> pure (Left (displayException (e :: IOException)))
            else pure (Left "not found")
    case found of
        Right c -> do
            sayIn w (describeLevel c)
            sayIn w (describePivot Nothing c)
            showCalibration w (describeCalibration Nothing c)
            pure c
        Left err -> do
            when exists $ sayIn w (path ++ ": " ++ err ++ "; starting afresh.")
            sayIn w noCalibration
            showCalibration w noCalibration
            pure defaultCalibration
  where
    noCalibration = "No calibration yet: trackkat learns it as you turn your head."

-- | What the calibration box says.
showCalibration :: Window -> String -> IO ()
showCalibration w c = updateStatus w (\st -> st{stCalibration = c})

-- | Save each calibration handed over, only the latest when several wait,
-- so that saving never holds up tracking. The file is replaced whole.
saveCalibrations :: Window -> FilePath -> MVar Calibration -> IO ()
saveCalibrations w path pending = forever $ do
    c <- takeMVar pending
    let new = path ++ ".new"
    (writeFile new (renderCalibration c) >> renameFile new path)
        `catch` \e -> sayIn w ("Can't save the calibration: " ++ displayException (e :: IOException))

-- | Read and parse the settings file, whole so editors can replace it.
readSettings :: FilePath -> IO (Either String (String, Config))
readSettings path =
    ( try (readFile' path) >>= \case
        Left e -> pure (Left (displayException (e :: IOException)))
        Right text -> pure ((text,) <$> parseConfig text)
    )

-- | Write settings from trackkat's window to the settings file, keeping
-- the rest of the file. 'watchSettings' then applies them.
saveSettings :: Window -> FilePath -> [(String, String)] -> IO ()
saveSettings w path edited = do
    result <- try (readFile' path)
    case result of
        Left e -> showSettingsMessage w ("Not saved: " ++ displayException (e :: IOException))
        Right text -> case editSettings edited text of
            Left err -> showSettingsMessage w ("Not saved: " ++ err)
            Right new
                | new == text -> showSettingsMessage w "Nothing has changed."
                | otherwise -> do
                    -- Put in place whole, so it is never read half written.
                    let temp = path ++ ".new"
                    written <- try (writeFile temp new >> renameFile temp path)
                    showSettingsMessage w $ case written of
                        Left e -> "Not saved: " ++ displayException (e :: IOException)
                        Right () -> "Saved."

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
                            say s (path ++ ": " ++ err ++ "; keeping the previous settings.")
                            go stamp' config
                        Right (text, new) -> do
                            writeIORef (sSettings s) new
                            rebindHotkeys (hotkeyBindings new)
                            say s (describeChanges config new)
                            when (clip new /= clip config) $ do
                                atomicModifyIORef' (sTracker s) (\t -> (setModel (clipModel (clip new)) t, ()))
                                say s "Tracking the clip's new shape; recentering."
                            showSettings (sWindow s) (settingValues text)
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
    { sWindow :: Window
    , sTracker :: IORef Tracker
    , sSettings :: IORef Config
    -- ^ replaced when the settings file is saved
    , sShaper :: IORef Shaper
    , sClip :: IORef Clip
    , sPaused :: IORef (Maybe HeadPose)
    -- ^ the pose games see while paused
    , sTrim :: IORef Trim
    -- ^ where forward is for the view, until the next recenter
    , sLatest :: IORef HeadPose
    -- ^ the last pose sent, shaped
    , sButtonsShown :: IORef Double
    -- ^ until when joystick button presses are printed, in monotonic seconds
    , sSaving :: MVar Calibration
    -- ^ a calibration waiting to be saved
    }

newSession :: Window -> Config -> Calibration -> IO Session
newSession w config c =
    Session w
        <$> newIORef (newTracker defaultIntrinsics (clipModel (clip config)) c)
        <*> newIORef config
        <*> newIORef newShaper
        <*> newIORef ClipLost
        <*> newIORef Nothing
        <*> newIORef NoTrim
        <*> newIORef centred
        <*> newIORef 0
        <*> newEmptyMVar

-- | The head at its centre.
centred :: HeadPose
centred = HeadPose 0 0 0 0 0 0

-- | Recenter on the next frame that shows the clip, undoing any trim. If
-- the clip is out of sight, games see the view centred straight away.
recenter :: Session -> IO ()
recenter s = do
    atomicModifyIORef' (sTracker s) (\t -> (request Recenter t, ()))
    atomicModifyIORef' (sTrim s) (NoTrim,) >>= \case
        NoTrim -> pure ()
        Trim _ _ -> do
            updateStatus (sWindow s) (\st -> st{stTrim = NoTrim})
            say s "Trim cleared."
    readIORef (sClip s) >>= \case
        ClipSeen -> pure ()
        ClipLost -> do
            writeIORef (sShaper s) newShaper
            writeIORef (sLatest s) centred

calibrate :: Session -> IO ()
calibrate s = atomicModifyIORef' (sTracker s) (\t -> (request Calibrate t, ()))

-- | Pause, holding the pose games see, or resume.
togglePause :: Session -> IO ()
togglePause s = do
    latest <- readIORef (sLatest s)
    output <- atomicModifyIORef' (sPaused s) $ \case
        Just _ -> (Nothing, Live)
        Nothing -> (Just latest, Paused)
    updateStatus (sWindow s) (\st -> st{stOutput = output})
    say s $ case output of
        Live -> "Resumed."
        Paused -> "Paused: games see your head held still."

-- | Make where games look now the view's forward. While paused, that is the
-- view games are held at, and they stay held there; the trim applies from
-- when the view moves again.
trim :: Session -> IO ()
trim s = do
    paused <- readIORef (sPaused s)
    view <- maybe (readIORef (sLatest s)) pure paused
    let t = trimTo view
    writeIORef (sTrim s) t
    updateStatus (sWindow s) (\st -> st{stTrim = t})
    say s ("Trimmed: forward is now " ++ describeTrim t ++ ".")

-- | A trim as the console and log show it.
describeTrim :: Trim -> String
describeTrim NoTrim = "straight ahead"
describeTrim (Trim y p) = printf "yaw %+.0f pitch %+.0f" y p

console :: Session -> IO ()
console s = do
    eof <- isEOF
    unless eof $ do
        line <- getLine
        case line of
            "" -> recenter s
            "c" -> calibrate s
            "j" -> showButtons s
            _ -> say s "Enter recenters; c Enter calibrates level; j Enter shows joystick buttons"
        console s

-- | Print joystick button presses for the next minute, so they can be
-- copied into the settings file.
showButtons :: Session -> IO ()
showButtons s = do
    now <- getMonotonicTime
    writeIORef (sButtonsShown s) (now + 60)
    say s "For the next minute, joystick buttons you press are named here, ready to bind in trackkat.conf."

-- | Report joysticks coming and going, and the buttons pressed while
-- 'sButtonsShown' says to. A button pressed while trackkat's window waits
-- for one is added to the window's bindings.
joysticks :: Session -> IO ()
joysticks s = forever $ do
    threadDelay 100000
    events <- takeJoystickEvents
    now <- getMonotonicTime
    shownUntil <- readIORef (sButtonsShown s)
    forM_ events $ \case
        Connected j -> say s ("Joystick connected: " ++ renderJoystick j)
        Disconnected j -> say s ("Joystick disconnected: " ++ renderJoystick j)
        Pressed j n -> do
            let b = JoystickButton j n
            when (now < shownUntil) $ say s ("Joystick button pressed: " ++ renderBinding b)
            capturing (sWindow s) >>= mapM_ (\k -> addBinding (sWindow s) k b)

-- | Track until the camera goes away.
track :: Session -> Camera -> IO ()
track s cam = do
    say s "Tracking."
    showCamera s Tracking
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
                (outcome, events, tracker) <- atomicModifyIORef' (sTracker s) $ \t ->
                    let (t', o, evs) = step now stripes t in (t', (o, evs, t'))
                let centre = trackerCentre tracker
                forM_ events $ \e -> do
                    say s (describeEvent centre e)
                    mapM_ (showCalibration (sWindow s)) (calibrationNews centre (trackerCalibration tracker) e)
                forM_ (concatMap learnt events) $ \c -> tryTakeMVar (sSaving s) >> putMVar (sSaving s) c
                -- A new centre or level is a jump the shaper must not smooth.
                when (any resetsShaping events) $ writeIORef (sShaper s) newShaper
                case outcome of
                    Tracked{outPose = hp} -> do
                        writeIORef (sClip s) ClipSeen
                        r <- response <$> readIORef (sSettings s)
                        t <- readIORef (sTrim s)
                        shaped <- atomicModifyIORef' (sShaper s) $ \sh ->
                            let (p, sh') = shape r t now hp sh in (sh', p)
                        writeIORef (sLatest s) shaped
                    Lost _ -> writeIORef (sClip s) ClipLost
                paused <- readIORef (sPaused s)
                sent <- maybe (readIORef (sLatest s)) pure paused
                publish (toGamePose sent)
                published <- getMonotonicTime
                let sighting = case outcome of
                        Tracked{outPose = hp, outSeen = seen} -> InSight hp seen
                        Lost n -> OutOfSight n
                updateStatus (sWindow s) (\st -> st{stSighting = sighting, stView = sent})
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
                    -- The window shows the pose and figures live, so its
                    -- log is left to events.
                    putStrLn (status outcome paused gameConnected ++ "   " ++ summary w)
                    updateStatus (sWindow s) (\st -> st{stStats = summary w})
            Corrupt why _ -> do
                say s ("# dropped a corrupt packet: " ++ why)
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
noteGCs :: IORef Int -> IORef Stats.Window -> IO ()
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
    when (PauseKey `elem` keys) $ togglePause s
    when (TrimKey `elem` keys) $ trim s

-- | Bindings as the console lists them.
keysAndButtons :: [Binding] -> String
keysAndButtons [] = "none"
keysAndButtons bs = renderBindings bs

-- | Refinements are applied slowly, so only a new centre or a requested
-- calibration resets shaping.
resetsShaping :: Event -> Bool
resetsShaping Recentered = True
resetsShaping (Calibrated _) = True
resetsShaping _ = False

-- | Describe an event, given the clip's pose at the centre.
describeEvent :: Maybe Pose -> Event -> String
describeEvent _ Recentered = "Recentered."
describeEvent _ CalibrationStarted = "Calibrating: turn your head left and right."
describeEvent _ CalibrationFailed = "Calibration failed: not enough turning, try again."
describeEvent _ (Calibrated c) =
    let (down, right) = cameraTilt (estimatedLevel c)
     in printf "Calibrated: camera tipped %.1f degrees down, %.1f to its right." down right
describeEvent _ (Refined LevelRefined c) = describeLevel c
describeEvent centre (Refined PivotRefined c) = describePivot centre c

-- | A calibration to save.
learnt :: Event -> [Calibration]
learnt (Calibrated c) = [c]
learnt (Refined _ c) = [c]
learnt _ = []

-- | What the calibration box in trackkat's window says after an event, if
-- the event is about calibration or moves the centre, given the clip's pose
-- at the centre and the calibration so far.
calibrationNews :: Maybe Pose -> Calibration -> Event -> Maybe String
calibrationNews centre c Recentered = Just (describeCalibration centre c)
calibrationNews centre _ (Calibrated c) = Just (describeEvent centre (Calibrated c) ++ "\n" ++ describePivot centre c)
calibrationNews centre _ (Refined _ c) = Just (describeCalibration centre c)
calibrationNews centre _ e = Just (describeEvent centre e)

-- | The calibration box's account of a calibration.
describeCalibration :: Maybe Pose -> Calibration -> String
describeCalibration centre c = describeLevel c ++ "\n" ++ describePivot centre c

status :: Outcome -> Maybe HeadPose -> Bool -> String
status outcome paused gameConnected =
    concat
        [ case outcome of
            Tracked{outPose = hp, outSeen = seen} ->
                printf "yaw %+6.1f  pitch %+6.1f  roll %+6.1f   x %+4.0f  y %+4.0f  z %+4.0f mm" (yaw hp) (pitch hp) (roll hp) (tx hp) (ty hp) (tz hp)
                    ++ describeSeen seen
            Lost n -> "lost the clip (" ++ show n ++ " blob" ++ (if n == 1 then ")" else "s)")
        , maybe "" (const "   PAUSED") paused
        , if gameConnected then "   game connected" else ""
        ]
