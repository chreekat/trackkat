{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}

-- | trackkat: track the head and serve the pose to games.
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (ExceptionInLinkedThread (..), cancel, link, withAsync)
import Control.Concurrent.MVar
import Control.Exception (Handler (..), IOException, SomeAsyncException (..), SomeException, bracket_, catch, catches, displayException, fromException, throwIO, try)
import Control.Monad (foldM, forM_, forever, unless, void, (<=<))
import Data.Functor (($>))
import Data.IORef
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..), getZonedTime, zonedTimeToLocalTime)
import Data.Word (Word64, Word8)
import GHC.Clock (getMonotonicTime, getMonotonicTimeNSec)
import GHC.Stats (GCDetails (..), RTSStats (..), getRTSStats, getRTSStatsEnabled)
import System.Directory (doesFileExist, getModificationTime, makeAbsolute, removePathForcibly, renameFile)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))
import System.IO
import System.Timeout (timeout)
import Text.Printf (printf)

import TrackKat.Build
import TrackKat.Calibration
import TrackKat.Camera
import TrackKat.CommandLine
import TrackKat.Config
import TrackKat.Controls
import TrackKat.Frame (Packet (..))
import TrackKat.GamePose
import TrackKat.Gui
import TrackKat.Idle
import TrackKat.Install
import TrackKat.Log
import TrackKat.Pose
import TrackKat.Recorder
import TrackKat.Recording (Did (Marked), clockSeconds, startOfSession, trackerTime)
import TrackKat.Release
import TrackKat.ServedGame
import TrackKat.Session
import TrackKat.Stats
import TrackKat.Tracker (Outcome (..))
import qualified TrackKat.USB as USB
import TrackKat.Updater (Looking (..), answered, begun, dueLook, newUpdater, offerNotice)
import qualified TrackKat.Updater as Updater
import TrackKat.Windows
import TrackKat.World (Instant, clockAt)

-- | This build's version; none for a development build.
version :: Maybe Version
version = buildVersion >>= parseVersion

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    getArgs >>= \args -> case parseCommand args of
        Nothing -> putStr usage >> exitFailure
        Just ListFiles -> mapM_ putStrLn releaseFiles
        Just (UninstallCommand how) -> uninstall how
        Just InstallCommand -> installFromCommandLine
        Just (Run start window recorded after) -> run start window recorded after

-- | Install this copy, and start the installed one, taking over from any
-- trackkat running.
installFromCommandLine :: IO ()
installFromCommandLine = do
    paths <- findPaths
    v <- maybe (putStrLn "A development build can't be installed." >> exitFailure) pure version
    dir <- installCopy paths v >>= either (\err -> putStrLn ("Can't install trackkat: " ++ err) >> exitFailure) pure
    running <- quitRunning
    launch (dir </> "trackkat.exe") (afterArgs running) >>= \case
        Right () -> putStrLn ("Installed trackkat in " ++ dir ++ ".")
        Left e -> putStrLn ("Installed trackkat in " ++ dir ++ ", but can't start it (error " ++ show e ++ ").") >> exitFailure

run :: Start -> WindowAtStart -> Recorded -> Maybe Int -> IO ()
run start window recorded after = do
    paths <- findPaths
    installedNow <- installedVersions paths
    let place = placement (installRoot paths) (exeDir paths)
    -- An older version, perhaps started by a game from its NPClient, or
    -- with an update waiting, hands over to the newest installed.
    forM_ (newerInstalled place installedNow) $ \v ->
        launch (versionExe paths v) (startArgs start window recorded ++ afterArgs after) >>= \case
            Right () -> exitSuccess
            Left e -> consoleLine ("Can't start trackkat " ++ versionFolder v ++ " (error " ++ show e ++ "); carrying on.")
    let takeOverFrom pid = do
            _ <- waitForExit pid 30
            -- Games' NPClients check for trackkat, holding its name briefly.
            let claim n = singleInstance >>= \ok -> if ok || n <= (0 :: Int) then pure ok else threadDelay 200000 >> claim (n - 1)
            claim 50
    alone <- maybe singleInstance takeOverFrom after
    -- Asked to record, trackkat takes over from one already running,
    -- which is not recording, or not to the file asked for.
    first <- if alone || recorded == Unrecorded then pure alone else quitRunning >>= maybe (pure False) takeOverFrom
    unless first $ do
        shown <- showRunning
        consoleLine (if shown then "trackkat is already running; showing its window." else "trackkat is already running.")
        exitFailure
    showVersion (maybe "development build" describeVersion version)
    showUpdateButton (maybe UpdateButtonDisabled (const UpdateButtonEnabled) (selfUpdating place))
    bracket_ (startGui window >>= either (\e -> say ("Can't show a window or tray icon (error " ++ show e ++ ").")) pure) stopGui $ do
        say $ case (version, place) of
            (Nothing, _) -> "trackkat, a development build, which can't be installed or updated, running from " ++ exeDir paths
            (Just v, Installed _) -> "trackkat " ++ describeVersion v ++ ", installed in " ++ exeDir paths
            (Just v, Portable) -> "trackkat " ++ describeVersion v ++ ", running from " ++ exeDir paths
        moveSettings say paths
        let settingsFile = configFile paths
        (config, settings, stamp) <- loadConfig settingsFile
        openShared >>= either (\e -> say ("Can't create shared memory for games (error " ++ show e ++ ").") >> exitFailure) pure
        let gameFile = settingsDir paths </> "trackkat.game"
        before <- servedBefore gameFile
        startTrackIRStandIn before >>= \case
            Right () -> mapM_ (say <=< servedAgain) before
            Left TrackIRSoftwareRunning ->
                say "NaturalPoint's TrackIR software is running: quit it, or games loading its NPClient get their pose from it, not trackkat."
            Left (StandInFailed e) -> say ("Can't stand in for NaturalPoint's software (error " ++ show e ++ ").")
        publish still
        pointGames (exeDir paths) (npclient config)
        case place of
            Installed v -> register say paths v
            Portable -> pure ()
        setStartup say paths (startup config)
        offered <- newIORef NoOffer
        refreshOffer paths place offered
        unwatched <- startHotkeys (watchedKeys config)
        forM_ unwatched $ \case
            (Keyboard, e) -> say ("Can't watch the keyboard for hotkeys (error " ++ show e ++ ").")
            (Mouse, e) -> say ("Can't watch the mouse for buttons (error " ++ show e ++ ").")
            (Joysticks, e) -> say ("Can't watch joysticks for buttons (error " ++ show e ++ ").")
        say (describeBindings config)
        let calibrationFile = settingsDir paths </> "trackkat.calibration"
        (calibration, told, learnt) <- loadCalibration (clipModel (clip config)) calibrationFile
        withRecording recorded paths settings learnt $ \recording -> do
            shell <- Shell paths <$> newIORef (newSession config calibration) <*> pure offered <*> newEmptyMVar <*> newEmptyMVar <*> pure recording
            mapM_ (perform shell) told
            (naming, named) <- nameButtons <$> monotonicNow
            logLine named
            alongside
                [ ("Watching the settings file", watchSettings settingsFile shell stamp)
                , ("Watching joysticks", joysticks naming)
                , ("Saving calibrations", saveCalibrations calibrationFile (shSaving shell))
                , ("Noting the game served", rememberServedGame gameFile)
                , ("Keeping trackkat up to date", maintain place shell)
                ]
                $ withAsync (cameraLoop shell)
                $ \camera -> do
                    link camera
                    controls start recorded settingsFile shell
                    -- Let the camera go, turning its lights off, but not for
                    -- long: after sleep a USB read may never return.
                    _ <- timeout 3000000 (cancel camera)
                    mapM_ finishRecording recording
                    -- Rather than wait for the other threads, one perhaps in a
                    -- download, leave at once.
                    hFlush stdout `catch` \e -> const (pure ()) (e :: IOException)
                    quitProcess

-- | Run trackkat recording what happens to its session, if the command
-- line asked, to the file it named or one named for the time, in the
-- settings folder: the session starts from the settings file's text
-- given, and the calibration file's, if it starts from what one says.
-- Tracking goes on unrecorded if the file can't be written to.
withRecording :: Recorded -> Paths -> String -> Maybe String -> (Maybe (FilePath, SessionRecorder) -> IO a) -> IO a
withRecording Unrecorded _ _ _ act = act Nothing
withRecording recorded paths settings learnt act = do
    path <- case recorded of
        RecordedTo file -> makeAbsolute file
        _ -> (settingsDir paths </>) . named . zonedTimeToLocalTime <$> getZonedTime
    start <- startOfSession notes settings learnt <$> getMonotonicTimeNSec
    withRecorder path start (say . describeStopped) $ \case
        Left err -> say (describeUnrecorded err) >> act Nothing
        Right recorder -> say (describeRecording path) >> newSessionRecorder start recorder >>= act . Just . (path,)
  where
    named (LocalTime day (TimeOfDay hours minutes secs)) = recordingName (toGregorian day) (hours, minutes, floor secs)
    notes =
        [ "trackkat " ++ maybe "development build" describeVersion version
        , "camera threshold " ++ show (threshold defaultSettings) ++ ", IR " ++ show (irBrightness defaultSettings)
        ]

-- | Write what is left of a recording and close it, saying where it is,
-- but not waiting long for a disk that doesn't answer.
finishRecording :: (FilePath, SessionRecorder) -> IO ()
finishRecording (path, recorder) =
    timeout 5000000 (finish (sessionRecorder recorder)) >>= \case
        Just Whole -> say (describeRecorded path)
        -- Told when it stopped.
        Just Partial -> pure ()
        Nothing -> say (describeStopped "the disk didn't take the last of it in 5 seconds")

-- | Run threads, each named for what it does, alongside some work,
-- stopping them when it ends. Nothing waits for them, so one that an
-- exception stops says so in the log; the exception goes on.
alongside :: [(String, IO ())] -> IO a -> IO a
alongside threads work = foldr (\(what, thread) rest -> withAsync (noted what thread) (const rest)) work threads
  where
    noted what thread =
        thread `catch` \e -> do
            case fromException e of
                -- Told to stop, as when the work ends.
                Just (SomeAsyncException _) -> pure ()
                Nothing -> say (what ++ " stopped: " ++ displayException e)
            throwIO (e :: SomeException)

-- | Tell games to load the NPClient chosen, trackkat's from its own folder,
-- falling back to that if NaturalPoint's isn't installed.
pointGames :: FilePath -> NPClient -> IO ()
pointGames dir choice = do
    found <- case choice of
        TrackKatNPClient -> pure dir
        TrackIRNPClient ->
            findTrackIRFolder >>= \case
                Just folder -> pure folder
                Nothing -> do
                    say "npclient = trackir, but NaturalPoint's TrackIR software isn't installed; games will load trackkat's NPClient."
                    pure dir
    setNPClientPath found >>= \case
        Right ()
            | found == dir -> say ("Games will load trackkat's NPClient from " ++ dir)
            | otherwise -> say ("Games will load NaturalPoint's NPClient from " ++ found ++ "; start trackkat before the game.")
        Left e -> say ("Can't point games at NPClient (registry error " ++ show e ++ ").")

-- | What the window offers, as installed versions now stand.
refreshOffer :: Paths -> Placement -> IORef Offer -> IO ()
refreshOffer paths place offered = do
    o <- offer version place <$> installedVersions paths
    before <- atomicModifyIORef' offered (\b -> (o, b))
    unless (o == before) $ showOffer (offerNotice (exeDir paths) o)

-- | Whether trackkat has handed over to another copy of itself, and so is
-- to quit.
data HandOver = HandedOver | Staying

-- | Take up what the window offers: install this copy, or switch to an
-- installed one, handing over to it.
takeOffer :: Start -> Recorded -> Shell -> IO HandOver
takeOffer start recorded sh =
    readIORef (shOffered sh) >>= \case
        NoOffer -> pure Staying
        OfferInstall v -> do
            say ("Installing trackkat " ++ versionFolder v ++ " in " ++ installRoot paths ++ ".")
            installCopy paths v >>= \case
                Left err -> say ("Can't install trackkat: " ++ err) $> Staying
                Right dir -> switchTo (dir </> "trackkat.exe")
        OfferInstalled v -> switchTo (versionExe paths v)
        OfferUpdate v -> switchTo (versionExe paths v)
  where
    paths = shPaths sh
    switchTo exe =
        handOver exe (startArgs start WindowShown (continued recorded)) >>= \case
            Right () -> say ("Switching to " ++ exe ++ ".") $> HandedOver
            Left e -> say ("Can't start " ++ exe ++ " (error " ++ show e ++ ").") $> Staying

-- | The version of this copy, if it updates itself: an installed release.
-- Portable copies and development builds don't.
selfUpdating :: Placement -> Maybe Version
selfUpdating place = case (place, version) of
    (Installed _, Just self) -> Just self
    _ -> Nothing

-- | Keep an installed trackkat tidy and up to date: delete older versions
-- once games have let go of them, and look for a newer one when 'dueLook'
-- says, and whenever the user asks with 'shCheckAsked'. Portable copies and
-- development builds do neither. Only this thread looks, so two looks
-- never run at once.
maintain :: Placement -> Shell -> IO ()
maintain place sh = forM_ (selfUpdating place) $ \self -> go self newUpdater
  where
    go self u = do
        _ <- cleanUp say paths place
        now <- monotonicNow
        wanted <- updates . sSettings <$> readIORef (shSession sh)
        u' <- maybe (pure u) (\why -> look self why now u) (dueLook wanted now u)
        -- Read, not taken, so a click the timeout cuts off is still there
        -- next time round; emptied once a look has answered it, along with
        -- any clicks while it looked.
        timeout (10 * 60 * 1000000) (readMVar (shCheckAsked sh)) >>= \case
            Nothing -> go self u'
            Just () -> do
                u'' <- look self Asked now u'
                _ <- tryTakeMVar (shCheckAsked sh)
                go self u''
    -- Look, at the round begun at the time given, and say what was found.
    look self why began u = bracket_ (showUpdateCheck CheckRunning) (showUpdateCheck CheckIdle) $ do
        mapM_ act (begun why)
        found <- checkForUpdate paths self
        refreshOffer paths place (shOffered sh)
        waiting <- readIORef (shOffered sh)
        -- The wait for the next look runs from when the round began, or,
        -- for a look asked for, from its answer.
        from <- case why of
            Due -> pure began
            Asked -> monotonicNow
        let (u', effects) = answered self why from found waiting u
        mapM_ act effects
        pure u'
    act (Updater.Log line) = logLine line
    act (Updater.ShowStatus found) = showUpdateStatus found
    paths = shPaths sh

-- | Report the games that start head tracking through NaturalPoint's
-- NPClient.
reportTrackIRGames :: IO ()
reportTrackIRGames = takeTrackIRGame >>= mapM_ (say . describe)
  where
    describe (game, Just name) = printf "Game %d (%s) is using NaturalPoint's NPClient; scrambling its data." game name
    describe (game, Nothing) = printf "Game %d is using NaturalPoint's NPClient." game

-- | The game a trackkat that ran before was serving through NaturalPoint's
-- NPClient, as the file says, if it still runs; the file is deleted
-- otherwise.
servedBefore :: FilePath -> IO (Maybe ServedGame)
servedBefore path = do
    found <- (Just <$> readFile' path) `catch` \e -> const (pure Nothing) (e :: IOException)
    case parseServedGame <$> found of
        Nothing -> pure Nothing
        Just (Left err) -> say (path ++ ": " ++ err) >> forget
        Just (Right g) -> do
            running <- stillServed g <$> gameProcess (servedProcess g) <*> windowProcess (servedWindow g)
            maybe forget (pure . Just) running
  where
    forget = removePathForcibly path $> Nothing

-- | The log's line for a game served again.
servedAgain :: ServedGame -> IO String
servedAgain g = describeServedAgain g <$> maybe (pure Nothing) scrambledGameName (servedProfile g)

-- | Keep the file naming the game served through NaturalPoint's NPClient
-- up to date, deleting it while there is none, for 'servedBefore'. It is
-- replaced whole, so never read half written.
rememberServedGame :: FilePath -> IO ()
rememberServedGame path = go 0
  where
    go seen = do
        threadDelay 50000
        (changes, game) <- trackIRServed
        forM_ (note seen changes game) $ \todo ->
            keep todo `catch` \e -> say ("Can't note the game served: " ++ displayException (e :: IOException))
        go changes
    keep Delete = removePathForcibly path
    keep (Write text) = writeFile new text >> renameFile new path
    new = path ++ ".new"

-- | Print a line, and add it to the log in trackkat's window.
say :: String -> IO ()
say line = consoleLine line >> guiLog line

-- | Log a line where those it is for read it.
logLine :: Line -> IO ()
logLine (ForUser line) = say line
logLine (ForDeveloper line) = consoleLine line

-- | Print a line to the terminal trackkat was started from, or to the file
-- or pipe its output goes to, if any; quietly not, if it can't, as once
-- the terminal has closed the pipe.
consoleLine :: String -> IO ()
consoleLine line = putStrLn line `catch` \e -> const (pure ()) (e :: IOException)

-- | Connect to the camera and track, again and again if it goes away.
cameraLoop :: Shell -> IO ()
cameraLoop sh = forever $ do
    advance sh CameraSought
    problem <-
        (USB.withContext (\ctx -> withCamera ctx (say . ("# " ++)) defaultSettings (track sh)) $> "The camera stopped.")
            `catches` [ Handler $ pure . explain
                      , Handler $ \e -> pure ("USB error: " ++ displayException (e :: USB.USBError))
                      , Handler $ \(ExceptionInLinkedThread _ e) -> pure ("Camera stopped: " ++ displayException e)
                      , Handler $ \CameraSilent -> pure "The camera went quiet, perhaps after sleep."
                      ]
    advance sh (CameraStopped problem)
    threadDelay 3000000

-- | Act on what is asked of trackkat's window and tray icon, and keep the
-- window's count of games current, until the user quits, or, when a game
-- started trackkat, until neither a game nor the window has used it for
-- 'idleSeconds'.
controls :: Start -> Recorded -> FilePath -> Shell -> IO ()
controls start recorded path sh = go 0 Busy
  where
    go checked unused = do
        threadDelay 20000
        actions <- takeActions
        mapM_ act actions
        switched <- if OfferAction `elem` actions then takeOffer start recorded sh else pure Staying
        reportTrackIRGames
        now <- getMonotonicTime
        if
            | QuitAction `elem` actions -> pure ()
            | HandedOver <- switched -> pure ()
            -- Checking game processes is a few system calls; twice a
            -- second is plenty.
            | now - checked < 0.5 -> go checked unused
            | otherwise -> do
                games <- gamesTracking
                name <- if games > 0 then trackingGameName else pure Nothing
                showGames games name
                open <- windowOpen
                let (unused', said) = idle start (clockAt now) (if games > 0 || open then Used else Unused) unused
                mapM_ logLine said
                case unused' of
                    Quitting -> pure ()
                    _ -> go now unused'

    act RecenterAction = advance sh RecenterAsked
    act PauseAction = advance sh PauseAsked
    act TrimAction = advance sh TrimAsked
    act AimAction = advance sh AimAsked
    act CalibrateAction = advance sh CalibrateAsked
    act ClearCalibrationAction = advance sh ClearCalibrationAsked
    act QuitAction = say "Quitting."
    act OfferAction = pure ()
    act CheckForUpdatesAction = void (tryPutMVar (shCheckAsked sh) ())
    act SaveAction = saveSettings path
    act (KeyChosen k (Just b)) = addBinding k b
    act (KeyChosen _ Nothing) = showSettingsMessage "trackkat can't use that key; choose another."

-- | Read the settings file, creating it with the defaults if there is none:
-- its settings and its text, along with the modification time of what was
-- read; and show it in trackkat's window.
loadConfig :: FilePath -> IO (Config, String, Maybe UTCTime)
loadConfig path = do
    exists <- doesFileExist path
    if not exists
        then do
            writeFile path (renderConfig defaultConfig)
            say ("Wrote default settings to " ++ path)
            showSettings (renderConfig defaultConfig)
            (defaultConfig,renderConfig defaultConfig,) <$> modificationTime path
        else do
            stamp <- modificationTime path
            readSettings path >>= \case
                Right (text, c) -> do
                    mapM_ (say . ((path ++ " ") ++)) (retiredSettings text)
                    showSettings text
                    pure (c, text, stamp)
                Left err -> do
                    say (path ++ ": " ++ err)
                    exitFailure

-- | Read what was learnt in earlier sessions about a head wearing the given
-- clip, or start afresh: the calibration, what to say of it, and the text
-- of the file it is from, if it is from one.
loadCalibration :: Model -> FilePath -> IO (Calibration, [Effect], Maybe String)
loadCalibration m path = do
    exists <- doesFileExist path
    found <-
        if exists
            then Just <$> ((read' <$> readFile' path) `catch` \e -> pure (Left (displayException (e :: IOException))))
            else pure Nothing
    let (calibration, told) = startingCalibration path (fmap snd <$> found)
    pure (calibration, told, either (const Nothing) (Just . fst) =<< found)
  where
    read' text = (text,) <$> (screenCalibration m =<< parseCalibration text)

-- | Save each calibration handed over, only the latest when several wait,
-- so that saving never holds up tracking. The file is replaced whole.
saveCalibrations :: FilePath -> MVar Calibration -> IO ()
saveCalibrations path pending = forever $ do
    c <- takeMVar pending
    let new = path ++ ".new"
    (writeFile new (renderCalibration c) >> renameFile new path)
        `catch` \e -> say ("Can't save the calibration: " ++ displayException (e :: IOException))

-- | Show the settings file's values in trackkat's window, as written.
showSettings :: String -> IO ()
showSettings text = mapM_ (uncurry showSetting) (settingValues text)

-- | Write the settings from trackkat's window to the settings file, keeping
-- the rest of the file. 'watchSettings' then applies them.
saveSettings :: FilePath -> IO ()
saveSettings path = do
    edited <- savedSettings settingNames
    result <- try (readFile' path)
    case result of
        Left e -> showSettingsMessage ("Not saved: " ++ displayException (e :: IOException))
        Right text -> case editSettings edited text of
            Left err -> showSettingsMessage ("Not saved: " ++ err)
            Right new
                | new == text -> pure ()
                | otherwise -> do
                    -- Put in place whole, so it is never read half written.
                    let temp = path ++ ".new"
                    written <- try (writeFile temp new >> renameFile temp path)
                    showSettingsMessage $ case written of
                        Left e -> "Not saved: " ++ displayException (e :: IOException)
                        Right () -> "Saved."

-- | Nothing while the file is missing.
modificationTime :: FilePath -> IO (Maybe UTCTime)
modificationTime path = (Just <$> getModificationTime path) `catch` \e -> const (pure Nothing) (e :: IOException)

-- | Apply the settings file each time it is saved. A file that is missing
-- or doesn't parse leaves the settings as they were; an editor still
-- writing it changes its time again when done, so it is read again then.
watchSettings :: FilePath -> Shell -> Maybe UTCTime -> IO ()
watchSettings path sh = go
  where
    go stamp = do
        threadDelay 500000
        stamp' <- modificationTime path
        case stamp' of
            Just _
                | stamp' /= stamp -> do
                    readSettings path >>= \case
                        Left err -> say (path ++ ": " ++ err ++ "; keeping the previous settings.")
                        Right (text, new) -> advance sh (SettingsSaved text new)
                    go stamp'
            _ -> go stamp

explain :: OpenError -> String
explain NoCamera = "No TrackIR 5 found. Is it plugged in?"
explain (WrongDriver err) = "The TrackIR isn't using the WinUSB driver (" ++ err ++ "). See the README."
explain InUse = "The TrackIR is in use by another program, perhaps NaturalPoint's software."
explain (OpenFailed e) = "USB error: " ++ show e

-- | What trackkat's threads share.
--
-- See Note [What stays mutable].
data Shell = Shell
    { shPaths :: Paths
    , shSession :: IORef Session
    , shOffered :: IORef Offer
    -- ^ what the window was last told it offers
    , shSaving :: MVar Calibration
    -- ^ a calibration waiting to be saved
    , shCheckAsked :: MVar ()
    -- ^ full from when the user asks to look for a newer version until
    -- 'maintain' has looked
    , shRecording :: Maybe (FilePath, SessionRecorder)
    -- ^ the file the session is recorded to, if it is, and its recorder
    }

-- Note [What stays mutable]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
-- trackkat's threads each wait on something of their own: the camera, the
-- window, the settings file, joysticks, the game served, the network. What
-- they decide by is in the library, as values taken from one state to the
-- next; what is left here is what two threads must both reach.
--
--  * 'shSession': the camera's frames, the window's buttons and the settings
--    file all change the one session, each change whole, through
--    'transition'.
--
--  * 'shOffered': 'maintain' finds what there is to offer, and the window's
--    thread takes the offer up. It also remembers what the window was last
--    told, so the window is told only of a change.
--
--  * 'shSaving' and 'shCheckAsked' hand work to a thread that may be slow, a
--    disk or the network, without waiting for it: the latest calibration to
--    save, and a click to answer.
--
--  * 'shRecording': each change to the session is handed to the recording
--    as it is made, so that the recording has them in the session's order,
--    whichever threads made them (see 'recordStep').
--
-- The rest of what a thread remembers from one round to the next is an
-- argument of its loop.

-- | The moment the monotonic clock tells.
monotonicNow :: IO Instant
monotonicNow = clockAt <$> getMonotonicTime

-- | Take the session through an event: the session after it, and what the
-- event asks to be done. A recording has it as happening now.
transition :: Shell -> Event -> IO (Session, [Effect])
transition sh event = case shRecording sh of
    Nothing -> transitionAt sh 0 0 event
    Just _ -> getMonotonicTimeNSec >>= \at -> transitionAt sh at 0 event

-- | Take the session through an event, as 'transition' does, that
-- happened at the given time on the monotonic clock, a frame having the
-- given counter, as a recording has it.
transitionAt :: Shell -> Word64 -> Word8 -> Event -> IO (Session, [Effect])
transitionAt sh at n event = case shRecording sh of
    Nothing -> stepped
    Just (_, recorder) -> recordStep recorder at n ((\result@(_, effects) -> (result, [did | Record did <- effects])) <$> stepped)
  where
    stepped = atomicModifyIORef' (shSession sh) $ \s -> let (s', effects) = step event s in (s', (s', effects))

-- | Take the session through an event, and do what it asks.
advance :: Shell -> Event -> IO ()
advance sh event = transition sh event >>= mapM_ (perform sh) . snd

perform :: Shell -> Effect -> IO ()
perform sh = \case
    Log line -> logLine line
    ShowCamera state -> showCamera state
    ShowOutput output -> showOutput output
    ShowTrim t -> showTrim t
    ShowAim a -> showAim a
    ShowCalibration c -> showCalibration c
    ShowClip reflectors -> showClip reflectors
    ShowFrame sighting view -> showFrame sighting view
    ShowBlobs blobs areas -> showBlobs blobs areas
    ShowSettings text -> showSettings text
    Publish pose -> publish pose
    SaveCalibration c -> tryTakeMVar (shSaving sh) >> putMVar (shSaving sh) c
    BindHotkeys config -> rebindHotkeys (watchedKeys config)
    PointGames choice -> pointGames (exeDir (shPaths sh)) choice
    SetStartup choice -> setStartup say (shPaths sh) choice
    Record (Marked n) -> say . (`describeMark` n) =<< traverse (soFar . sessionRecorder . snd) (shRecording sh)
    Record _ -> pure ()

-- | What the camera's thread keeps while the camera is connected.
data Connection = Connection
    { cGamesChecked :: !Double
    -- ^ when the games tracking were last counted, in monotonic seconds
    , cGames :: !Int
    -- ^ how many there were
    , cStatusShown :: !Double
    -- ^ when the status was last shown
    , cLastArrival :: !(Maybe Double)
    -- ^ when the frame before arrived
    , cWindow :: !Window
    -- ^ the figures since the status was last shown
    , cGCs :: !Int
    -- ^ garbage collections when last looked at
    }

-- | Report joysticks coming and going, and the buttons pressed, as
-- 'joystickEffects' has it.
joysticks :: ButtonNaming -> IO ()
joysticks naming = forever $ do
    threadDelay 100000
    events <- takeJoystickEvents
    now <- monotonicNow
    forM_ (concatMap (joystickEffects naming now) events) $ \case
        JoystickLog line -> logLine line
        OfferButton b -> capturing >>= mapM_ (`addBinding` b)

-- | Track until the camera goes away.
track :: Shell -> Camera -> IO ()
track sh cam = do
    advance sh . CameraConnected =<< recenterCount
    collections <- gcCount
    withLights cam $ \lights -> withPackets cam $ \next ->
        let go c = do
                (arrived, packets) <- next
                foldM (packet lights arrived) c packets >>= go
         in go (Connection 0 0 0 Nothing emptyWindow collections)
  where
    packet :: (Light -> Light -> IO ()) -> Double -> Connection -> Packet -> IO Connection
    packet lights arrived c = \case
        Frame n stripes -> do
            ns <- getMonotonicTimeNSec
            let now = clockSeconds ns
                seen = trackerTime ns
            edges <- takeKeyEdges seen
            mapM_ (perform sh) . snd =<< transitionAt sh ns n . ControlsPolled seen edges =<< recenterCount
            (s, effects) <- transitionAt sh ns n (FrameArrived seen stripes)
            -- When games were sent the frame's pose.
            published <- foldM (\at e -> perform sh e >> case e of Publish _ -> Just <$> getMonotonicTime; _ -> pure at) Nothing effects
            let timed = maybe id (\at -> sawFrame (at - arrived) (maybe 0 (arrived -) (cLastArrival c)) (length stripes)) published (cWindow c)
            (collections, window) <- noteGCs (cGCs c) timed

            -- Checking game processes is a few system calls; twice a
            -- second is plenty.
            (checked, games) <-
                if now - cGamesChecked c >= 0.5
                    then (now,) <$> gamesTracking
                    else pure (cGamesChecked c, cGames c)
            lights (case sOutcome s of Just Tracked{} -> On; _ -> Off) (if games > 0 then On else Off)

            let c' = c{cGamesChecked = checked, cGames = games, cLastArrival = Just arrived, cWindow = window, cGCs = collections}
            if now - cStatusShown c >= 1
                then do
                    -- The window shows the pose and figures live, so its
                    -- log is left to events, as is a terminal's, which
                    -- has its prompt back; only output captured or piped
                    -- gets them.
                    consoleOutput >>= \case
                        OutputRedirected -> forM_ (sOutcome s) $ \outcome ->
                            consoleLine (status outcome (sSending s) games ++ "   " ++ summary window)
                        ParentConsole -> pure ()
                        NoConsole -> pure ()
                    showStats (summary window)
                    pure c'{cStatusShown = now, cWindow = emptyWindow}
                else pure c'
        Corrupt why _ -> do
            advance sh (PacketCorrupt why)
            pure c{cWindow = sawCorrupt (cWindow c)}
        _ -> pure c

-- | Garbage collections so far, if the runtime is counting (+RTS -T).
gcCount :: IO Int
gcCount = do
    enabled <- getRTSStatsEnabled
    if enabled then fromIntegral . gcs <$> getRTSStats else pure 0

-- | Add to a window the collections since there were the given number,
-- with the latest one's pause: how many there are now, and the window.
-- Sampled every frame, so a pause hidden behind a later one in the same
-- frame is missed.
noteGCs :: Int -> Window -> IO (Int, Window)
noteGCs before window = do
    enabled <- getRTSStatsEnabled
    if enabled
        then do
            stats <- getRTSStats
            let n = fromIntegral (gcs stats)
            pure (n, if n /= before then sawGCs (n - before) (fromIntegral (gcdetails_elapsed_ns (gc stats)) / 1e9) window else window)
        else pure (before, window)
