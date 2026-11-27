{-# LANGUAGE LambdaCase #-}

-- | trackkat's window and system tray icon: see cbits/gui.c. They run on a
-- thread of their own; state is pushed in with the @show@ functions, which
-- never wait on the window, and what the user asks for is polled with
-- 'takeActions'.
module TrackKat.Gui (
    WindowAtStart (..),
    withGui,
    quitProcess,
    showRunning,
    quitRunning,
    showVersion,
    openSettingsFile,
    UpdateButton (..),
    showUpdateButton,
    UpdateCheck (..),
    showUpdateCheck,
    UpdateStatus (..),
    showUpdateStatus,
    Notice (..),
    showOffer,
    windowOpen,
    Action (..),
    takeActions,
    capturing,
    addBinding,
    guiLog,
    CameraState (..),
    showCamera,
    Sighting (..),
    showFrame,
    showBlobs,
    showClip,
    Output (..),
    showOutput,
    showTrim,
    showAim,
    showGames,
    showStats,
    showCalibration,
    showSetting,
    showSettingsMessage,
    takeSavedSettings,
) where

import Control.Concurrent.Async (link, wait, withAsyncBound)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (Exception, bracket, finally, throwIO, try)
import Control.Monad (forM_, void, when)
import Data.Bifunctor (first)
import Data.Bits (shiftL, shiftR, testBit, (.&.))
import Data.Either (isRight)
import Data.Maybe (fromMaybe)
import Foreign.C.String (CWString, peekCWString, withCWString)
import Foreign.C.Types (CFloat (..), CInt (..))
import Foreign.Marshal.Array (allocaArray, withArray, withArrayLen)
import Foreign.Ptr (Ptr, nullPtr)
import Graphics.Win32.GDI.Types (HWND)
import Graphics.Win32.Message (wM_QUIT)
import Graphics.Win32.Window (LPMSG, allocaMessage, dispatchMessage, getMessage, translateMessage)
import Graphics.Win32.Window.PostMessage (postThreadMessage)
import System.Win32.Thread (getCurrentThreadId)
import System.Win32.Types (BOOL, HANDLE)

import TrackKat.CommandLine (WindowAtStart (..))
import TrackKat.Config (KeyOrButton, WhichWindow (..), keyFromCodes, renderKeyOrButton)
import TrackKat.Controls (Hotkey (..))
import TrackKat.Display
import TrackKat.Frame (Blob (..))
import TrackKat.Gui.Calls (raiseWindow)
import TrackKat.Gui.Curve (registerCurve)
import TrackKat.Gui.Kit (fontHandles, kitDpi, setUp)
import TrackKat.Gui.Live (registerLive)
import TrackKat.Gui.Log (makeLog)
import TrackKat.Gui.Masthead (registerMasthead)
import TrackKat.Gui.Shown (logLine, takeSnapshot, tell)
import TrackKat.Gui.Tray (makeTray, startTray)
import TrackKat.Gui.Window (makeMain)
import TrackKat.Linear (V3 (..))
import TrackKat.Pose (HeadPose (..), Reflector (..))
import TrackKat.Response (Aiming (..), Trim (..))
import TrackKat.Trackers (BlobRole (..))
import TrackKat.UI.Live (TrimMoves (..), trimMoves)
import TrackKat.UI.Tray (TrayChoice (..))

foreign import ccall safe "hts_gui_start" c_start :: CInt -> IO CInt
foreign import ccall safe "hts_gui_build" c_build :: HWND -> HWND -> HWND -> CInt -> Ptr HANDLE -> IO ()
foreign import ccall unsafe "windows.h SetProcessDPIAware" c_SetProcessDPIAware :: IO BOOL
foreign import ccall unsafe "hts_gui_ask" c_ask :: CInt -> IO ()
foreign import ccall safe "hts_gui_dialog_message" c_dialog_message :: LPMSG -> IO CInt
foreign import ccall safe "hts_gui_stop" c_stop :: IO ()
foreign import ccall safe "hts_gui_quit" c_quit :: IO ()
foreign import ccall unsafe "hts_gui_show_running" c_show_running :: IO CInt
foreign import ccall unsafe "hts_gui_quit_running" c_quit_running :: IO CInt
foreign import ccall unsafe "hts_gui_version" c_version :: CWString -> IO ()
foreign import ccall unsafe "hts_gui_open_settings" c_open_settings :: CWString -> IO ()
foreign import ccall unsafe "hts_gui_updatable" c_updatable :: CInt -> IO ()
foreign import ccall unsafe "hts_gui_checking" c_checking :: CInt -> IO ()
foreign import ccall unsafe "hts_gui_update_status" c_update_status :: CInt -> CWString -> IO ()
foreign import ccall unsafe "hts_gui_offer" c_offer :: CWString -> CWString -> CWString -> IO ()
foreign import ccall unsafe "hts_gui_window_open" c_window_open :: IO CInt
foreign import ccall unsafe "hts_gui_take_actions" c_take_actions :: IO CInt
foreign import ccall unsafe "hts_gui_chosen_key" c_chosen_key :: CInt -> IO CInt
foreign import ccall unsafe "hts_gui_capturing" c_capturing :: IO CInt
foreign import ccall unsafe "hts_gui_add_binding" c_add_binding :: CInt -> CWString -> IO ()
foreign import ccall unsafe "hts_gui_log" c_log :: CWString -> IO ()
foreign import ccall unsafe "hts_gui_camera" c_camera :: CInt -> CWString -> IO ()
foreign import ccall unsafe "hts_gui_frame"
    c_frame :: CInt -> CInt -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> IO ()
foreign import ccall unsafe "hts_gui_blobs" c_blobs :: CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> IO ()
foreign import ccall unsafe "hts_gui_clip" c_clip :: CInt -> Ptr CFloat -> IO ()
foreign import ccall unsafe "hts_gui_paused" c_paused :: CInt -> CFloat -> CFloat -> IO ()
foreign import ccall unsafe "hts_gui_trim" c_trim :: CInt -> CFloat -> CFloat -> CInt -> IO ()
foreign import ccall unsafe "hts_gui_aiming" c_aiming :: CInt -> IO ()
foreign import ccall unsafe "hts_gui_games" c_games :: CInt -> CWString -> IO ()
foreign import ccall unsafe "hts_gui_stats" c_stats :: CWString -> IO ()
foreign import ccall unsafe "hts_gui_calibration" c_calibration :: CWString -> IO ()
foreign import ccall unsafe "hts_gui_settings_message" c_settings_message :: CWString -> IO ()
foreign import ccall unsafe "hts_gui_set_setting" c_set_setting :: CWString -> CWString -> IO ()
foreign import ccall unsafe "hts_gui_take_saved_setting" c_take_saved_setting :: CWString -> CWString -> CInt -> IO CInt

-- | Run the window and tray icon chosen alongside some work, which is told
-- whether they started (Left is a Windows error code), and remove them when
-- it ends.
withGui :: WhichWindow -> WindowAtStart -> (Either Int () -> IO a) -> IO a
withGui CWindow w work = bracket (c_start (windowCode w)) (const c_stop) (work . started)
withGui HaskellWindow w work = do
    made <- newEmptyMVar
    withAsyncBound (runGui made) $ \gui -> do
        link gui
        (result, thread) <- takeMVar made
        work result
            `finally` (c_stop >> when (isRight result) (postThreadMessage thread wM_QUIT 0 0) >> wait gui)
  where
    -- Windows belong to the thread that made them, which alone gets their
    -- messages: a bound thread, so it is always the same one.
    runGui made = do
        thread <- getCurrentThreadId
        -- Before any window, each of which keeps the awareness it is made with.
        _ <- c_SetProcessDPIAware
        kit <- setUp
        takeSnapshot
        result <- start kit
        putMVar made (result, thread)
        when (isRight result) (allocaMessage messageLoop)
    start kit = first (\(Failed e) -> e) <$> try (makeAll kit)
    makeAll kit = do
        mapM_ (\register -> register kit guiLog >>= orFail) [registerMasthead, registerCurve, registerLive]
        window <- makeMain kit guiLog >>= orFail
        logWindow <- makeLog kit guiLog >>= orFail
        tray <- makeTray guiLog (choose window) >>= orFail
        withArray (fontHandles kit) (c_build tray window logWindow (fromIntegral (kitDpi kit)))
        startTray tray
        case w of
            WindowShown -> raiseWindow window
            WindowHidden -> pure ()
    orFail = either (throwIO . Failed) pure
    messageLoop msg =
        getMessage msg Nothing >>= \more -> when more $ do
            handled <- c_dialog_message msg
            when (handled == 0) (translateMessage msg >> void (dispatchMessage msg))
            messageLoop msg

-- | A Windows error code that making the window failed with.
newtype Failed = Failed Int
    deriving (Show)

instance Exception Failed

-- | What the user chose from the tray icon or its menu, or another trackkat
-- asked for, with the window.
choose :: HWND -> TrayChoice -> IO ()
choose window OpenChoice = raiseWindow window
choose _ RecenterChoice = ask RecenterAction
choose _ PauseChoice = ask PauseAction
choose _ TrimChoice = ask TrimAction
choose _ OfferChoice = ask OfferAction
choose _ QuitChoice = ask QuitAction

-- | Ask for something, as the window does, for 'takeActions'.
ask :: Action -> IO ()
ask a = forM_ [b | (Asked a', b) <- actBits, a' == a] (c_ask . shiftL 1)

windowCode :: WindowAtStart -> CInt
windowCode WindowShown = 1
windowCode WindowHidden = 0

started :: CInt -> Either Int ()
started 0 = Right ()
started rc = Left (fromIntegral rc)

-- | Remove the tray icon and end the process at once, whatever other
-- threads are doing.
quitProcess :: IO a
quitProcess = c_quit >> error "quitProcess returned"

-- | Show the window of a trackkat already running; False if none is.
showRunning :: IO Bool
showRunning = (/= 0) <$> c_show_running

-- | Ask a trackkat already running to quit, returning its process ID, if
-- one is running.
quitRunning :: IO (Maybe Int)
quitRunning = (\pid -> if pid == 0 then Nothing else Just (fromIntegral pid)) <$> c_quit_running

-- | The version the window's title, its strip across the top and the log
-- window show. Call before 'withGui'.
showVersion :: String -> IO ()
showVersion v = tell (\s -> s{shownVersion = v}) >> withCWString v c_version

-- | Open the settings file in an editor: the program Windows opens such a
-- file with, the one the user picks if there is none, or else Notepad.
openSettingsFile :: FilePath -> IO ()
openSettingsFile path = withCWString path c_open_settings

-- | Whether the window's Check for updates button works: only for a copy
-- that updates itself.
data UpdateButton = UpdateButtonEnabled | UpdateButtonDisabled

-- | Enable the Check for updates button, or not. Call before 'withGui'.
showUpdateButton :: UpdateButton -> IO ()
showUpdateButton UpdateButtonEnabled = c_updatable 1
showUpdateButton UpdateButtonDisabled = c_updatable 0

-- | Show a look for a newer version running, on the Check for updates
-- button, which says so and can't be clicked until it is done.
showUpdateCheck :: UpdateCheck -> IO ()
showUpdateCheck c = tell (\s -> s{shownCheck = c}) >> c_checking (case c of CheckRunning -> 1; CheckIdle -> 0)

-- | Show what the last look for a newer version found, in the strip across
-- the top of the window, beside the Check for updates button.
showUpdateStatus :: UpdateStatus -> IO ()
showUpdateStatus u = tell (\s -> s{shownStatus = Just u}) >> push u
  where
    push (UpToDate s) = withCWString s (c_update_status 1)
    push (UpdateNote s) = withCWString s (c_update_status 0)

-- | Offer something in the strip across the top of the window, which turns
-- amber, with a button, and perhaps an item in the tray's menu, either of
-- which asks for it with 'OfferAction'. Nothing takes the offer back.
showOffer :: Maybe Notice -> IO ()
showOffer o = do
    tell (\s -> s{shownOffer = o})
    withCWString text $ \t -> withCWString button $ \b -> withCWString tray (c_offer t b)
  where
    (text, button, tray) = case o of
        Just (Notice t b m) -> (t, b, fromMaybe "" m)
        Nothing -> ("", "", "")

-- | Whether the window or the log's own window is open; hidden in the tray
-- doesn't count.
windowOpen :: IO Bool
windowOpen = (/= 0) <$> c_window_open

-- | Something the user asked for in the window or the tray.
data Action
    = RecenterAction
    | PauseAction
    | TrimAction
    | AimAction
    | CalibrateAction
    | -- | forget the learnt calibration and track afresh
      ClearCalibrationAction
    | QuitAction
    | -- | the strip's button, or its item in the tray's menu
      OfferAction
    | -- | look for a newer version now
      CheckForUpdatesAction
    | -- | a change in the window to save: take it with 'takeSavedSettings'
      SaveAction
    | -- | the Edit settings file button, which first asks to save a change
      -- waiting to be ('SaveAction'), but nothing else
      OpenSettingsAction
    | -- | a key pressed to add to a binding list, if it has a name
      KeyChosen Hotkey (Maybe KeyOrButton)
    deriving (Eq, Show)

-- | What the user asked for since the last call, in the order to act on
-- it.
takeActions :: IO [Action]
takeActions = do
    bits <- c_take_actions
    mapM taken [asked | (asked, b) <- actBits, testBit bits b]
  where
    taken (Asked a) = pure a
    taken (KeyChosenFor k) = do
        code <- maybe (pure 0) (fmap fromIntegral . c_chosen_key) (list k)
        pure (KeyChosen k (keyFromCodes (code `shiftR` 16) (code .&. 0xFFFF)))

-- | What an ACT_ bit of cbits/gui.c's asks for.
data Asked
    = Asked Action
    | -- | a key was chosen for the hotkey's binding list: see hts_gui_chosen_key
      KeyChosenFor Hotkey

-- | cbits/gui.c's ACT_ bits, in the order 'takeActions' lists them.
actBits :: [(Asked, Int)]
actBits =
    [ (Asked RecenterAction, 0)
    , (Asked PauseAction, 1)
    , (Asked CalibrateAction, 2)
    , (Asked QuitAction, 3)
    , (Asked SaveAction, 4)
    , (Asked TrimAction, 7)
    , (Asked ClearCalibrationAction, 9)
    , (Asked OfferAction, 10)
    , (Asked CheckForUpdatesAction, 11)
    , (Asked AimAction, 12)
    , -- After SaveAction, so the editor opens on the change saved with it.
      (Asked OpenSettingsAction, 14)
    , (KeyChosenFor RecenterKey, 5)
    , (KeyChosenFor PauseKey, 6)
    , (KeyChosenFor TrimKey, 8)
    , (KeyChosenFor AimKey, 13)
    ]

-- | The binding list waiting for a key or joystick button to add, if any.
capturing :: IO (Maybe Hotkey)
capturing =
    c_capturing >>= \case
        0 -> pure (Just RecenterKey)
        1 -> pure (Just PauseKey)
        2 -> pure (Just TrimKey)
        3 -> pure (Just AimKey)
        _ -> pure Nothing

-- | Add a key or button to a hotkey's list in the window, if it has one,
-- bound as the settings file binds one with no gesture written, and stop
-- waiting for one.
addBinding :: Hotkey -> KeyOrButton -> IO ()
addBinding k b = forM_ (list k) $ \which -> withCWString (renderKeyOrButton b) (c_add_binding which)

-- | How cbits/gui.c numbers the binding lists; marking has none, and is
-- bound in the settings file alone.
list :: Hotkey -> Maybe CInt
list RecenterKey = Just 0
list PauseKey = Just 1
list TrimKey = Just 2
list AimKey = Just 3
list MarkKey = Nothing

-- | Add a line to the log the window shows.
guiLog :: String -> IO ()
guiLog line = logLine line >> withCWString line c_log

-- | Set the window's Camera light, and its words if the camera is down.
showCamera :: CameraState -> IO ()
showCamera c = tell (\s -> s{shownCamera = c}) >> push c
  where
    push Searching = withCWString "" (c_camera 0)
    push Tracking = withCWString "" (c_camera 1)
    push (CameraDown why) = withCWString why (c_camera 2)

-- | Show a frame: the head, which reflectors were seen, and the pose games
-- are sent. Skipped if the window is busy copying the last one.
showFrame :: Sighting -> HeadPose -> IO ()
showFrame sighting view =
    tell (\s -> s{shownSighting = sightingC, shownView = poseC view})
        >> c_frame blobs shown (f yaw) (f pitch) (f roll) (f tx) (f ty) (f tz) (g yaw) (g pitch) (g roll) (g tx) (g ty) (g tz)
  where
    (blobs, shown, head') = case sighting of
        InSight hp missing -> (-1, sum [bit | (which, bit) <- reflectorBits, which `notElem` missing], hp)
        OutOfSight n -> (fromIntegral n, 0, view)
    -- As REFLECTOR_LEFT and the rest in cbits/gui.c.
    reflectorBits = [(LeftReflector, 1), (RightReflector, 2), (TopReflector, 4)]
    f axis = realToFrac (axis head')
    g axis = realToFrac (axis view)
    sightingC = case sighting of
        InSight hp missing -> InSight (poseC hp) missing
        OutOfSight n -> OutOfSight n

-- | Show a frame's blobs, heaviest first, at their places in the camera's
-- image with their size and shape, and what each was taken for; and the
-- areas masked around still lights, as centre and radius in pixels. The
-- window shows the first few of each. Skipped if the window is busy copying
-- the last ones.
showBlobs :: [(Blob, BlobRole)] -> [((Double, Double), Double)] -> IO ()
showBlobs blobs areas = do
    tell (\s -> s{shownBlobs = map blobC (take blobsMax blobs), shownAreas = map areaC (take areasMax areas)})
    withArrayLen (concatMap spot (take blobsMax blobs)) $ \n spots ->
        withArrayLen (concatMap area (take areasMax areas)) $ \m ->
            c_blobs (fromIntegral (n `div` 7)) spots (fromIntegral (m `div` 3))
  where
    -- As BLOBS_MAX and AREAS_MAX in cbits/gui.c.
    blobsMax = 16
    areasMax = 8
    spot (b, role) =
        map realToFrac [blobX b, blobY b, fromIntegral (blobPoints b)]
            ++ [roleCode role]
            ++ map realToFrac [blobVarX b, blobVarY b, blobCovXY b]
    area ((x, y), r) = map realToFrac [x, y, r]
    blobC (b, role) = (b{blobX = single (blobX b), blobY = single (blobY b), blobVarX = single (blobVarX b), blobVarY = single (blobVarY b), blobCovXY = single (blobCovXY b)}, role)
    areaC ((x, y), r) = ((single x, single y), single r)
    -- As SPOT_REFLECTOR and the rest in cbits/gui.c.
    roleCode ReflectorBlob = 0
    roleCode StrayBlob = 1
    roleCode MaskedBlob = 2

-- | Show the clip's reflectors (left, right, top) on the head, in mm from
-- the neck pivot along the head's axes (see 'TrackKat.Pose.inHeadAxes'), or
-- show none.
showClip :: Maybe (V3, V3, V3) -> IO ()
showClip clip = tell (\s -> s{shownClip = fmap (\(l, r, t) -> (v3C l, v3C r, v3C t)) clip}) >> push clip
  where
    push Nothing = c_clip 0 nullPtr
    push (Just (l, r, t)) = withArray (concatMap coords [l, r, t]) (c_clip 1)
    coords (V3 x y z) = map realToFrac [x, y, z]
    v3C (V3 x y z) = V3 (single x) (single y) (single z)

-- | Light the Pause button, saying where the view is held, or not.
showOutput :: Output -> IO ()
showOutput o = tell (\s -> s{shownOutput = case o of Live -> Live; Paused held -> Paused (poseC held)}) >> push o
  where
    push Live = c_paused 0 0 0
    push (Paused held) = c_paused 1 (realToFrac (yaw held)) (realToFrac (pitch held))

-- | Where forward is for the view.
showTrim :: Trim -> IO ()
showTrim trim = tell (\s -> s{shownTrim = case trim of NoTrim -> NoTrim; Trim t -> Trim (poseC t)}) >> push trim
  where
    push NoTrim = c_trim 0 0 0 0
    push (Trim t) = c_trim 1 (realToFrac (yaw t)) (realToFrac (pitch t)) (case trimMoves t of MovesToo -> 1; TurnsOnly -> 0)

-- | Light the Aim button, or not.
showAim :: Aiming -> IO ()
showAim a = tell (\s -> s{shownAiming = a}) >> c_aiming (case a of NotAiming -> 0; Aiming -> 1)

-- | How many games are tracking, and the name of one of them, if known.
showGames :: Int -> Maybe String -> IO ()
showGames n name = tell (\s -> s{shownGames = n, shownGameName = name}) >> withCWString (fromMaybe "" name) (c_games (fromIntegral n))

-- | The performance figures of the status line.
showStats :: String -> IO ()
showStats st = tell (\s -> s{shownStats = st}) >> withCWString st c_stats

-- | What the window says of the calibration, on a line or two.
showCalibration :: String -> IO ()
showCalibration s = withCWString s c_calibration

-- | Show a setting's value, by its name in the settings file.
showSetting :: String -> String -> IO ()
showSetting name value = withCWString name $ \n -> withCWString value (c_set_setting n)

-- | The line beside the response's heading: whether the last change was
-- saved, and if not, why.
showSettingsMessage :: String -> IO ()
showSettingsMessage s = withCWString s c_settings_message

-- | Take the named settings the user changed in the window and asked to
-- save, with their values, leaving out the rest. See Note [The window's
-- changes] in cbits/gui.c.
takeSavedSettings :: [String] -> IO [(String, String)]
takeSavedSettings names = concat <$> mapM saved names
  where
    saved name = withCWString name $ \n -> allocaArray size $ \buf -> do
        shown <- c_take_saved_setting n buf (fromIntegral size)
        if shown /= 0 then (\v -> [(name, v)]) <$> peekCWString buf else pure []
    -- cbits/gui.c's FIELD_MAX
    size = 4096

-- | A pose as cbits/gui.c is told it. See Note [Matching cbits/gui.c's
-- arithmetic] in "TrackKat.UI.Draw".
poseC :: HeadPose -> HeadPose
poseC (HeadPose a b c d e f) = HeadPose (single a) (single b) (single c) (single d) (single e) (single f)

-- | A number in single precision, as cbits/gui.c is told numbers.
single :: Double -> Double
single x = realToFrac (realToFrac x :: CFloat)
