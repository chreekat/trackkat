{-# LANGUAGE LambdaCase #-}

-- | trackkat's window and system tray icon: see cbits/gui.c. State is
-- pushed in with the @show@ functions, which never wait on the window, and
-- what the user asks for is polled with 'takeActions'.
module TrackKat.Gui (
    WindowAtStart (..),
    startGui,
    stopGui,
    quitProcess,
    showRunning,
    quitRunning,
    showVersion,
    UpdateButton (..),
    showUpdateButton,
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
    Output (..),
    showOutput,
    showTrim,
    showGames,
    showStats,
    showCalibration,
    showSetting,
    showSettingsMessage,
    savedSettings,
) where

import Data.Bits (shiftR, testBit, (.&.))
import Data.Maybe (fromMaybe)
import Foreign.C.String (CWString, peekCWString, withCWString)
import Foreign.C.Types (CFloat (..), CInt (..))
import Foreign.Marshal.Array (allocaArray, withArrayLen)
import Foreign.Ptr (Ptr)

import TrackKat.Config (Binding, bindingFromCodes, renderBinding)
import TrackKat.Frame (Blob (..))
import TrackKat.Pose (HeadPose (..))
import TrackKat.Response (Trim (..))
import TrackKat.Tracker (BlobRole (..))
import TrackKat.Windows (Hotkey (..))

foreign import ccall unsafe "hts_gui_start" c_start :: CInt -> IO CInt
foreign import ccall unsafe "hts_gui_stop" c_stop :: IO ()
foreign import ccall unsafe "hts_gui_quit" c_quit :: IO ()
foreign import ccall unsafe "hts_gui_show_running" c_show_running :: IO CInt
foreign import ccall unsafe "hts_gui_quit_running" c_quit_running :: IO CInt
foreign import ccall unsafe "hts_gui_version" c_version :: CWString -> IO ()
foreign import ccall unsafe "hts_gui_updatable" c_updatable :: CInt -> IO ()
foreign import ccall unsafe "hts_gui_offer" c_offer :: CWString -> CWString -> CWString -> IO ()
foreign import ccall unsafe "hts_gui_window_open" c_window_open :: IO CInt
foreign import ccall unsafe "hts_gui_take_actions" c_take_actions :: IO CInt
foreign import ccall unsafe "hts_gui_chosen_key" c_chosen_key :: CInt -> IO CInt
foreign import ccall unsafe "hts_gui_capturing" c_capturing :: IO CInt
foreign import ccall unsafe "hts_gui_add_binding" c_add_binding :: CInt -> CWString -> IO ()
foreign import ccall unsafe "hts_gui_log" c_log :: CWString -> IO ()
foreign import ccall unsafe "hts_gui_camera" c_camera :: CInt -> CWString -> IO ()
foreign import ccall unsafe "hts_gui_frame"
    c_frame :: CInt -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> IO ()
foreign import ccall unsafe "hts_gui_blobs" c_blobs :: CInt -> Ptr CFloat -> IO ()
foreign import ccall unsafe "hts_gui_paused" c_paused :: CInt -> IO ()
foreign import ccall unsafe "hts_gui_trim" c_trim :: CInt -> CFloat -> CFloat -> CInt -> IO ()
foreign import ccall unsafe "hts_gui_games" c_games :: CInt -> IO ()
foreign import ccall unsafe "hts_gui_stats" c_stats :: CWString -> IO ()
foreign import ccall unsafe "hts_gui_calibration" c_calibration :: CWString -> IO ()
foreign import ccall unsafe "hts_gui_settings_message" c_settings_message :: CWString -> IO ()
foreign import ccall unsafe "hts_gui_set_setting" c_set_setting :: CWString -> CWString -> IO ()
foreign import ccall unsafe "hts_gui_saved_setting" c_saved_setting :: CWString -> CWString -> CInt -> IO CInt

-- | Whether the window opens at start; the tray icon always shows.
data WindowAtStart = WindowShown | WindowHidden

-- | Start the window and tray icon on a thread of their own. Left is a
-- Windows error code.
startGui :: WindowAtStart -> IO (Either Int ())
startGui w = do
    rc <- c_start (case w of WindowShown -> 1; WindowHidden -> 0)
    pure (if rc == 0 then Right () else Left (fromIntegral rc))

-- | Remove the tray icon, which Windows otherwise leaves behind.
stopGui :: IO ()
stopGui = c_stop

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

-- | The version the window's title and log box show. Call before
-- 'startGui'.
showVersion :: String -> IO ()
showVersion v = withCWString v c_version

-- | Whether the window's Check for updates button works: only for a copy
-- that updates itself.
data UpdateButton = UpdateButtonEnabled | UpdateButtonDisabled

-- | Enable the Check for updates button, or not. Call before 'startGui'.
showUpdateButton :: UpdateButton -> IO ()
showUpdateButton UpdateButtonEnabled = c_updatable 1
showUpdateButton UpdateButtonDisabled = c_updatable 0

-- | Show a banner across the top of the window offering something, with a
-- button, and perhaps an item in the tray's menu, either of which asks for
-- it with 'OfferAction'. Nothing hides the banner.
showOffer :: Maybe (String, String, Maybe String) -> IO ()
showOffer o = withCWString text $ \t -> withCWString button $ \b -> withCWString tray (c_offer t b)
  where
    (text, button, tray) = case o of
        Just (t, b, m) -> (t, b, fromMaybe "" m)
        Nothing -> ("", "", "")

-- | Whether the window is open; hidden in the tray doesn't count.
windowOpen :: IO Bool
windowOpen = (/= 0) <$> c_window_open

data Action
    = RecenterAction
    | PauseAction
    | TrimAction
    | CalibrateAction
    | -- | forget the learnt calibration and track afresh
      ClearCalibrationAction
    | QuitAction
    | -- | the banner's button, or its item in the tray's menu
      OfferAction
    | -- | look for a newer version now
      CheckForUpdatesAction
    | -- | the Save button: read the settings with 'savedSettings'
      SaveAction
    | -- | a key pressed to add to a binding list, if it has a name
      KeyChosen Hotkey (Maybe Binding)
    deriving (Eq, Show)

-- | What the user asked for since the last call.
takeActions :: IO [Action]
takeActions = do
    bits <- c_take_actions
    keys <- sequence [chosen k | (k, b) <- [(RecenterKey, 5), (PauseKey, 6), (TrimKey, 8)], testBit bits b]
    pure ([a | (a, b) <- [(RecenterAction, 0), (PauseAction, 1), (CalibrateAction, 2), (QuitAction, 3), (SaveAction, 4), (TrimAction, 7), (ClearCalibrationAction, 9), (OfferAction, 10), (CheckForUpdatesAction, 11)], testBit bits b] ++ keys)
  where
    chosen k = do
        code <- fromIntegral <$> c_chosen_key (list k)
        pure (KeyChosen k (bindingFromCodes (code `shiftR` 16) (code .&. 0xFFFF)))

-- | The binding list waiting for a key or joystick button to add, if any.
capturing :: IO (Maybe Hotkey)
capturing =
    c_capturing >>= \case
        0 -> pure (Just RecenterKey)
        1 -> pure (Just PauseKey)
        2 -> pure (Just TrimKey)
        _ -> pure Nothing

-- | Add a binding to a list in the window, and stop waiting for one.
addBinding :: Hotkey -> Binding -> IO ()
addBinding k b = withCWString (renderBinding b) (c_add_binding (list k))

-- | How cbits/gui.c numbers the binding lists.
list :: Hotkey -> CInt
list RecenterKey = 0
list PauseKey = 1
list TrimKey = 2

-- | Add a line to the log the window shows.
guiLog :: String -> IO ()
guiLog line = withCWString line c_log

data CameraState
    = Searching
    | Tracking
    | -- | why not
      CameraDown String

showCamera :: CameraState -> IO ()
showCamera Searching = withCWString "" (c_camera 0)
showCamera Tracking = withCWString "" (c_camera 1)
showCamera (CameraDown why) = withCWString why (c_camera 2)

-- | What a frame showed of the clip.
data Sighting
    = InSight HeadPose
    | -- | how many reflectors were seen
      OutOfSight Int

-- | Show a frame: the head, and the pose games are sent. Skipped if the
-- window is busy copying the last one.
showFrame :: Sighting -> HeadPose -> IO ()
showFrame sighting view =
    c_frame blobs (f yaw) (f pitch) (f roll) (f tx) (f ty) (f tz) (g yaw) (g pitch) (g roll) (g tx) (g ty) (g tz)
  where
    (blobs, head') = case sighting of
        InSight hp -> (-1, hp)
        OutOfSight n -> (fromIntegral n, view)
    f axis = realToFrac (axis head')
    g axis = realToFrac (axis view)

-- | Show a frame's blobs, heaviest first, at their places in the camera's
-- image, and what each was taken for; the window shows the first few.
-- Skipped if the window is busy copying the last ones.
showBlobs :: [(Blob, BlobRole)] -> IO ()
showBlobs blobs = withArrayLen (concatMap spot (take shownBlobs blobs)) $ \n -> c_blobs (fromIntegral (n `div` 4))
  where
    -- As BLOBS_MAX in cbits/gui.c.
    shownBlobs = 16
    spot (b, role) = map realToFrac [blobX b, blobY b, fromIntegral (blobPoints b)] ++ [roleCode role]
    -- As SPOT_REFLECTOR and the rest in cbits/gui.c.
    roleCode ReflectorBlob = 0
    roleCode StrayBlob = 1
    roleCode MaskedBlob = 2

-- | Whether games see the head move.
data Output = Live | Paused

showOutput :: Output -> IO ()
showOutput Live = c_paused 0
showOutput Paused = c_paused 1

-- | Where forward is for the view.
showTrim :: Trim -> IO ()
showTrim NoTrim = c_trim 0 0 0 0
showTrim (Trim t) = c_trim 1 (realToFrac (yaw t)) (realToFrac (pitch t)) (if movesToo then 1 else 0)
  where
    -- Beyond turning, enough to notice.
    movesToo = abs (roll t) >= 1 || any ((>= 5) . abs) [tx t, ty t, tz t]

-- | How many games are tracking.
showGames :: Int -> IO ()
showGames = c_games . fromIntegral

-- | The performance figures of the status line.
showStats :: String -> IO ()
showStats s = withCWString s c_stats

-- | What the calibration box says, perhaps over several lines.
showCalibration :: String -> IO ()
showCalibration s = withCWString s c_calibration

-- | Show a setting's value, by its name in the settings file.
showSetting :: String -> String -> IO ()
showSetting name value = withCWString name $ \n -> withCWString value (c_set_setting n)

-- | The line beside the Save button.
showSettingsMessage :: String -> IO ()
showSettingsMessage s = withCWString s c_settings_message

-- | The named settings' values as the user last saved them, leaving out
-- those the window doesn't show.
savedSettings :: [String] -> IO [(String, String)]
savedSettings names = concat <$> mapM saved names
  where
    saved name = withCWString name $ \n -> allocaArray size $ \buf -> do
        shown <- c_saved_setting n buf (fromIntegral size)
        if shown /= 0 then (\v -> [(name, v)]) <$> peekCWString buf else pure []
    size = 256
