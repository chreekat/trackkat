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
    Output (..),
    showOutput,
    showGames,
    showStats,
    showCalibration,
    showSetting,
    showSettingsMessage,
    savedSettings,
) where

import Data.Bits (shiftR, testBit, (.&.))
import Foreign.C.String (CWString, peekCWString, withCWString)
import Foreign.C.Types (CFloat (..), CInt (..))
import Foreign.Marshal.Array (allocaArray)

import TrackKat.Config (Binding, bindingFromCodes, renderBinding)
import TrackKat.Pose (HeadPose (..))
import TrackKat.Windows (Hotkey (..))

foreign import ccall unsafe "hts_gui_start" c_start :: CInt -> IO CInt
foreign import ccall unsafe "hts_gui_stop" c_stop :: IO ()
foreign import ccall unsafe "hts_gui_quit" c_quit :: IO ()
foreign import ccall unsafe "hts_gui_show_running" c_show_running :: IO CInt
foreign import ccall unsafe "hts_gui_window_open" c_window_open :: IO CInt
foreign import ccall unsafe "hts_gui_take_actions" c_take_actions :: IO CInt
foreign import ccall unsafe "hts_gui_chosen_key" c_chosen_key :: CInt -> IO CInt
foreign import ccall unsafe "hts_gui_capturing" c_capturing :: IO CInt
foreign import ccall unsafe "hts_gui_add_binding" c_add_binding :: CInt -> CWString -> IO ()
foreign import ccall unsafe "hts_gui_log" c_log :: CWString -> IO ()
foreign import ccall unsafe "hts_gui_camera" c_camera :: CInt -> CWString -> IO ()
foreign import ccall unsafe "hts_gui_frame"
    c_frame :: CInt -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> IO ()
foreign import ccall unsafe "hts_gui_paused" c_paused :: CInt -> IO ()
foreign import ccall unsafe "hts_gui_games" c_games :: CInt -> IO ()
foreign import ccall unsafe "hts_gui_stats" c_stats :: CWString -> IO ()
foreign import ccall unsafe "hts_gui_calibration" c_calibration :: CWString -> IO ()
foreign import ccall unsafe "hts_gui_settings_message" c_settings_message :: CWString -> IO ()
foreign import ccall unsafe "hts_gui_set_setting" c_set_setting :: CWString -> CWString -> IO ()
foreign import ccall unsafe "hts_gui_saved_setting" c_saved_setting :: CWString -> CWString -> CInt -> IO ()

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

-- | Whether the window is open; hidden in the tray doesn't count.
windowOpen :: IO Bool
windowOpen = (/= 0) <$> c_window_open

data Action
    = RecenterAction
    | PauseAction
    | CalibrateAction
    | QuitAction
    | -- | the Save button: read the settings with 'savedSettings'
      SaveAction
    | -- | a key pressed to add to a binding list, if it has a name
      KeyChosen Hotkey (Maybe Binding)
    deriving (Show)

-- | What the user asked for since the last call.
takeActions :: IO [Action]
takeActions = do
    bits <- c_take_actions
    keys <- sequence [chosen k | (k, b) <- [(RecenterKey, 5), (PauseKey, 6)], testBit bits b]
    pure ([a | (a, b) <- [(RecenterAction, 0), (PauseAction, 1), (CalibrateAction, 2), (QuitAction, 3), (SaveAction, 4)], testBit bits b] ++ keys)
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
        _ -> pure Nothing

-- | Add a binding to a list in the window, and stop waiting for one.
addBinding :: Hotkey -> Binding -> IO ()
addBinding k b = withCWString (renderBinding b) (c_add_binding (list k))

-- | How cbits/gui.c numbers the binding lists.
list :: Hotkey -> CInt
list RecenterKey = 0
list PauseKey = 1

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

-- | Whether games see the head move.
data Output = Live | Paused

showOutput :: Output -> IO ()
showOutput Live = c_paused 0
showOutput Paused = c_paused 1

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

-- | The named settings' values as the user last saved them.
savedSettings :: [String] -> IO [(String, String)]
savedSettings = mapM $ \name -> withCWString name $ \n -> allocaArray size $ \buf -> do
    c_saved_setting n buf (fromIntegral size)
    (,) name <$> peekCWString buf
  where
    size = 256
