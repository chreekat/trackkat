-- | trackkat's system tray icon: see cbits/tray.c. It shows the status as
-- 'TrackKat.Icon' draws it, and its menu's choices are polled with
-- 'takeChoices'.
module TrackKat.Tray (
    startTray,
    stopTray,
    quitProcess,
    showRunning,
    showStatus,
    Choice (..),
    takeChoices,
) where

import Data.Bits (testBit)
import qualified Data.ByteString as B
import Foreign.C.String (CWString, withCWString)
import Foreign.C.Types (CInt (..), CUChar)
import Foreign.Ptr (Ptr, castPtr)

import TrackKat.Icon
import TrackKat.Status

foreign import ccall unsafe "hts_tray_start" c_start :: IO CInt
foreign import ccall unsafe "hts_tray_stop" c_stop :: IO ()
foreign import ccall unsafe "hts_tray_quit" c_quit :: IO ()
foreign import ccall unsafe "hts_tray_show_running" c_show_running :: IO CInt
foreign import ccall unsafe "hts_tray_icon_size" c_icon_size :: IO CInt
foreign import ccall unsafe "hts_tray_show" c_show :: Ptr CUChar -> CInt -> CWString -> CInt -> IO ()
foreign import ccall unsafe "hts_tray_take" c_take :: IO CInt

-- | Start the tray icon on a thread of its own. Left is a Windows error
-- code.
startTray :: IO (Either Int ())
startTray = (\rc -> if rc == 0 then Right () else Left (fromIntegral rc)) <$> c_start

-- | Remove the tray icon, which Windows otherwise leaves behind.
stopTray :: IO ()
stopTray = c_stop

-- | Remove the tray icon and end the process at once, whatever other
-- threads are doing.
quitProcess :: IO a
quitProcess = c_quit >> error "quitProcess returned"

-- | Ask a trackkat already running to open its window; False if none is.
showRunning :: IO Bool
showRunning = (/= 0) <$> c_show_running

-- | Show a status in the tray: its icon, its tooltip, and whether Pause is
-- ticked in the menu.
showStatus :: Status -> IO ()
showStatus st = do
    size <- fromIntegral <$> c_icon_size
    B.useAsCString (iconPixels size (statusIcon st)) $ \pixels ->
        withCWString (statusTip st) $ \tip ->
            c_show (castPtr pixels) (fromIntegral size) tip (case stOutput st of Paused -> 1; Live -> 0)

-- | What can be chosen from the tray icon: clicking it opens the window.
data Choice = OpenChoice | RecenterChoice | PauseChoice | TrimChoice | QuitChoice
    deriving (Eq, Show)

-- | What was chosen since the last call.
takeChoices :: IO [Choice]
takeChoices = do
    bits <- c_take
    pure [c | (c, b) <- [(OpenChoice, 0), (RecenterChoice, 1), (PauseChoice, 2), (TrimChoice, 3), (QuitChoice, 4)], testBit bits b]
