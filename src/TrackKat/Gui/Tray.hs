{-# LANGUAGE LambdaCase #-}

-- | trackkat's system tray icon, with window = haskell: the hidden window it
-- talks to, its look and tooltip ("TrackKat.UI.Tray"), and its menu.
module TrackKat.Gui.Tray (makeTray, startTray) where

import Control.Exception (bracket)
import Control.Monad (forM_, unless, void, when)
import Data.Bits ((.&.), (.|.))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Foreign.C.String (withCWString)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr, nullPtr)
import Graphics.Win32.GDI.Types (HICON, HMENU, HWND, LPRECT)
import Graphics.Win32.Menu (appendMenu, createPopupMenu, destroyMenu, mFT_SEPARATOR, mFT_STRING, mF_CHECKED, setMenuDefaultItem, tPM_RIGHTBUTTON)
import Graphics.Win32.Message (WindowMessage, wM_APP, wM_COMMAND, wM_LBUTTONDBLCLK, wM_LBUTTONUP, wM_RBUTTONUP, wM_TIMER)
import Graphics.Win32.Misc (getCursorPos)
import Graphics.Win32.Window (
    WindowClosure,
    defWindowProc,
    mkClassName,
    registerClass,
    setWindowClosure,
    wS_OVERLAPPED,
 )
import Graphics.Win32.Window.ForegroundWindow (setForegroundWindow)
import Graphics.Win32.Window.PostMessage (postMessage)
import System.Win32.DLL (getModuleHandle)
import System.Win32.Info (sM_CXSMICON)
import System.Win32.Types (BOOL, DWORD, LPCTSTR, UINT, WPARAM, getLastError)

import TrackKat.Gui.Calls (createWindow)
import TrackKat.Gui.Paint (guarded, resourceIcon)
import TrackKat.Gui.Shown (snapshot)
import TrackKat.Gui.Win32 (nifIcon, nifMessage, nifTip, nimAdd, nimDelete, nimModify, withNotifyIconData, wmContextMenu)
import TrackKat.UI.Tray

foreign import ccall safe "hts_gui_open_settings_now" c_open_settings_now :: IO ()
foreign import ccall unsafe "hts_gui_stopping" c_stopping :: IO CInt

-- Calls that may hand the thread's windows messages, and so the window
-- procedure here, while they wait: safe.
foreign import ccall safe "shellapi.h Shell_NotifyIconW" c_Shell_NotifyIcon :: DWORD -> Ptr () -> IO BOOL
foreign import ccall safe "windows.h TrackPopupMenu"
    c_TrackPopupMenu :: HMENU -> UINT -> CInt -> CInt -> CInt -> HWND -> LPRECT -> IO BOOL
foreign import ccall unsafe "windows.h GetSystemMetrics" c_GetSystemMetrics :: CInt -> IO CInt
foreign import ccall unsafe "windows.h SetTimer" c_SetTimer :: HWND -> WPARAM -> UINT -> Ptr () -> IO WPARAM
foreign import ccall unsafe "windows.h RegisterWindowMessageW" c_RegisterWindowMessage :: LPCTSTR -> IO WindowMessage

-- As cbits/gui.c's WM_APP_ messages, which other trackkats send too.
wmAppTray, wmAppShow, wmAppQuit, wmAppOpenSettings :: WindowMessage
wmAppTray = wM_APP + 1
wmAppShow = wM_APP + 2
wmAppQuit = wM_APP + 4
wmAppOpenSettings = wM_APP + 5

-- | What the icon was last shown as, if it is shown.
data Showing = NotShown | ShownAs TrayLook String
    deriving (Eq)

-- | Make the hidden window the tray icon talks to; it is told what the user
-- chose from the icon or its menu, or another trackkat asked for, brings
-- the window up to date with the last function given before the icon, and
-- reports what failed with the first. Left is a Windows error code.
makeTray :: (String -> IO ()) -> (TrayChoice -> IO ()) -> IO () -> IO (Either Int HWND)
makeTray report choose tick = do
    inst <- getModuleHandle Nothing
    registered <- registerClass (0, inst, Nothing, Nothing, Nothing, Nothing, className)
    case registered of
        Nothing -> Left . fromIntegral <$> getLastError
        Just _ -> do
            -- Hidden, but top-level, so a second trackkat can find it and
            -- Explorer's TaskbarCreated reaches it.
            w <- createWindow 0 "trackkat" "trackkat" wS_OVERLAPPED (0, 0, 0, 0) Nothing 0
            if w == nullPtr
                then Left . fromIntegral <$> getLastError
                else do
                    taskbarCreated <- withCWString "TaskbarCreated" c_RegisterWindowMessage
                    shown <- newIORef NotShown
                    void (setWindowClosure w (trayProc report choose tick taskbarCreated shown))
                    pure (Right w)
  where
    -- As cbits/gui.c's hts_gui_quit_running and hts_gui_show_running find it.
    className = mkClassName "trackkat"

-- | Show the icon, and keep it and the window up to date, as often as the
-- window's live view moves.
startTray :: HWND -> IO ()
startTray w = void (c_SetTimer w 1 33 nullPtr)

trayProc :: (String -> IO ()) -> (TrayChoice -> IO ()) -> IO () -> WindowMessage -> IORef Showing -> WindowClosure
trayProc report choose tick taskbarCreated shown = guarded report "The tray icon" (respond choose tick taskbarCreated shown)

respond :: (TrayChoice -> IO ()) -> IO () -> WindowMessage -> IORef Showing -> WindowClosure
respond choose tick taskbarCreated shown w msg wp lp
    | msg == taskbarCreated && taskbarCreated /= 0 = do
        -- Explorer restarted, and forgot the icon.
        writeIORef shown NotShown
        update
        pure 0
    | msg == wM_TIMER = tick >> update >> pure 0
    | msg == wmAppTray = do
        let mouse = fromIntegral lp
        if mouse == wM_LBUTTONUP || mouse == wM_LBUTTONDBLCLK
            then choose OpenChoice
            else when (mouse == wM_RBUTTONUP || mouse == wmContextMenu) menu
        pure 0
    | msg == wmAppShow = choose OpenChoice >> pure 0
    | msg == wmAppQuit = choose QuitChoice >> pure 0
    | msg == wmAppOpenSettings = c_open_settings_now >> pure 0
    | msg == wM_COMMAND = do
        let item = fromIntegral (wp .&. 0xFFFF) - firstItem
        when (item >= 0 && item <= fromEnum (maxBound :: TrayChoice)) (choose (toEnum item))
        pure 0
    | otherwise = defWindowProc (Just w) msg wp lp
  where
    update = do
        stopping <- c_stopping
        when (stopping == 0) $ do
            s <- snapshot
            was <- readIORef shown
            let now = ShownAs (trayLook s) (trayTip s)
            unless (now == was) $ do
                icon <- trayIcon (trayLook s)
                -- Modified if it is there; added if not, or no longer.
                ok <- if was == NotShown then pure False else notify nimModify icon (trayTip s)
                added <-
                    if ok
                        then pure True
                        else do
                            new <- notify nimAdd icon (trayTip s)
                            -- Taken back as in cbits/gui.c's update_tray.
                            stopped <- c_stopping
                            when (new && stopped /= 0) $ void (notify nimDelete icon "")
                            pure new
                writeIORef shown (if added then now else NotShown)
    menu = do
        s <- snapshot
        bracket createPopupMenu destroyMenu $ \m -> do
            forM_ (trayMenu s) $ \case
                MenuItem choice name ticked ->
                    appendMenu m (mFT_STRING .|. (case ticked of Ticked -> mF_CHECKED; Unticked -> 0)) (itemId choice) (Just name)
                MenuSeparator -> appendMenu m mFT_SEPARATOR 0 Nothing
            setMenuDefaultItem m (itemId OpenChoice) False
            (x, y) <- getCursorPos
            -- Without these, the menu doesn't close when clicking elsewhere.
            void (setForegroundWindow w)
            void (c_TrackPopupMenu m tPM_RIGHTBUTTON (fromIntegral x) (fromIntegral y) 0 w nullPtr)
            postMessage (Just w) 0 0 0
    notify message icon tip = notifyIcon message w icon tip

-- | The menu's items' IDs, from this on in the order of 'TrayChoice'.
firstItem :: Int
firstItem = 300

itemId :: TrayChoice -> UINT
itemId c = fromIntegral (firstItem + fromEnum c)

-- | The look's icon (icons/trackkat.rc numbers them from 2 in the order of
-- 'TrayLook'), at the size the tray shows.
trayIcon :: TrayLook -> IO HICON
trayIcon look = c_GetSystemMetrics (fromIntegral sM_CXSMICON) >>= resourceIcon (2 + fromEnum look)

-- | Add, modify or delete the window's icon, uID 1, with a tooltip.
notifyIcon :: DWORD -> HWND -> HICON -> String -> IO BOOL
notifyIcon message w icon tip =
    withNotifyIconData w 1 (nifIcon .|. nifTip .|. nifMessage) wmAppTray icon tip (c_Shell_NotifyIcon message)
