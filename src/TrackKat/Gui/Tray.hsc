{-# LANGUAGE LambdaCase #-}

-- | trackkat's system tray icon, with window = haskell: the hidden window it
-- talks to, its look and tooltip ("TrackKat.Tray"), and its menu. The window
-- it opens is cbits/gui.c's.
module TrackKat.Gui.Tray (makeTray) where

#include <windows.h>
#include <shellapi.h>

import Control.Monad (forM_, unless, void, when)
import Data.Bits ((.&.), (.|.))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Foreign.C.String (CWString, peekCWString, withCWString, withCWStringLen)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (allocaArray, copyArray)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr, wordPtrToPtr)
import Foreign.Storable (peek, pokeByteOff)
import Graphics.Win32.GDI.Types (HICON, HMENU, HWND, LPRECT)
import Graphics.Win32.Menu (appendMenu, createPopupMenu, destroyMenu, setMenuDefaultItem)
import Graphics.Win32.Message (WindowMessage, wM_APP, wM_COMMAND, wM_LBUTTONDBLCLK, wM_LBUTTONUP, wM_RBUTTONUP, wM_TIMER)
import Graphics.Win32.Misc (getCursorPos)
import Graphics.Win32.Window
    ( ClassName
    , WindowClosure
    , defWindowProc
    , mkClassName
    , registerClass
    , setWindowClosure
    , wS_OVERLAPPED
    )
import Graphics.Win32.Window.ForegroundWindow (setForegroundWindow)
import Graphics.Win32.Window.PostMessage (postMessage)
import System.Win32.DLL (getModuleHandle)
import System.Win32.Types (BOOL, DWORD, HINSTANCE, LPCTSTR, LPVOID, UINT, getLastError)

import TrackKat.Display (CameraState (..))
import TrackKat.Tray

foreign import ccall safe "hts_gui_refresh" c_refresh :: IO ()
foreign import ccall safe "hts_gui_open_settings_now" c_open_settings_now :: IO ()
foreign import ccall unsafe "hts_gui_stopping" c_stopping :: IO CInt
foreign import ccall unsafe "hts_gui_tray_state"
    c_tray_state :: Ptr CInt -> CWString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> CWString -> IO ()

-- Calls that may hand the thread's windows messages, and so the window
-- procedure here, while they wait: safe.
foreign import ccall safe "windows.h CreateWindowExW"
    c_CreateWindowEx :: DWORD -> ClassName -> LPCTSTR -> DWORD -> CInt -> CInt -> CInt -> CInt -> HWND -> HMENU -> HINSTANCE -> LPVOID -> IO HWND
foreign import ccall safe "shellapi.h Shell_NotifyIconW" c_Shell_NotifyIcon :: DWORD -> Ptr () -> IO BOOL
foreign import ccall safe "windows.h TrackPopupMenu"
    c_TrackPopupMenu :: HMENU -> UINT -> CInt -> CInt -> CInt -> HWND -> LPRECT -> IO BOOL
foreign import ccall unsafe "windows.h LoadImageW"
    c_LoadImage :: HINSTANCE -> LPCTSTR -> UINT -> CInt -> CInt -> UINT -> IO HICON
foreign import ccall unsafe "windows.h LoadIconW" c_LoadIcon :: HINSTANCE -> LPCTSTR -> IO HICON
foreign import ccall unsafe "windows.h GetSystemMetrics" c_GetSystemMetrics :: CInt -> IO CInt
foreign import ccall unsafe "windows.h RegisterWindowMessageW" c_RegisterWindowMessage :: LPCTSTR -> IO WindowMessage

-- As cbits/gui.c's WM_APP_ messages, which other trackkats send too.
wmAppTray, wmAppShow, wmAppQuit, wmAppOpenSettings :: WindowMessage
wmAppTray = wM_APP + 1
wmAppShow = wM_APP + 2
wmAppQuit = wM_APP + 4
wmAppOpenSettings = wM_APP + 5

-- | What the icon was last shown as, if it is shown.
data Shown = NotShown | ShownAs TrayLook String
    deriving (Eq)

-- | Make the hidden window the tray icon talks to, for cbits/gui.c's
-- hts_gui_create, which times it; it is told what the user chose from the
-- icon or its menu, or another trackkat asked for. Left is a Windows error
-- code.
makeTray :: (TrayChoice -> IO ()) -> IO (Either Int HWND)
makeTray choose = do
    inst <- getModuleHandle Nothing
    registered <- registerClass (0, inst, Nothing, Nothing, Nothing, Nothing, className)
    case registered of
        Nothing -> Left . fromIntegral <$> getLastError
        Just _ -> do
            -- Hidden, but top-level, so a second trackkat can find it and
            -- Explorer's TaskbarCreated reaches it.
            w <- withCWString "trackkat" $ \title ->
                c_CreateWindowEx 0 className title wS_OVERLAPPED 0 0 0 0 nullPtr nullPtr inst nullPtr
            if w == nullPtr
                then Left . fromIntegral <$> getLastError
                else do
                    taskbarCreated <- withCWString "TaskbarCreated" c_RegisterWindowMessage
                    shown <- newIORef NotShown
                    void (setWindowClosure w (trayProc choose taskbarCreated shown))
                    pure (Right w)
  where
    -- As cbits/gui.c's hts_gui_quit_running and hts_gui_show_running find it.
    className = mkClassName "trackkat"

trayProc :: (TrayChoice -> IO ()) -> WindowMessage -> IORef Shown -> WindowClosure
trayProc choose taskbarCreated shown w msg wp lp
    | msg == taskbarCreated && taskbarCreated /= 0 = do
        -- Explorer restarted, and forgot the icon.
        writeIORef shown NotShown
        update
        pure 0
    | msg == wM_TIMER = c_refresh >> update >> pure 0
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
    wmContextMenu = 0x007B
    update = do
        stopping <- c_stopping
        when (stopping == 0) $ do
            s <- trayState
            was <- readIORef shown
            let now = ShownAs (trayLook s) (trayTip s)
            unless (now == was) $ do
                icon <- trayIcon (trayLook s)
                -- Modified if it is there; added if not, or no longer.
                ok <- if was == NotShown then pure False else notify nimModify icon (trayTip s)
                added <- if ok then pure True else notify nimAdd icon (trayTip s)
                writeIORef shown (if added then now else NotShown)
    menu = do
        s <- trayState
        m <- createPopupMenu
        forM_ (trayMenu s) $ \case
            MenuItem choice name tick ->
                appendMenu m (#{const MF_STRING} .|. (case tick of Ticked -> #{const MF_CHECKED}; Unticked -> 0)) (itemId choice) (Just name)
            MenuSeparator -> appendMenu m #{const MF_SEPARATOR} 0 Nothing
        setMenuDefaultItem m (itemId OpenChoice) False
        (x, y) <- getCursorPos
        -- Without these, the menu doesn't close when clicking elsewhere.
        void (setForegroundWindow w)
        void (c_TrackPopupMenu m #{const TPM_RIGHTBUTTON} (fromIntegral x) (fromIntegral y) 0 w nullPtr)
        postMessage (Just w) 0 0 0
        destroyMenu m
    notify message icon tip = notifyIcon message w icon tip
    nimAdd = #{const NIM_ADD}
    nimModify = #{const NIM_MODIFY}

-- | The menu's items' IDs, from this on in the order of 'TrayChoice'.
firstItem :: Int
firstItem = 300

itemId :: TrayChoice -> UINT
itemId c = fromIntegral (firstItem + fromEnum c)

-- | What cbits/gui.c last saw of trackkat.
trayState :: IO TrayState
trayState =
    alloca $ \camera -> allocaArray textMax $ \cameraText -> alloca $ \clipLost -> alloca $ \games ->
        alloca $ \paused -> allocaArray tipMax $ \offer -> do
            c_tray_state camera cameraText clipLost games paused offer
            cam <- peek camera
            why <- peekCWString cameraText
            lost <- peek clipLost
            n <- peek games
            p <- peek paused
            item <- peekCWString offer
            pure
                TrayState
                    { -- As hts_gui_camera's numbering.
                      trayCamera = case cam of 0 -> Searching; 1 -> Tracking; _ -> CameraDown why
                    , trayClip = if lost /= 0 then ClipOutOfSight else ClipInSight
                    , trayGames = fromIntegral n
                    , trayHolding = if p /= 0 then ViewHeld else ViewLive
                    , trayOffer = if null item then Nothing else Just item
                    }
  where
    -- As TEXT_MAX in cbits/gui.c.
    textMax = 512

-- | NOTIFYICONDATAW's szTip, in characters with the NUL.
tipMax :: Int
tipMax = 128

-- | The look's icon from the program's resources (icons/trackkat.rc, from 2
-- in the order of 'TrayLook'), at the size the tray shows, or Windows' own
-- if the build left it out. Shared: never destroyed.
trayIcon :: TrayLook -> IO HICON
trayIcon look = do
    inst <- getModuleHandle Nothing
    size <- c_GetSystemMetrics #{const SM_CXSMICON}
    icon <- c_LoadImage inst (resource (2 + fromEnum look)) #{const IMAGE_ICON} size size #{const LR_SHARED}
    if icon /= nullPtr then pure icon else c_LoadIcon nullPtr (resource idiApplication)
  where
    resource :: Int -> LPCTSTR
    resource = wordPtrToPtr . fromIntegral
    -- IDI_APPLICATION, a resource number cast to a pointer.
    idiApplication = 32512

-- | Add or modify the window's icon, uID 1, with a tooltip.
notifyIcon :: DWORD -> HWND -> HICON -> String -> IO BOOL
notifyIcon message w icon tip =
    allocaBytes size $ \p -> do
        fillBytes p 0 size
        #{poke NOTIFYICONDATAW, cbSize} p (fromIntegral size :: DWORD)
        #{poke NOTIFYICONDATAW, hWnd} p w
        #{poke NOTIFYICONDATAW, uID} p (1 :: UINT)
        #{poke NOTIFYICONDATAW, uFlags} p (#{const NIF_ICON | NIF_TIP | NIF_MESSAGE} :: UINT)
        #{poke NOTIFYICONDATAW, uCallbackMessage} p wmAppTray
        #{poke NOTIFYICONDATAW, hIcon} p icon
        withCWStringLen tip $ \(s, n) ->
            copyArray (castPtr (p `plusPtr` #{offset NOTIFYICONDATAW, szTip})) s (min n (tipMax - 1))
        c_Shell_NotifyIcon message (castPtr p)
  where
    size = #{size NOTIFYICONDATAW}
