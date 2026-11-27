-- | Windows calls that can hand one of the thread's windows a message, and
-- so run a window procedure here, while they wait: imported safe, so that
-- procedure can run.
module TrackKat.Gui.Calls (
    createWindow,
    send,
    showWindow,
    moveWindow,
    resizeWindow,
    scrollContent,
    setScrollRange,
    setScrollPos,
    raiseWindow,
    pointerParam,
) where

import Control.Monad (void)
import Data.Bits ((.|.))
import Foreign.C.String (withCWString)
import Foreign.C.Types (CInt (..), CIntPtr (..))
import Foreign.Ptr (Ptr, nullPtr, ptrToWordPtr, wordPtrToPtr)
import Graphics.Win32.GDI.Types (HMENU, HWND)
import Graphics.Win32.Message (WindowMessage)
import Graphics.Win32.Window (sWP_NOACTIVATE, sWP_NOMOVE, sWP_NOZORDER, sW_RESTORE, sW_SHOW)
import System.Win32.DLL (getModuleHandle)
import System.Win32.Types (BOOL, DWORD, HINSTANCE, LPARAM, LPCTSTR, LPVOID, LRESULT, UINT, WPARAM)

import TrackKat.Gui.Win32 (sbVert, swErase, swInvalidate, swScrollChildren, withScrollRange)

foreign import ccall safe "windows.h CreateWindowExW"
    c_CreateWindowEx :: DWORD -> LPCTSTR -> LPCTSTR -> DWORD -> CInt -> CInt -> CInt -> CInt -> HWND -> HMENU -> HINSTANCE -> LPVOID -> IO HWND
foreign import ccall safe "windows.h SendMessageW" c_SendMessage :: HWND -> WindowMessage -> WPARAM -> LPARAM -> IO LRESULT
foreign import ccall safe "windows.h ShowWindow" c_ShowWindow :: HWND -> CInt -> IO BOOL
foreign import ccall safe "windows.h MoveWindow" c_MoveWindow :: HWND -> CInt -> CInt -> CInt -> CInt -> BOOL -> IO BOOL
foreign import ccall safe "windows.h SetWindowPos" c_SetWindowPos :: HWND -> HWND -> CInt -> CInt -> CInt -> CInt -> UINT -> IO BOOL
foreign import ccall safe "windows.h ScrollWindowEx"
    c_ScrollWindowEx :: HWND -> CInt -> CInt -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> UINT -> IO CInt
foreign import ccall safe "windows.h SetScrollInfo" c_SetScrollInfo :: HWND -> CInt -> Ptr () -> BOOL -> IO CInt
foreign import ccall safe "windows.h SetScrollPos" c_SetScrollPos :: HWND -> CInt -> CInt -> BOOL -> IO CInt
foreign import ccall safe "windows.h SetForegroundWindow" c_SetForegroundWindow :: HWND -> IO BOOL

-- | A window of an extended style, class, text and style, at x, y, w, h,
-- with a parent or owner (or none) and a child's ID; null if it couldn't be
-- made.
createWindow :: DWORD -> String -> String -> DWORD -> (CInt, CInt, CInt, CInt) -> Maybe HWND -> Int -> IO HWND
createWindow ex cls text style (x, y, w, h) parent ident = do
    inst <- getModuleHandle Nothing
    withCWString cls $ \c -> withCWString text $ \t ->
        c_CreateWindowEx ex c t style x y w h (maybe nullPtr id parent) (wordPtrToPtr (fromIntegral ident)) inst nullPtr

-- | Send a window a message, waiting for its answer.
send :: HWND -> WindowMessage -> WPARAM -> LPARAM -> IO LRESULT
send = c_SendMessage

showWindow :: HWND -> CInt -> IO ()
showWindow w how = void (c_ShowWindow w how)

-- | Move a window to x, y, w, h, and repaint it.
moveWindow :: HWND -> (CInt, CInt, CInt, CInt) -> IO ()
moveWindow wnd (x, y, w, h) = void (c_MoveWindow wnd x y w h True)

-- | Make a window a width and height, leaving where it is and what is in
-- front of it.
resizeWindow :: HWND -> (CInt, CInt) -> IO ()
resizeWindow wnd (w, h) = void (c_SetWindowPos wnd nullPtr 0 0 w h (sWP_NOMOVE .|. sWP_NOZORDER .|. sWP_NOACTIVATE))

-- | Scroll a window's content and children down some pixels (up, if
-- negative), repainting what is uncovered.
scrollContent :: HWND -> CInt -> IO ()
scrollContent wnd dy = void (c_ScrollWindowEx wnd 0 dy nullPtr nullPtr nullPtr nullPtr (swScrollChildren .|. swInvalidate .|. swErase))

-- | Set a window's vertical scroll bar from 0 to a largest position, with
-- a page.
setScrollRange :: HWND -> CInt -> UINT -> IO ()
setScrollRange wnd top page = withScrollRange top page $ \info -> void (c_SetScrollInfo wnd sbVert info True)

-- | Move a window's vertical scroll bar's box.
setScrollPos :: HWND -> CInt -> IO ()
setScrollPos wnd pos = void (c_SetScrollPos wnd sbVert pos True)

-- | Show a window, restored if it was minimized, in front of the rest.
raiseWindow :: HWND -> IO ()
raiseWindow wnd = do
    minimized <- c_IsIconic wnd
    showWindow wnd (fromIntegral (if minimized then sW_RESTORE else sW_SHOW))
    void (c_SetForegroundWindow wnd)

foreign import ccall unsafe "windows.h IsIconic" c_IsIconic :: HWND -> IO BOOL

-- | A pointer as a message's WPARAM or LPARAM.
pointerParam :: (Num n) => Ptr a -> n
pointerParam = fromIntegral . ptrToWordPtr
