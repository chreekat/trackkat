-- | Windows calls that can hand one of the thread's windows a message, and
-- so run a window procedure here, while they wait: imported safe, so that
-- procedure can run.
module TrackKat.Gui.Calls (
    createWindow,
    send,
    showWindow,
    moveWindow,
    pointerParam,
) where

import Control.Monad (void)
import Foreign.C.String (withCWString)
import Foreign.C.Types (CInt (..), CIntPtr (..))
import Foreign.Ptr (Ptr, nullPtr, ptrToWordPtr, wordPtrToPtr)
import Graphics.Win32.GDI.Types (HMENU, HWND)
import Graphics.Win32.Message (WindowMessage)
import System.Win32.DLL (getModuleHandle)
import System.Win32.Types (BOOL, DWORD, HINSTANCE, LPARAM, LPCTSTR, LPVOID, LRESULT, WPARAM)

foreign import ccall safe "windows.h CreateWindowExW"
    c_CreateWindowEx :: DWORD -> LPCTSTR -> LPCTSTR -> DWORD -> CInt -> CInt -> CInt -> CInt -> HWND -> HMENU -> HINSTANCE -> LPVOID -> IO HWND
foreign import ccall safe "windows.h SendMessageW" c_SendMessage :: HWND -> WindowMessage -> WPARAM -> LPARAM -> IO LRESULT
foreign import ccall safe "windows.h ShowWindow" c_ShowWindow :: HWND -> CInt -> IO BOOL
foreign import ccall safe "windows.h MoveWindow" c_MoveWindow :: HWND -> CInt -> CInt -> CInt -> CInt -> BOOL -> IO BOOL

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

-- | A pointer as a message's WPARAM or LPARAM.
pointerParam :: (Num n) => Ptr a -> n
pointerParam = fromIntegral . ptrToWordPtr
