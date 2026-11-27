-- | trackkat's window, with window = haskell: made, scrolled and kept to
-- its content's width here, and hidden rather than closed. cbits/gui.c
-- builds the controls not yet made in Haskell into it, and is passed the
-- messages this doesn't handle.
module TrackKat.Gui.Window (makeMain) where

import Control.Exception (bracket_)
import Control.Monad (unless, when)
import Data.Bits (complement, shiftR, (.&.))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int16)
import Foreign.C.Types (CInt (..), CIntPtr (..))
import Foreign.Ptr (Ptr, wordPtrToPtr)
import Graphics.Win32.GDI.Types (HWND)
import Graphics.Win32.Message (WindowMessage, wM_CLOSE, wM_COMMAND, wM_GETMINMAXINFO, wM_SIZE, wM_VSCROLL)
import Graphics.Win32.Misc (iDCANCEL)
import Graphics.Win32.Window (WindowClosure, adjustWindowRect, getClientRect, getWindowRect, sW_HIDE, wS_MAXIMIZEBOX, wS_OVERLAPPEDWINDOW, wS_VSCROLL)
import System.Win32.Info (sM_CXVSCROLL)
import System.Win32.Types (BOOL, INT, LPARAM, LRESULT, WPARAM)

import TrackKat.Display (Shown (..))
import TrackKat.Gui.Calls (resizeWindow, scrollContent, setScrollPos, setScrollRange, showWindow)
import TrackKat.Gui.Kit (Kit, kitDpi, scaled)
import TrackKat.Gui.Paint (ownWindow)
import TrackKat.Gui.Shown (snapshot)
import TrackKat.Gui.Win32
import TrackKat.UI.Window

-- C's, which can send the thread's windows messages: safe.
foreign import ccall safe "hts_gui_main_proc" c_main_proc :: HWND -> WindowMessage -> WPARAM -> LPARAM -> IO LRESULT
foreign import ccall safe "hts_gui_scrolled" c_scrolled :: CInt -> IO ()
foreign import ccall unsafe "windows.h GetSystemMetrics" c_GetSystemMetrics :: CInt -> IO CInt
foreign import ccall unsafe "windows.h GetScrollInfo" c_GetScrollInfo :: HWND -> CInt -> Ptr () -> IO BOOL
foreign import ccall unsafe "windows.h GetWindowLongPtrW" c_GetWindowLongPtr :: HWND -> INT -> IO CIntPtr

-- | How the window is scrolled.
data Scrolling = Scrolling
    { scrollOffset :: IORef Int
    -- ^ how far its content is scrolled up, in pixels
    , fitting :: IORef Bool
    -- ^ whether 'fit' is under way, and the window's sizing is its own
    }

-- | Make the window, hidden, named for the version shown, reporting what
-- fails in it. Left is a Windows error code.
makeMain :: Kit -> (String -> IO ()) -> IO (Either Int HWND)
makeMain kit report = do
    sc <- Scrolling <$> newIORef 0 <*> newIORef False
    version <- shownVersion <$> snapshot
    ownWindow report "The window" "trackkat_window" ("trackkat " ++ version) (wS_OVERLAPPEDWINDOW .&. complement wS_MAXIMIZEBOX) (s contentWidth, s contentHeight) (mainProc kit sc)
  where
    s = scaled kit . fromIntegral

mainProc :: Kit -> Scrolling -> WindowClosure
mainProc kit sc w msg wp lp
    -- The multipliers' arrows send it too, as themselves.
    | msg == wM_VSCROLL && lp == 0 = do
        (_, (_, _, _, dragged)) <- withScrollInfo (c_GetScrollInfo w sbVert)
        mapM_ (scrollBy kit sc w) (barScroll (wp .&. 0xFFFF) (fromIntegral dragged))
        pure 0
    | msg == wmMouseWheel = do
        scrollBy kit sc w (Wheel (fromIntegral (fromIntegral (wp `shiftR` 16) :: Int16)))
        pure 0
    -- Minimized, it has no content to fit.
    | msg == wM_SIZE && wp /= sizeMinimized = fit kit sc w >> pure 0
    | msg == wM_GETMINMAXINFO = limit kit w (wordPtrToPtr (fromIntegral lp)) >> pure 0
    | msg == wM_CLOSE = hide
    | msg == wM_COMMAND && wp .&. 0xFFFF == fromIntegral iDCANCEL = hide
    | otherwise = c_main_proc w msg wp lp
  where
    hide = showWindow w (fromIntegral sW_HIDE) >> pure 0

-- | What a scroll bar's message asks, given where its box is dragged.
barScroll :: WPARAM -> Int -> Maybe Scroll
barScroll code dragged
    | code == sbLineUp = Just LineUp
    | code == sbLineDown = Just LineDown
    | code == sbPageUp = Just PageUp
    | code == sbPageDown = Just PageDown
    | code == sbThumbTrack || code == sbThumbPosition = Just (ScrollTo dragged)
    | code == sbTop = Just ToTop
    | code == sbBottom = Just ToBottom
    | otherwise = Nothing

-- | Scroll the content, its scroll bar and what cbits/gui.c places by it.
scrollBy :: Kit -> Scrolling -> HWND -> Scroll -> IO ()
scrollBy kit sc w how = do
    (_, _, _, bottom) <- getClientRect w
    from <- readIORef (scrollOffset sc)
    let to = scrolled (kitDpi kit) (fromIntegral bottom) from how
    unless (to == from) $ do
        scrollContent w (fromIntegral (from - to))
        writeIORef (scrollOffset sc) to
        setScrollPos w (fromIntegral to)
        c_scrolled (fromIntegral to)

-- | Give the window a scroll bar if it is shorter than its content, and
-- keep the content's width beside it.
fit :: Kit -> Scrolling -> HWND -> IO ()
fit kit sc w = do
    busy <- readIORef (fitting sc)
    unless busy $ bracket_ (writeIORef (fitting sc) True) (writeIORef (fitting sc) False) $ do
        (_, _, _, bottom) <- getClientRect w
        setScrollRange w (s contentHeight - 1) (fromIntegral bottom)
        readIORef (scrollOffset sc) >>= scrollBy kit sc w . ScrollTo
        (_, _, right, _) <- getClientRect w
        when (fromIntegral right /= s contentWidth) $ do
            (l, t, r, b) <- getWindowRect w
            resizeWindow w (fromIntegral (r - l) + s contentWidth - fromIntegral right, fromIntegral (b - t))
  where
    s = scaled kit . fromIntegral

-- | Keep the window as wide as its content and scroll bar, if it has one,
-- and no taller than its content, in a MINMAXINFO.
limit :: Kit -> HWND -> Ptr () -> IO ()
limit kit w info = do
    (l, t, r, b) <- adjustWindowRect (0, 0, s contentWidth, s contentHeight) wS_OVERLAPPEDWINDOW False
    style <- c_GetWindowLongPtr w gwlStyle
    bar <- if fromIntegral style .&. wS_VSCROLL /= 0 then c_GetSystemMetrics (fromIntegral sM_CXVSCROLL) else pure 0
    let width = fromIntegral (r - l) + bar
    setMinTrackSize info width (fromIntegral (scaled kit 240))
    setMaxTrackSize info width (fromIntegral (b - t))
  where
    s = fromIntegral . scaled kit . fromIntegral
