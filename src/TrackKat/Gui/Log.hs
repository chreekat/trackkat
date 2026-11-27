{-# LANGUAGE LambdaCase #-}

-- | The log's own window, with window = haskell: the whole log, and
-- trackkat's version, to copy into a bug report. cbits/gui.c's Open log
-- shows it.
module TrackKat.Gui.Log (registerLog, makeLog) where

import Control.Monad (unless, void)
import Data.Bits (shiftR, (.&.), (.|.))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Foreign.C.String (CWString, withCWString)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr, nullPtr, wordPtrToPtr)
import Foreign.Storable (peek)
import Graphics.Win32.Control (bS_PUSHBUTTON, eS_AUTOVSCROLL, eS_MULTILINE, eS_READONLY, sS_LEFT, sS_NOPREFIX)
import Graphics.Win32.GDI.Clip (cF_UNICODETEXT)
import Graphics.Win32.GDI.Types (HWND, LPRECT)
import Graphics.Win32.Message (WindowMessage, wM_APP, wM_CLOSE, wM_COMMAND, wM_CREATE, wM_GETMINMAXINFO, wM_SETFONT, wM_SETREDRAW, wM_SIZE, wM_TIMER)
import Graphics.Win32.Misc (iDCANCEL)
import Graphics.Win32.Window (WindowClosure, defWindowProc, getClientRect, sW_HIDE, wS_CHILD, wS_CLIPSIBLINGS, wS_EX_CLIENTEDGE, wS_OVERLAPPEDWINDOW, wS_TABSTOP, wS_VISIBLE, wS_VSCROLL)
import System.Win32.Info (sM_CXICON)
import System.Win32.Mem (gMEM_MOVEABLE)
import System.Win32.Types (BOOL, DWORD, HANDLE, UINT, WPARAM)

import TrackKat.Display (Shown (..))
import TrackKat.Gui.Calls (createWindow, moveWindow, pointerParam, send, showWindow)
import TrackKat.Gui.Kit (Kit, fontHandle, scaled)
import TrackKat.Gui.Paint (Colour (..), Font (..), registerWindow, solidBrush, topWindow)
import TrackKat.Gui.Shown (logBook, snapshot)
import TrackKat.Gui.Tips (makeTips, tip)
import TrackKat.Gui.Win32
import TrackKat.UI.Log (Mark, logMax, since, start)

foreign import ccall unsafe "windows.h GetSystemMetrics" c_GetSystemMetrics :: CInt -> IO CInt
foreign import ccall unsafe "windows.h SetTimer" c_SetTimer :: HWND -> WPARAM -> UINT -> Ptr () -> IO WPARAM
foreign import ccall unsafe "windows.h SetBkColor" c_SetBkColor :: Ptr () -> DWORD -> IO DWORD
foreign import ccall unsafe "windows.h GetScrollInfo" c_GetScrollInfo :: HWND -> CInt -> Ptr () -> IO BOOL
foreign import ccall unsafe "windows.h InvalidateRect" c_InvalidateRect :: HWND -> LPRECT -> BOOL -> IO BOOL

-- Win32's GlobalUnlock takes the lock count reaching zero for a failure.
foreign import ccall unsafe "windows.h GlobalAlloc" c_GlobalAlloc :: UINT -> CSize -> IO HANDLE
foreign import ccall unsafe "windows.h GlobalLock" c_GlobalLock :: HANDLE -> IO (Ptr ())
foreign import ccall unsafe "windows.h GlobalUnlock" c_GlobalUnlock :: HANDLE -> IO BOOL
foreign import ccall unsafe "windows.h GlobalFree" c_GlobalFree :: HANDLE -> IO HANDLE

-- Calls that send a window a message, so safe as in "TrackKat.Gui.Calls".
foreign import ccall safe "windows.h GetWindowTextLengthW" c_GetWindowTextLength :: HWND -> IO CInt
foreign import ccall safe "windows.h GetWindowTextW" c_GetWindowText :: HWND -> CWString -> CInt -> IO CInt
foreign import ccall safe "windows.h OpenClipboard" c_OpenClipboard :: HWND -> IO BOOL
foreign import ccall safe "windows.h EmptyClipboard" c_EmptyClipboard :: IO BOOL
foreign import ccall safe "windows.h SetClipboardData" c_SetClipboardData :: UINT -> HANDLE -> IO HANDLE
foreign import ccall safe "windows.h CloseClipboard" c_CloseClipboard :: IO BOOL

-- | The window's controls, once it has made them.
data Parts = Parts {partHint, partBox, partVersion, partCopy, partClose :: HWND}

-- | Register the log window's class, reporting what fails in it.
registerLog :: Kit -> (String -> IO ()) -> IO (Either Int ())
registerLog kit report = do
    parts <- newIORef Nothing
    mark <- newIORef start
    -- Behind the read-only box, for the life of the process.
    white <- solidBrush (RGB 255 255 255)
    size <- c_GetSystemMetrics (fromIntegral sM_CXICON)
    registerWindow report "The log window" "trackkat_log" size (logProc kit parts mark white)

-- | Make the log window, hidden: top-level, so it can sit beside a game.
-- Left is a Windows error code.
makeLog :: Kit -> IO (Either Int HWND)
makeLog kit = topWindow "trackkat_log" "trackkat log" wS_OVERLAPPEDWINDOW (scaled kit 680, scaled kit 460)

logProc :: Kit -> IORef (Maybe Parts) -> IORef Mark -> HANDLE -> WindowClosure
logProc kit parts mark white w msg wp lp
    | msg == wM_CREATE = do
        p <- build kit w
        writeIORef parts (Just p)
        -- New lines join the box as often as the window refreshes.
        _ <- c_SetTimer w 1 33 nullPtr
        pure 0
    | msg == wM_TIMER = withParts (feed mark) >> pure 0
    | msg == wM_COMMAND, low wp == copyId = withParts (copyLog w) >> pure 0
    | msg == wM_COMMAND, low wp == closeId || low wp == fromIntegral iDCANCEL = showWindow w (fromIntegral sW_HIDE) >> pure 0
    | msg == wmAppCopyLog = withParts (copyLog w) >> pure 0
    -- Read-only, but it takes selections, so it looks like it.
    | msg == wmCtlColorStatic =
        readIORef parts >>= \case
            Just p | wordPtrToPtr (fromIntegral lp) == partBox p -> do
                _ <- c_SetBkColor (wordPtrToPtr (fromIntegral wp)) 0xFFFFFF
                pure (fromIntegral (pointerParam white :: WPARAM))
            _ -> defWindowProc (Just w) msg wp lp
    | msg == wM_SIZE = withParts (layout kit (fromIntegral (lp .&. 0xFFFF)) (fromIntegral ((lp `shiftR` 16) .&. 0xFFFF))) >> pure 0
    | msg == wM_GETMINMAXINFO = do
        setMinTrackSize (wordPtrToPtr (fromIntegral lp)) (scaled kit 400) (scaled kit 220)
        pure 0
    | msg == wM_CLOSE = showWindow w (fromIntegral sW_HIDE) >> pure 0
    | otherwise = defWindowProc (Just w) msg wp lp
  where
    withParts act = readIORef parts >>= mapM_ act
    low x = x .&. 0xFFFF :: WPARAM

-- | The main window's Copy log asks the log window for it, as WM_APP_COPY_LOG
-- in cbits/gui.c.
wmAppCopyLog :: WindowMessage
wmAppCopyLog = wM_APP + 6

copyId, closeId :: WPARAM
copyId = 101
closeId = 102

-- | The window's controls: a hint, the log, the version, and Copy log and
-- Close, with their help.
build :: Kit -> HWND -> IO Parts
build kit w = do
    tips <- makeTips kit w
    version <- shownVersion <$> snapshot
    hint <- control "STATIC" "What trackkat reports; copy it into bug reports." (sS_LEFT .|. sS_NOPREFIX) 0 UiFont
    box <- control "EDIT" "" (wS_VSCROLL .|. eS_MULTILINE .|. eS_READONLY .|. eS_AUTOVSCROLL) 0 MonoFont
    tip tips box "What trackkat reports: recentering, calibration, joysticks, errors"
    _ <- send box emSetLimitText (fromIntegral (logMax * 2)) 0
    versionLabel <- control "STATIC" ("trackkat " ++ version) (sS_LEFT .|. sS_NOPREFIX .|. ssNotify) 0 SmallFont
    tip tips versionLabel "The version of trackkat running, as bug reports should say"
    copy <- control "BUTTON" "Copy log" (wS_TABSTOP .|. bS_PUSHBUTTON) copyId UiFont
    tip tips copy "Copy the whole log, to paste into a bug report"
    close <- control "BUTTON" "Close" (wS_TABSTOP .|. bS_PUSHBUTTON) closeId UiFont
    tip tips close "Close the log window; trackkat keeps running, and the log keeps growing"
    let p = Parts hint box versionLabel copy close
    (_, _, right, bottom) <- getClientRect w
    layout kit (fromIntegral right) (fromIntegral bottom) p
    pure p
  where
    control :: String -> String -> DWORD -> WPARAM -> Font -> IO HWND
    control cls text style ident font = do
        c <- createWindow (if cls == "EDIT" then wS_EX_CLIENTEDGE else 0) cls text (wS_CHILD .|. wS_VISIBLE .|. wS_CLIPSIBLINGS .|. style) (0, 0, 0, 0) (Just w) (fromIntegral ident)
        _ <- send c wM_SETFONT (pointerParam (fontHandle kit font)) 0
        pure c

-- | The controls in a width and height: the hint along the top, the log
-- below it, and the version, Copy log and Close along the bottom.
layout :: Kit -> CInt -> CInt -> Parts -> IO ()
layout kit w h p = do
    moveWindow (partHint p) (s 12, s 10, w - s 24, s 18)
    moveWindow (partBox p) (s 12, s 32, w - s 24, h - s 32 - s 48)
    moveWindow (partVersion p) (s 12, h - s 33, w - s 240, s 18)
    moveWindow (partCopy p) (w - s 212, h - s 38, s 96, s 26)
    moveWindow (partClose p) (w - s 108, h - s 38, s 96, s 26)
  where
    s = scaled kit

-- | Add the lines logged since the box last took some.
feed :: IORef Mark -> Parts -> IO ()
feed mark p = do
    (new, now) <- since <$> readIORef mark <*> logBook
    writeIORef mark now
    unless (null new) $ append (partBox p) (concatMap (++ "\r\n") new)

-- | Add text to the end of the box, kept to about the log's length by
-- dropping whole lines from its start. A reader at the end follows it; one
-- who has scrolled up or selected something keeps their place.
append :: HWND -> String -> IO ()
append box text = do
    _ <- send box wM_SETREDRAW 0 0
    (start0, end0) <- selection
    (_, (page, pos, top, _)) <- withScrollInfo (c_GetScrollInfo box sbVert)
    let following = start0 == end0 && (page == 0 || pos + fromIntegral page >= top)
    first0 <- send box emGetFirstVisibleLine 0 0
    len0 <- c_GetWindowTextLength box
    (startAt, endAt, first, len) <-
        if fromIntegral len0 > logMax
            then do
                cutLine <- (+ 1) <$> send box emLineFromChar (fromIntegral (fromIntegral len0 - logMax * 3 `div` 4)) 0
                cut <- send box emLineIndex (fromIntegral cutLine) 0
                if cut > 0
                    then do
                        setSelection 0 (fromIntegral cut)
                        replace ""
                        let less x = if x > fromIntegral cut then x - fromIntegral cut else 0
                        pure (less start0, less end0, if first0 > cutLine then first0 - cutLine else 0, len0 - fromIntegral cut)
                    else pure (start0, end0, first0, len0)
            else pure (start0, end0, first0, len0)
    setSelection (fromIntegral len) (fromIntegral len)
    replace text
    if following
        then do
            end <- c_GetWindowTextLength box
            setSelection (fromIntegral end) (fromIntegral end)
            void (send box emScrollCaret 0 0)
        else do
            setSelection startAt endAt
            now <- send box emGetFirstVisibleLine 0 0
            void (send box emLineScroll 0 (fromIntegral (first - now)))
    _ <- send box wM_SETREDRAW 1 0
    void (c_InvalidateRect box nullPtr True)
  where
    selection =
        alloca $ \s -> alloca $ \e -> do
            _ <- send box emGetSel (pointerParam s) (pointerParam e)
            (,) <$> peek (s :: Ptr DWORD) <*> peek (e :: Ptr DWORD)
    setSelection :: DWORD -> DWORD -> IO ()
    setSelection s e = void (send box emSetSel (fromIntegral s) (fromIntegral e))
    replace t = withCWString t $ \str -> void (send box emReplaceSel 0 (pointerParam str))

-- | Copy what the box holds to the clipboard.
copyLog :: HWND -> Parts -> IO ()
copyLog w p = do
    len <- c_GetWindowTextLength (partBox p)
    mem <- c_GlobalAlloc gMEM_MOVEABLE (fromIntegral (len + 1) * 2)
    unless (mem == nullPtr) $ do
        text <- c_GlobalLock mem
        _ <- c_GetWindowText (partBox p) (castPtr text) (len + 1)
        _ <- c_GlobalUnlock mem
        handOver mem
  where
    handOver mem = do
        opened <- c_OpenClipboard w
        taken <-
            if opened
                then do
                    _ <- c_EmptyClipboard
                    handed <- c_SetClipboardData cF_UNICODETEXT mem
                    _ <- c_CloseClipboard
                    pure (handed /= nullPtr)
                else pure False
        unless taken (void (c_GlobalFree mem))
