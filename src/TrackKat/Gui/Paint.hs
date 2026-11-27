{-# LANGUAGE LambdaCase #-}

-- | What the parts of trackkat's window drawn in Haskell share: the
-- program's icons, drawing on a device context with the window's kit, and
-- window procedures an exception can't escape.
module TrackKat.Gui.Paint (
    registerPart,
    ownWindow,
    Background (..),
    guarded,
    Canvas (..),
    painted,
    resourceIcon,
    programIcon,
    Font (..),
    Colour (..),
    solidBrush,
    draw,
    fillRect,
    textWidth,
    textAt,
    Lines (..),
    textHeight,
    textIn,
    Stroke (..),
    polyline,
    disc,
    drawIcon,
    buffered,
) where

import Control.Exception (SomeAsyncException, bracket, catch, displayException, fromException, throwIO)
import Data.Bits ((.|.))
import Foreign.C.String (newCWString, withCWStringLen)
import Foreign.C.Types (CInt (..), CIntPtr (..))
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullPtr, wordPtrToPtr)
import Graphics.Win32.GDI.Bitmap (sRCCOPY)
import Graphics.Win32.GDI.Brush (nULL_BRUSH)
import Graphics.Win32.GDI.Pen (PenStyle, pS_DASH, pS_DOT, pS_SOLID)
import Graphics.Win32.GDI.Types (HDC, HICON, HWND, LPRECT, RECT, allocaRECT, peekRECT, pokeRECT, tRANSPARENT)
import Graphics.Win32.Message (wM_SETICON)
import Graphics.Win32.Misc (iDC_ARROW, loadCursor)
import Graphics.Win32.Resource (iMAGE_ICON, lR_SHARED)
import Graphics.Win32.Window (LPPAINTSTRUCT, WindowClosure, adjustWindowRect, allocaPAINTSTRUCT, cW_USEDEFAULT, defWindowProc, wS_EX_CONTROLPARENT)
import System.Win32.DLL (getModuleHandle)
import System.Win32.Info (cOLOR_BTNFACE, sM_CXICON, sM_CXSMICON)
import System.Win32.Types (ATOM, BOOL, DWORD, HANDLE, HINSTANCE, INT, LPCTSTR, UINT, getLastError)

import TrackKat.Gui.Calls (createWindow, pointerParam, send)
import TrackKat.Gui.Kit (Kit, fontHandle)
import TrackKat.Gui.Win32
import TrackKat.UI.Draw

-- BeginPaint can send the window messages of its own, so it is safe.
foreign import ccall safe "windows.h BeginPaint" c_BeginPaint :: HWND -> LPPAINTSTRUCT -> IO HDC
foreign import ccall unsafe "windows.h EndPaint" c_EndPaint :: HWND -> LPPAINTSTRUCT -> IO BOOL

foreign import ccall unsafe "windows.h LoadImageW"
    c_LoadImage :: HINSTANCE -> LPCTSTR -> UINT -> CInt -> CInt -> UINT -> IO HICON
foreign import ccall unsafe "windows.h LoadIconW" c_LoadIcon :: HINSTANCE -> LPCTSTR -> IO HICON
foreign import ccall unsafe "windows.h CreateSolidBrush" c_CreateSolidBrush :: DWORD -> IO HANDLE
foreign import ccall unsafe "windows.h CreatePen" c_CreatePen :: INT -> CInt -> DWORD -> IO HANDLE
foreign import ccall unsafe "windows.h DeleteObject" c_DeleteObject :: HANDLE -> IO BOOL
foreign import ccall unsafe "windows.h SelectObject" c_SelectObject :: HDC -> HANDLE -> IO HANDLE
foreign import ccall unsafe "windows.h FillRect" c_FillRect :: HDC -> LPRECT -> HANDLE -> IO CInt
foreign import ccall unsafe "windows.h SetTextColor" c_SetTextColor :: HDC -> DWORD -> IO DWORD
foreign import ccall unsafe "windows.h SetBkMode" c_SetBkMode :: HDC -> INT -> IO INT
foreign import ccall unsafe "windows.h DrawTextW" c_DrawText :: HDC -> Ptr () -> CInt -> LPRECT -> UINT -> IO CInt
foreign import ccall unsafe "windows.h Polyline" c_Polyline :: HDC -> Ptr CInt -> CInt -> IO BOOL
foreign import ccall unsafe "windows.h DrawIconEx"
    c_DrawIconEx :: HDC -> CInt -> CInt -> HICON -> CInt -> CInt -> UINT -> HANDLE -> UINT -> IO BOOL
foreign import ccall unsafe "windows.h Ellipse" c_Ellipse :: HDC -> CInt -> CInt -> CInt -> CInt -> IO BOOL
foreign import ccall unsafe "windows.h Rectangle" c_Rectangle :: HDC -> CInt -> CInt -> CInt -> CInt -> IO BOOL
foreign import ccall unsafe "windows.h Polygon" c_Polygon :: HDC -> Ptr CInt -> CInt -> IO BOOL
foreign import ccall unsafe "windows.h RoundRect"
    c_RoundRect :: HDC -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO BOOL
foreign import ccall unsafe "windows.h SetBkColor" c_SetBkColor :: HDC -> DWORD -> IO DWORD
foreign import ccall unsafe "windows.h DrawFocusRect" c_DrawFocusRect :: HDC -> LPRECT -> IO BOOL
foreign import ccall unsafe "windows.h GetStockObject" c_GetStockObject :: INT -> IO HANDLE
foreign import ccall unsafe "windows.h SetTextCharacterExtra" c_SetTextCharacterExtra :: HDC -> CInt -> IO CInt
foreign import ccall unsafe "windows.h SaveDC" c_SaveDC :: HDC -> IO CInt
foreign import ccall unsafe "windows.h RestoreDC" c_RestoreDC :: HDC -> CInt -> IO BOOL
foreign import ccall unsafe "windows.h IntersectClipRect" c_IntersectClipRect :: HDC -> CInt -> CInt -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "windows.h CreateCompatibleDC" c_CreateCompatibleDC :: HDC -> IO HDC
foreign import ccall unsafe "windows.h DeleteDC" c_DeleteDC :: HDC -> IO BOOL
foreign import ccall unsafe "windows.h CreateCompatibleBitmap" c_CreateCompatibleBitmap :: HDC -> CInt -> CInt -> IO HANDLE
foreign import ccall unsafe "windows.h BitBlt"
    c_BitBlt :: HDC -> CInt -> CInt -> CInt -> CInt -> HDC -> CInt -> CInt -> DWORD -> IO BOOL
foreign import ccall unsafe "windows.h RegisterClassW" c_RegisterClass :: Ptr () -> IO ATOM
foreign import ccall unsafe "windows.h GetSystemMetrics" c_GetSystemMetrics :: CInt -> IO CInt
foreign import ccall unsafe "windows.h SystemParametersInfoW" c_SystemParametersInfo :: UINT -> UINT -> Ptr () -> UINT -> IO BOOL
foreign import ccall "wrapper" mkProc :: WindowClosure -> IO (FunPtr WindowClosure)

-- | Register a window class for a part of the window drawn here; its
-- procedure is 'guarded', named by the part. Left is a Windows error code.
registerPart :: (String -> IO ()) -> String -> String -> Background -> WindowClosure -> IO (Either Int ())
registerPart report part className background = registerClass report part className background nullPtr

registerClass :: (String -> IO ()) -> String -> String -> Background -> HICON -> WindowClosure -> IO (Either Int ())
registerClass report part className background icon proc = do
    inst <- getModuleHandle Nothing
    cursor <- loadCursor Nothing iDC_ARROW
    -- Both for the life of the process.
    wndProc <- mkProc (guarded report part proc)
    name <- newCWString className
    atom <- withWndClass wndProc inst cursor (backgroundBrush background) icon name c_RegisterClass
    if atom /= 0 then pure (Right ()) else Left . fromIntegral <$> getLastError

-- | A window of its own, hidden, of a class registered for it with the
-- program's icon and a procedure 'guarded' as in 'registerPart'; with a
-- title and style, and a client area of a width and height but no taller
-- than the screen's work area. Left is a Windows error code.
ownWindow :: (String -> IO ()) -> String -> String -> String -> DWORD -> (CInt, CInt) -> WindowClosure -> IO (Either Int HWND)
ownWindow report part cls title style (w, h) proc = do
    icon <- c_GetSystemMetrics (fromIntegral sM_CXICON) >>= resourceIcon programIcon
    registered <- registerClass report part cls ButtonFace icon proc
    case registered of
        Left e -> pure (Left e)
        Right () -> do
            (l, t, r, b) <- adjustWindowRect (0, 0, fromIntegral w, fromIntegral h) style False
            work <- workArea
            let height = maybe id (\(_, top, _, bottom) -> min (bottom - top)) work (b - t)
            wnd <- createWindow wS_EX_CONTROLPARENT cls title style (anywhere, anywhere, fromIntegral (r - l), fromIntegral height) Nothing 0
            if wnd == nullPtr
                then Left . fromIntegral <$> getLastError
                else do
                    -- The title bar's and taskbar's, drawn at its own size
                    -- rather than shrunk from the class's.
                    small <- c_GetSystemMetrics (fromIntegral sM_CXSMICON) >>= resourceIcon programIcon
                    _ <- send wnd wM_SETICON iconSmall (pointerParam small)
                    pure (Right wnd)
  where
    anywhere = fromIntegral cW_USEDEFAULT

-- | The screen's but for the taskbar, if Windows says.
workArea :: IO (Maybe RECT)
workArea = allocaRECT $ \p -> do
    ok <- c_SystemParametersInfo spiGetWorkArea 0 (castPtr p) 0
    if ok then Just <$> peekRECT p else pure Nothing

-- | What Windows fills a part's window with before it is painted.
data Background
    = -- | nothing: the part paints all of itself
      Unfilled
    | -- | the colour of buttons and dialogs
      ButtonFace

-- | A window class's background brush, as WNDCLASSW's hbrBackground takes a
-- system colour.
backgroundBrush :: Background -> HANDLE
backgroundBrush Unfilled = nullPtr
backgroundBrush ButtonFace = wordPtrToPtr (fromIntegral cOLOR_BTNFACE + 1)

-- | A window procedure that reports what fails in it, naming the part of
-- the window, and leaves the message to Windows' handling: an exception
-- can't leave a window procedure but by ending the process.
guarded :: (String -> IO ()) -> String -> WindowClosure -> WindowClosure
guarded report part proc w msg wp lp =
    proc w msg wp lp `catch` \e -> case fromException e of
        Just async -> throwIO (async :: SomeAsyncException)
        Nothing -> report (part ++ " failed: " ++ displayException e) >> defWindowProc (Just w) msg wp lp

-- | A device context to draw on, and the kit to draw with.
data Canvas = Canvas {canvasKit :: Kit, canvasDC :: HDC}

-- | Paint a window that asked to be, ending the paint whatever happens.
painted :: Kit -> HWND -> (Canvas -> IO ()) -> IO ()
painted kit w paint = allocaPAINTSTRUCT $ \ps -> bracket (c_BeginPaint w ps) (\_ -> c_EndPaint w ps) (paint . Canvas kit)

-- | An icon from the program's resources (icons/trackkat.rc) at a size in
-- pixels, or Windows' own if the build left it out. Shared: never destroyed.
resourceIcon :: Int -> CInt -> IO HICON
resourceIcon number size = do
    inst <- getModuleHandle Nothing
    icon <- c_LoadImage inst (resource number) iMAGE_ICON size size lR_SHARED
    if icon /= nullPtr then pure icon else c_LoadIcon nullPtr (resource idiApplication)
  where
    resource :: Int -> LPCTSTR
    resource = wordPtrToPtr . fromIntegral
    -- IDI_APPLICATION, a resource number cast to a pointer.
    idiApplication = 32512

-- | The program's own icon, in icons/trackkat.rc.
programIcon :: Int
programIcon = 1

-- | A colour as Windows takes it: red, green and blue in its low bytes.
colourRef :: Colour -> DWORD
colourRef (RGB r g b) = fromIntegral r .|. fromIntegral g * 256 .|. fromIntegral b * 65536

solidBrush :: Colour -> IO HANDLE
solidBrush = c_CreateSolidBrush . colourRef

-- | Fill left, top, right, bottom with a colour.
fillRect :: Canvas -> RECT -> Colour -> IO ()
fillRect (Canvas _ dc) r c = do
    brush <- solidBrush c
    _ <- allocaRECT $ \p -> pokeRECT p r >> c_FillRect dc p brush
    () <$ c_DeleteObject brush

-- | How wide text is on one line in a font.
textWidth :: Canvas -> Font -> String -> IO CInt
textWidth cv font s = do
    selectFont cv font
    (_, _, right, _) <- drawText (canvasDC cv) s (0, 0, 0, 0) (dtSingleLine .|. dtNoPrefix .|. dtCalcRect)
    pure (fromIntegral right)

-- | Draw text on one line, centred down x, y, w, h and ended with an
-- ellipsis if it doesn't fit, aligned by DT_LEFT and the like.
textAt :: Canvas -> Font -> Colour -> (CInt, CInt, CInt, CInt) -> UINT -> String -> IO ()
textAt cv font c (x, y, w, h) align s = do
    selectFont cv font
    inkFor (canvasDC cv) c
    () <$ drawText (canvasDC cv) s (rect x y w h) (align .|. dtSingleLine .|. dtVCenter .|. dtNoPrefix .|. dtEndEllipsis)

-- | Text broken into lines at spaces, to fit a width.
data Lines = Wrapped

-- | How tall text broken into lines comes to in a width.
textHeight :: Canvas -> Font -> Lines -> CInt -> String -> IO CInt
textHeight cv font Wrapped w s = do
    selectFont cv font
    (_, top, _, bottom) <- drawText (canvasDC cv) s (0, 0, fromIntegral w, 0) (dtWordBreak .|. dtNoPrefix .|. dtCalcRect)
    pure (fromIntegral (bottom - top))

-- | Draw text broken into lines in x, y, w, h, the last ended with an
-- ellipsis if it doesn't fit.
textIn :: Canvas -> Font -> Colour -> Lines -> (CInt, CInt, CInt, CInt) -> String -> IO ()
textIn cv font c Wrapped (x, y, w, h) s = do
    selectFont cv font
    inkFor (canvasDC cv) c
    () <$ drawText (canvasDC cv) s (rect x y w h) (dtWordBreak .|. dtEditControl .|. dtNoPrefix .|. dtEndEllipsis)

-- | Draw lines through points with a pen of a stroke, width and colour.
polyline :: Canvas -> Stroke -> CInt -> Colour -> [(CInt, CInt)] -> IO ()
polyline (Canvas _ dc) stroke width c points =
    withTools dc stroke width c Nothing $
        withArrayLen (concatMap (\(x, y) -> [x, y]) points) $
            \n p -> c_Polyline dc p (fromIntegral (n `div` 2))

-- | Draw a disc of a fill colour, outlined in a pen's width and colour,
-- of a radius about x, y.
disc :: Canvas -> Colour -> CInt -> Colour -> (CInt, CInt) -> CInt -> IO ()
disc (Canvas _ dc) fill width line (x, y) r = withTools dc Solid width line (Just fill) (c_Ellipse dc (x - r) (y - r) (x + r + 1) (y + r + 1))

-- | Draw a shape.
draw :: Canvas -> Shape -> IO ()
draw cv = \case
    Fill c r -> fillRect cv (rectOf r) c
    Frame c (l, t, r, b) -> withTools dc Solid 1 c Nothing (c_Rectangle dc (n l) (n t) (n r) (n b))
    Line stroke width c points -> polyline cv stroke (n width) c (map point points)
    Disc fill width line centre r -> disc cv fill (n width) line (point centre) (n r)
    Ring stroke width c (x, y) r ->
        withTools dc stroke (n width) c Nothing (c_Ellipse dc (n (x - r)) (n (y - r)) (n (x + r + 1)) (n (y + r + 1)))
    Polygon c points ->
        withTools dc Solid 1 c (Just c) $
            withArrayLen (concatMap (\(x, y) -> [n x, n y]) points) $
                \k p -> c_Polygon dc p (fromIntegral (k `div` 2))
    Rounded fill edge corner (l, t, r, b) ->
        withTools dc Solid 1 edge fill (c_RoundRect dc (n l) (n t) (n r) (n b) (n corner) (n corner))
    Focus (l, t, r, b) -> allocaRECT $ \p -> do
        pokeRECT p (fromIntegral l, fromIntegral t, fromIntegral r, fromIntegral b)
        _ <- c_SetTextColor dc 0
        _ <- c_SetBkColor dc 0xFFFFFF
        () <$ c_DrawFocusRect dc p
    Text font c spacing align (l, t, r, b) s -> do
        _ <- c_SetTextCharacterExtra dc (n spacing)
        textAt cv font c (n l, n t, n (r - l), n (b - t)) (alignment align) s
        () <$ c_SetTextCharacterExtra dc 0
    Paragraph font c r s -> do
        selectFont cv font
        inkFor dc c
        () <$ drawText dc s (rectOf r) (dtLeft .|. dtWordBreak .|. dtNoPrefix)
    Clipped (l, t, r, b) shapes -> do
        _ <- c_SaveDC dc
        _ <- c_IntersectClipRect dc (n l) (n t) (n r) (n b)
        mapM_ (draw cv) shapes
        () <$ c_RestoreDC dc (-1)
  where
    dc = canvasDC cv
    n :: Int -> CInt
    n = fromIntegral
    point (x, y) = (n x, n y)
    rectOf (l, t, r, b) = (fromIntegral l, fromIntegral t, fromIntegral r, fromIntegral b)
    alignment AlignLeft = dtLeft
    alignment AlignCentre = dtCenter
    alignment AlignRight = dtRight

-- | Draw with a pen of a stroke, width and colour, and a brush of a colour
-- or none; a dotted or dashed pen's gaps leave what is there.
withTools :: HDC -> Stroke -> CInt -> Colour -> Maybe Colour -> IO a -> IO ()
withTools dc stroke width line fill act = do
    _ <- c_SetBkMode dc tRANSPARENT
    bracket acquire release (\_ -> () <$ act)
  where
    acquire = do
        pen <- c_CreatePen (penStyle stroke) width (colourRef line)
        brush <- maybe (c_GetStockObject nULL_BRUSH) solidBrush fill
        oldPen <- c_SelectObject dc pen
        oldBrush <- c_SelectObject dc brush
        pure (pen, brush, oldPen, oldBrush)
    release (pen, brush, oldPen, oldBrush) = do
        mapM_ (c_SelectObject dc) [oldBrush, oldPen]
        _ <- c_DeleteObject pen
        mapM_ (\_ -> c_DeleteObject brush) fill

penStyle :: Stroke -> PenStyle
penStyle Solid = pS_SOLID
penStyle Dotted = pS_DOT
penStyle Dashed = pS_DASH

-- | Draw an icon at x, y, size pixels square.
drawIcon :: Canvas -> HICON -> CInt -> CInt -> CInt -> IO ()
drawIcon (Canvas _ dc) icon x y size = () <$ c_DrawIconEx dc x y icon size size 0 nullPtr diNormal

-- | Draw a width and height off screen, then onto a device context at x,
-- y at once, so it never shows half drawn.
buffered :: Canvas -> (CInt, CInt, CInt, CInt) -> (Canvas -> IO ()) -> IO ()
buffered (Canvas kit dc) (x, y, w, h) paint =
    bracket (c_CreateCompatibleDC dc) c_DeleteDC $ \mem ->
        bracket (c_CreateCompatibleBitmap dc w h) c_DeleteObject $ \bitmap ->
            bracket (c_SelectObject mem bitmap) (c_SelectObject mem) $ \_ -> do
                paint (Canvas kit mem)
                () <$ c_BitBlt dc x y w h mem 0 0 sRCCOPY

selectFont :: Canvas -> Font -> IO ()
selectFont (Canvas kit dc) font = () <$ c_SelectObject dc (fontHandle kit font)

-- | Text drawn over what is there, in a colour.
inkFor :: HDC -> Colour -> IO ()
inkFor dc c = c_SetBkMode dc tRANSPARENT >> () <$ c_SetTextColor dc (colourRef c)

rect :: CInt -> CInt -> CInt -> CInt -> RECT
rect x y w h = (fromIntegral x, fromIntegral y, fromIntegral (x + w), fromIntegral (y + h))

-- | DrawTextW, giving back the rectangle as DT_CALCRECT leaves it.
drawText :: HDC -> String -> RECT -> UINT -> IO RECT
drawText dc s r flags =
    withCWStringLen s $ \(str, n) -> allocaRECT $ \p -> do
        pokeRECT p r
        _ <- c_DrawText dc (castPtr str) (fromIntegral n) p flags
        peekRECT p
