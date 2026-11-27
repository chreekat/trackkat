{-# LANGUAGE LambdaCase #-}

-- | What the parts of trackkat's window drawn in Haskell share: the
-- program's icons, the window's fonts and scale, drawing on a device
-- context, and window procedures an exception can't escape.
module TrackKat.Gui.Paint (
    registerPart,
    guarded,
    resourceIcon,
    Font (..),
    screenDpi,
    scaled,
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

#include <windows.h>

import Control.Exception (SomeAsyncException, bracket, catch, displayException, fromException, throwIO)
import Data.Bits ((.|.))
import Foreign.C.String (newCWString, withCWStringLen)
import Foreign.C.Types (CInt (..), CIntPtr (..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullPtr, wordPtrToPtr)
import Foreign.Storable (pokeByteOff)
import Graphics.Win32.GDI.Types (HDC, HICON, LPRECT, RECT, allocaRECT, peekRECT, pokeRECT)
import Graphics.Win32.Misc (iDC_ARROW, loadCursor)
import Graphics.Win32.Window (WindowClosure, defWindowProc)
import System.Win32.DLL (getModuleHandle)
import System.Win32.Types (ATOM, BOOL, DWORD, HANDLE, HINSTANCE, LPCTSTR, UINT, getLastError)

import TrackKat.UI.Draw

foreign import ccall unsafe "hts_gui_dpi" c_dpi :: IO CInt
foreign import ccall unsafe "hts_gui_font" c_font :: CInt -> IO HANDLE

foreign import ccall unsafe "windows.h LoadImageW"
    c_LoadImage :: HINSTANCE -> LPCTSTR -> UINT -> CInt -> CInt -> UINT -> IO HICON
foreign import ccall unsafe "windows.h LoadIconW" c_LoadIcon :: HINSTANCE -> LPCTSTR -> IO HICON
foreign import ccall unsafe "windows.h CreateSolidBrush" c_CreateSolidBrush :: DWORD -> IO HANDLE
foreign import ccall unsafe "windows.h CreatePen" c_CreatePen :: CInt -> CInt -> DWORD -> IO HANDLE
foreign import ccall unsafe "windows.h DeleteObject" c_DeleteObject :: HANDLE -> IO BOOL
foreign import ccall unsafe "windows.h SelectObject" c_SelectObject :: HDC -> HANDLE -> IO HANDLE
foreign import ccall unsafe "windows.h FillRect" c_FillRect :: HDC -> LPRECT -> HANDLE -> IO CInt
foreign import ccall unsafe "windows.h SetTextColor" c_SetTextColor :: HDC -> DWORD -> IO DWORD
foreign import ccall unsafe "windows.h SetBkMode" c_SetBkMode :: HDC -> CInt -> IO CInt
foreign import ccall unsafe "windows.h DrawTextW" c_DrawText :: HDC -> Ptr () -> CInt -> LPRECT -> UINT -> IO CInt
foreign import ccall unsafe "windows.h Polyline" c_Polyline :: HDC -> Ptr CInt -> CInt -> IO BOOL
foreign import ccall unsafe "windows.h DrawIconEx"
    c_DrawIconEx :: HDC -> CInt -> CInt -> HICON -> CInt -> CInt -> UINT -> HANDLE -> UINT -> IO BOOL
foreign import ccall unsafe "windows.h Ellipse" c_Ellipse :: HDC -> CInt -> CInt -> CInt -> CInt -> IO BOOL
foreign import ccall unsafe "windows.h Rectangle" c_Rectangle :: HDC -> CInt -> CInt -> CInt -> CInt -> IO BOOL
foreign import ccall unsafe "windows.h Polygon" c_Polygon :: HDC -> Ptr CInt -> CInt -> IO BOOL
foreign import ccall unsafe "windows.h GetStockObject" c_GetStockObject :: CInt -> IO HANDLE
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
foreign import ccall "wrapper" mkProc :: WindowClosure -> IO (FunPtr WindowClosure)

-- | Register a window class for a part of the window drawn here, which
-- cbits/gui.c's hts_gui_create then makes it of rather than registering its
-- own; its procedure is 'guarded', named by the part. Left is a Windows
-- error code.
registerPart :: (String -> IO ()) -> String -> String -> WindowClosure -> IO (Either Int ())
registerPart report part className proc = do
    inst <- getModuleHandle Nothing
    cursor <- loadCursor Nothing iDC_ARROW
    -- Both for the life of the process.
    wndProc <- mkProc (guarded report part proc)
    name <- newCWString className
    atom <- allocaBytes size $ \p -> do
        fillBytes p 0 size
        #{poke WNDCLASSW, lpfnWndProc} p wndProc
        #{poke WNDCLASSW, hInstance} p inst
        #{poke WNDCLASSW, hCursor} p cursor
        #{poke WNDCLASSW, lpszClassName} p name
        c_RegisterClass (castPtr p)
    if atom /= 0 then pure (Right ()) else Left . fromIntegral <$> getLastError
  where
    size = #{size WNDCLASSW}

-- | A window procedure that reports what fails in it, naming the part of
-- the window, and leaves the message to Windows' handling: an exception
-- can't leave a window procedure but by ending the process.
guarded :: (String -> IO ()) -> String -> WindowClosure -> WindowClosure
guarded report part proc w msg wp lp =
    proc w msg wp lp `catch` \e -> case fromException e of
        Just async -> throwIO (async :: SomeAsyncException)
        Nothing -> report (part ++ " failed: " ++ displayException e) >> defWindowProc (Just w) msg wp lp

-- | An icon from the program's resources (icons/trackkat.rc) at a size in
-- pixels, or Windows' own if the build left it out. Shared: never destroyed.
resourceIcon :: Int -> CInt -> IO HICON
resourceIcon number size = do
    inst <- getModuleHandle Nothing
    icon <- c_LoadImage inst (resource number) #{const IMAGE_ICON} size size #{const LR_SHARED}
    if icon /= nullPtr then pure icon else c_LoadIcon nullPtr (resource idiApplication)
  where
    resource :: Int -> LPCTSTR
    resource = wordPtrToPtr . fromIntegral
    -- IDI_APPLICATION, a resource number cast to a pointer.
    idiApplication = 32512

-- | The screen's dots per inch, which the window is drawn to.
screenDpi :: IO Int
screenDpi = fromIntegral <$> c_dpi

-- | A length at 96 dots per inch, at the screen's.
scaled :: CInt -> IO CInt
scaled x = (\dpi -> fromIntegral (scaledAt (fromIntegral dpi) (fromIntegral x))) <$> c_dpi

-- | A colour as Windows takes it: red, green and blue in its low bytes.
colourRef :: Colour -> DWORD
colourRef (RGB r g b) = fromIntegral r .|. fromIntegral g * 256 .|. fromIntegral b * 65536

solidBrush :: Colour -> IO HANDLE
solidBrush = c_CreateSolidBrush . colourRef

-- | Fill left, top, right, bottom with a colour.
fillRect :: HDC -> RECT -> Colour -> IO ()
fillRect dc r c = do
    brush <- solidBrush c
    _ <- allocaRECT $ \p -> pokeRECT p r >> c_FillRect dc p brush
    () <$ c_DeleteObject brush

-- | How wide text is on one line in a font.
textWidth :: HDC -> Font -> String -> IO CInt
textWidth dc font s = do
    selectFont dc font
    (_, _, right, _) <- drawText dc s (0, 0, 0, 0) (#{const DT_SINGLELINE} .|. #{const DT_NOPREFIX} .|. #{const DT_CALCRECT})
    pure (fromIntegral right)

-- | Draw text on one line, centred down x, y, w, h and ended with an
-- ellipsis if it doesn't fit, aligned by DT_LEFT and the like.
textAt :: HDC -> Font -> Colour -> (CInt, CInt, CInt, CInt) -> UINT -> String -> IO ()
textAt dc font c (x, y, w, h) align s = do
    selectFont dc font
    inkFor dc c
    () <$ drawText dc s (rect x y w h) (align .|. #{const DT_SINGLELINE} .|. #{const DT_VCENTER} .|. #{const DT_NOPREFIX} .|. #{const DT_END_ELLIPSIS})

-- | Text broken into lines at spaces, to fit a width.
data Lines = Wrapped

-- | How tall text broken into lines comes to in a width.
textHeight :: HDC -> Font -> Lines -> CInt -> String -> IO CInt
textHeight dc font Wrapped w s = do
    selectFont dc font
    (_, top, _, bottom) <- drawText dc s (0, 0, fromIntegral w, 0) (#{const DT_WORDBREAK} .|. #{const DT_NOPREFIX} .|. #{const DT_CALCRECT})
    pure (fromIntegral (bottom - top))

-- | Draw text broken into lines in x, y, w, h, the last ended with an
-- ellipsis if it doesn't fit.
textIn :: HDC -> Font -> Colour -> Lines -> (CInt, CInt, CInt, CInt) -> String -> IO ()
textIn dc font c Wrapped (x, y, w, h) s = do
    selectFont dc font
    inkFor dc c
    () <$ drawText dc s (rect x y w h) (#{const DT_WORDBREAK} .|. #{const DT_EDITCONTROL} .|. #{const DT_NOPREFIX} .|. #{const DT_END_ELLIPSIS})

-- | Draw lines through points with a pen of a stroke, width and colour.
polyline :: HDC -> Stroke -> CInt -> Colour -> [(CInt, CInt)] -> IO ()
polyline dc stroke width c points =
    withTools dc stroke width c Nothing $
        withArrayLen (concatMap (\(x, y) -> [x, y]) points) $ \n p -> c_Polyline dc p (fromIntegral (n `div` 2))

-- | Draw a disc of a fill colour, outlined in a pen's width and colour,
-- of a radius about x, y.
disc :: HDC -> Colour -> CInt -> Colour -> (CInt, CInt) -> CInt -> IO ()
disc dc fill width line (x, y) r = withTools dc Solid width line (Just fill) (c_Ellipse dc (x - r) (y - r) (x + r + 1) (y + r + 1))

-- | Draw a shape.
draw :: HDC -> Shape -> IO ()
draw dc = \case
    Fill c r -> fillRect dc (rectOf r) c
    Frame c (l, t, r, b) -> withTools dc Solid 1 c Nothing (c_Rectangle dc (n l) (n t) (n r) (n b))
    Line stroke width c points -> polyline dc stroke (n width) c (map point points)
    Disc fill width line centre r -> disc dc fill (n width) line (point centre) (n r)
    Ring stroke width c (x, y) r ->
        withTools dc stroke (n width) c Nothing (c_Ellipse dc (n (x - r)) (n (y - r)) (n (x + r + 1)) (n (y + r + 1)))
    Polygon c points ->
        withTools dc Solid 1 c (Just c) $
            withArrayLen (concatMap (\(x, y) -> [n x, n y]) points) $ \k p -> c_Polygon dc p (fromIntegral (k `div` 2))
    Text font c spacing align (l, t, r, b) s -> do
        _ <- c_SetTextCharacterExtra dc (n spacing)
        textAt dc font c (n l, n t, n (r - l), n (b - t)) (alignment align) s
        () <$ c_SetTextCharacterExtra dc 0
    Paragraph font c r s -> do
        selectFont dc font
        inkFor dc c
        () <$ drawText dc s (rectOf r) (#{const DT_LEFT} .|. #{const DT_WORDBREAK} .|. #{const DT_NOPREFIX})
    Clipped (l, t, r, b) shapes -> do
        _ <- c_SaveDC dc
        _ <- c_IntersectClipRect dc (n l) (n t) (n r) (n b)
        mapM_ (draw dc) shapes
        () <$ c_RestoreDC dc (-1)
  where
    n :: Int -> CInt
    n = fromIntegral
    point (x, y) = (n x, n y)
    rectOf (l, t, r, b) = (fromIntegral l, fromIntegral t, fromIntegral r, fromIntegral b)
    alignment AlignLeft = #{const DT_LEFT}
    alignment AlignCentre = #{const DT_CENTER}
    alignment AlignRight = #{const DT_RIGHT}

-- | Draw with a pen of a stroke, width and colour, and a brush of a colour
-- or none; a dotted or dashed pen's gaps leave what is there.
withTools :: HDC -> Stroke -> CInt -> Colour -> Maybe Colour -> IO a -> IO ()
withTools dc stroke width line fill act = do
    _ <- c_SetBkMode dc #{const TRANSPARENT}
    pen <- c_CreatePen (penStyle stroke) width (colourRef line)
    brush <- maybe (c_GetStockObject #{const NULL_BRUSH}) solidBrush fill
    oldPen <- c_SelectObject dc pen
    oldBrush <- c_SelectObject dc brush
    _ <- act
    mapM_ (c_SelectObject dc) [oldBrush, oldPen]
    _ <- c_DeleteObject pen
    mapM_ (\_ -> c_DeleteObject brush) fill

penStyle :: Stroke -> CInt
penStyle Solid = #{const PS_SOLID}
penStyle Dotted = #{const PS_DOT}
penStyle Dashed = #{const PS_DASH}

-- | Draw an icon at x, y, size pixels square.
drawIcon :: HDC -> HICON -> CInt -> CInt -> CInt -> IO ()
drawIcon dc icon x y size = () <$ c_DrawIconEx dc x y icon size size 0 nullPtr #{const DI_NORMAL}

-- | Draw a width and height off screen, then onto a device context at x,
-- y at once, so it never shows half drawn.
buffered :: HDC -> (CInt, CInt, CInt, CInt) -> (HDC -> IO ()) -> IO ()
buffered dc (x, y, w, h) paint =
    bracket (c_CreateCompatibleDC dc) c_DeleteDC $ \mem ->
        bracket (c_CreateCompatibleBitmap dc w h) c_DeleteObject $ \bitmap ->
            bracket (c_SelectObject mem bitmap) (c_SelectObject mem) $ \_ -> do
                paint mem
                () <$ c_BitBlt dc x y w h mem 0 0 #{const SRCCOPY}

selectFont :: HDC -> Font -> IO ()
selectFont dc font = do
    f <- c_font (fromIntegral (fromEnum font))
    () <$ c_SelectObject dc f

-- | Text drawn over what is there, in a colour.
inkFor :: HDC -> Colour -> IO ()
inkFor dc c = c_SetBkMode dc #{const TRANSPARENT} >> () <$ c_SetTextColor dc (colourRef c)

rect :: CInt -> CInt -> CInt -> CInt -> RECT
rect x y w h = (fromIntegral x, fromIntegral y, fromIntegral (x + w), fromIntegral (y + h))

-- | DrawTextW, giving back the rectangle as DT_CALCRECT leaves it.
drawText :: HDC -> String -> RECT -> UINT -> IO RECT
drawText dc s r flags =
    withCWStringLen s $ \(str, n) -> allocaRECT $ \p -> do
        pokeRECT p r
        _ <- c_DrawText dc (castPtr str) (fromIntegral n) p flags
        peekRECT p
