-- | What the parts of trackkat's window drawn in Haskell share: the
-- program's icons, the window's fonts and scale, drawing on a device
-- context, and window procedures an exception can't escape.
module TrackKat.Gui.Paint (
    guarded,
    resourceIcon,
    Font (..),
    scaled,
    Colour,
    rgb,
    fillRect,
    textWidth,
    textAt,
    Lines (..),
    textHeight,
    textIn,
    polyline,
    drawIcon,
) where

#include <windows.h>

import Control.Exception (SomeAsyncException, catch, displayException, fromException, throwIO)
import Data.Bits ((.|.))
import Foreign.C.String (withCWStringLen)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Ptr (Ptr, castPtr, nullPtr, wordPtrToPtr)
import Graphics.Win32.GDI.Types (HDC, HICON, LPRECT, RECT, allocaRECT, peekRECT, pokeRECT)
import Graphics.Win32.Window (WindowClosure, defWindowProc)
import System.Win32.DLL (getModuleHandle)
import System.Win32.Types (BOOL, DWORD, HANDLE, HINSTANCE, LPCTSTR, UINT)

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

-- | The window's fonts, as cbits/gui.c's hts_gui_font numbers them.
data Font = UiFont | BoldFont | SmallFont
    deriving (Eq, Show, Enum)

-- | A length at 96 dots per inch, at the screen's.
scaled :: CInt -> IO CInt
scaled x = (\dpi -> x * dpi `div` 96) <$> c_dpi

-- | A colour as Windows takes it: red, green and blue in its low bytes.
type Colour = DWORD

rgb :: DWORD -> DWORD -> DWORD -> Colour
rgb r g b = r .|. g * 256 .|. b * 65536

-- | Fill left, top, right, bottom with a colour.
fillRect :: HDC -> RECT -> Colour -> IO ()
fillRect dc r c = do
    brush <- c_CreateSolidBrush c
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

-- | Draw lines through points with a solid pen of a width and colour.
polyline :: HDC -> CInt -> Colour -> [(CInt, CInt)] -> IO ()
polyline dc width c points = do
    pen <- c_CreatePen #{const PS_SOLID} width c
    old <- c_SelectObject dc pen
    _ <- withArrayLen (concatMap (\(x, y) -> [x, y]) points) $ \n p -> c_Polyline dc p (fromIntegral (n `div` 2))
    _ <- c_SelectObject dc old
    () <$ c_DeleteObject pen

-- | Draw an icon at x, y, size pixels square.
drawIcon :: HDC -> HICON -> CInt -> CInt -> CInt -> IO ()
drawIcon dc icon x y size = () <$ c_DrawIconEx dc x y icon size size 0 nullPtr #{const DI_NORMAL}

selectFont :: HDC -> Font -> IO ()
selectFont dc font = do
    f <- c_font (fromIntegral (fromEnum font))
    () <$ c_SelectObject dc f

-- | Text drawn over what is there, in a colour.
inkFor :: HDC -> Colour -> IO ()
inkFor dc c = c_SetBkMode dc #{const TRANSPARENT} >> () <$ c_SetTextColor dc c

rect :: CInt -> CInt -> CInt -> CInt -> RECT
rect x y w h = (fromIntegral x, fromIntegral y, fromIntegral (x + w), fromIntegral (y + h))

-- | DrawTextW, giving back the rectangle as DT_CALCRECT leaves it.
drawText :: HDC -> String -> RECT -> UINT -> IO RECT
drawText dc s r flags =
    withCWStringLen s $ \(str, n) -> allocaRECT $ \p -> do
        pokeRECT p r
        _ <- c_DrawText dc (castPtr str) (fromIntegral n) p flags
        peekRECT p
