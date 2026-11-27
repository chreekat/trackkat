-- | What the window made in Haskell is drawn with: the screen's scale and
-- the window's fonts.
module TrackKat.Gui.Kit (
    Kit,
    kitDpi,
    setUp,
    scaled,
    fontHandle,
    fontHandles,
) where

import Control.Monad (unless, void, when)
import Foreign.C.String (CString, withCAString, withCWString)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Utils (copyBytes, fillBytes)
import Foreign.Ptr (FunPtr, Ptr, castPtrToFunPtr, nullFunPtr, nullPtr)
import Graphics.Win32.GDI.Types (HDC, HWND)
import System.Win32.Info (getSystemDirectory)
import System.Win32.Types (BOOL, HANDLE, HINSTANCE, LPCTSTR, UINT, iNVALID_HANDLE_VALUE)

import TrackKat.Gui.Win32
import TrackKat.UI.Draw (Font (..), scaledAt)

foreign import ccall unsafe "windows.h GetDC" c_GetDC :: HWND -> IO HDC
foreign import ccall unsafe "windows.h ReleaseDC" c_ReleaseDC :: HWND -> HDC -> IO CInt
foreign import ccall unsafe "windows.h GetDeviceCaps" c_GetDeviceCaps :: HDC -> CInt -> IO CInt
foreign import ccall unsafe "windows.h CreateActCtxW" c_CreateActCtx :: Ptr () -> IO HANDLE
foreign import ccall unsafe "windows.h ActivateActCtx" c_ActivateActCtx :: HANDLE -> Ptr () -> IO BOOL
foreign import ccall unsafe "windows.h LoadLibraryW" c_LoadLibrary :: LPCTSTR -> IO HINSTANCE
foreign import ccall unsafe "windows.h GetProcAddress" c_GetProcAddress :: HINSTANCE -> CString -> IO (Ptr ())
foreign import ccall unsafe "dynamic" callInit :: FunPtr (Ptr () -> IO BOOL) -> Ptr () -> IO BOOL
foreign import ccall unsafe "windows.h CreateFontIndirectW" c_CreateFontIndirect :: Ptr LogFont -> IO HANDLE
foreign import ccall unsafe "windows.h SystemParametersInfoW" c_SystemParametersInfo :: UINT -> UINT -> Ptr () -> UINT -> IO BOOL

-- | The screen's dots per inch, and a font for each 'Font'.
data Kit = Kit
    { kitDpi :: Int
    , kitFonts :: Fonts
    }

data Fonts = Fonts {uiFont, boldFont, smallFont, capsFont, tinyFont, monoFont :: HANDLE}

-- | Set the thread up to make the window on, and make the kit it is drawn
-- with, for the life of the process.
setUp :: IO Kit
setUp = do
    dpi <- screenDpi
    useModernControls
    initCommonControls
    Kit dpi <$> makeFonts dpi

screenDpi :: IO Int
screenDpi = do
    screen <- c_GetDC nullPtr
    dpi <- c_GetDeviceCaps screen logPixelsY
    _ <- c_ReleaseDC nullPtr screen
    pure (fromIntegral dpi)

-- | Buttons and sliders that look like the rest of Windows: version 6 of
-- the common controls, through the manifest Windows keeps in shell32 for
-- this. Without it they look like Windows 95.
useModernControls :: IO ()
useModernControls = do
    system <- getSystemDirectory
    withActCtx (system ++ "\\shell32.dll") 124 $ \ctx -> do
        h <- c_CreateActCtx ctx
        unless (h == iNVALID_HANDLE_VALUE) $ alloca $ \cookie -> void (c_ActivateActCtx h cookie)

-- | Load the common controls that 'useModernControls' chose, as a library
-- loaded once it has: linked, the process would load version 5 at start.
initCommonControls :: IO ()
initCommonControls = do
    comctl <- withCWString "comctl32.dll" c_LoadLibrary
    unless (comctl == nullPtr) $ do
        initEx <- castPtrToFunPtr <$> withCAString "InitCommonControlsEx" (c_GetProcAddress comctl)
        when (initEx /= nullFunPtr) $ withInitCommonControls (void . callInit initEx)

-- | The fonts Windows shows messages in, or Segoe UI if it doesn't say:
-- that, in bold, smaller, smaller in bold, smaller still, and Consolas.
makeFonts :: Int -> IO Fonts
makeFonts dpi =
    allocaBytes logFontSize $ \base -> do
        found <- withNonClientMetrics $ \metrics size -> do
            ok <- c_SystemParametersInfo spiGetNonClientMetrics size metrics 0
            when ok (copyBytes base (messageFontOf metrics) logFontSize)
            pure ok
        unless found $ do
            fillBytes base 0 logFontSize
            setLogFontHeight base (negate (fromIntegral (scaledAt dpi 12)))
            setLogFontFace base "Segoe UI"
        height <- logFontHeight base
        let variant :: (Ptr LogFont -> IO ()) -> IO HANDLE
            variant change =
                allocaBytes logFontSize $ \lf -> do
                    copyBytes lf base logFontSize
                    change lf
                    c_CreateFontIndirect lf
            smaller lf = setLogFontHeight lf (height * 9 `quot` 10)
        Fonts
            <$> variant (const (pure ()))
            <*> variant setLogFontBold
            <*> variant smaller
            <*> variant (\lf -> smaller lf >> setLogFontBold lf)
            <*> variant (\lf -> setLogFontHeight lf (height * 3 `quot` 4))
            <*> variant (\lf -> setLogFontFixed lf >> setLogFontFace lf "Consolas")

-- | A length at 96 dots per inch, at the screen's.
scaled :: Kit -> CInt -> CInt
scaled kit = fromIntegral . scaledAt (kitDpi kit) . fromIntegral

fontHandle :: Kit -> Font -> HANDLE
fontHandle kit font = case font of
    UiFont -> uiFont fonts
    BoldFont -> boldFont fonts
    SmallFont -> smallFont fonts
    CapsFont -> capsFont fonts
    TinyFont -> tinyFont fonts
    MonoFont -> monoFont fonts
  where
    fonts = kitFonts kit

-- | Every font, in the order of 'Font'.
fontHandles :: Kit -> [HANDLE]
fontHandles kit = map (fontHandle kit) [UiFont ..]
