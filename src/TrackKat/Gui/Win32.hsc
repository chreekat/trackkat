-- | What the parts of trackkat's window drawn in Haskell take from
-- Windows' headers that the Win32 package doesn't give.
module TrackKat.Gui.Win32 (
    -- * Window classes
    withWndClass,

    -- * Text and icons
    dtLeft,
    dtCenter,
    dtRight,
    dtSingleLine,
    dtVCenter,
    dtNoPrefix,
    dtCalcRect,
    dtEndEllipsis,
    dtWordBreak,
    dtEditControl,
    diNormal,
) where

#include <windows.h>

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (FunPtr, Ptr)
import Foreign.Storable (pokeByteOff)
import Graphics.Win32.GDI.Types (HCURSOR)
import Graphics.Win32.Window (WindowClosure)
import System.Win32.Types (HINSTANCE, LPCTSTR, UINT)

-- | A WNDCLASSW of a procedure, instance, cursor and name, the rest zero.
withWndClass :: FunPtr WindowClosure -> HINSTANCE -> HCURSOR -> LPCTSTR -> (Ptr () -> IO a) -> IO a
withWndClass proc inst cursor name act =
    allocaBytes size $ \p -> do
        fillBytes p 0 size
        #{poke WNDCLASSW, lpfnWndProc} p proc
        #{poke WNDCLASSW, hInstance} p inst
        #{poke WNDCLASSW, hCursor} p cursor
        #{poke WNDCLASSW, lpszClassName} p name
        act p
  where
    size = #{size WNDCLASSW}

dtLeft, dtCenter, dtRight, dtSingleLine, dtVCenter, dtNoPrefix, dtCalcRect, dtEndEllipsis, dtWordBreak, dtEditControl :: UINT
dtLeft = #{const DT_LEFT}
dtCenter = #{const DT_CENTER}
dtRight = #{const DT_RIGHT}
dtSingleLine = #{const DT_SINGLELINE}
dtVCenter = #{const DT_VCENTER}
dtNoPrefix = #{const DT_NOPREFIX}
dtCalcRect = #{const DT_CALCRECT}
dtEndEllipsis = #{const DT_END_ELLIPSIS}
dtWordBreak = #{const DT_WORDBREAK}
dtEditControl = #{const DT_EDITCONTROL}

diNormal :: UINT
diNormal = #{const DI_NORMAL}
