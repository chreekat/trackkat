-- | What the parts of trackkat's window drawn in Haskell take from
-- Windows' headers that the Win32 package doesn't give.
module TrackKat.Gui.Win32 (
    -- * Window classes
    withWndClass,
    gwlpUserData,
    idcHand,

    -- * Messages
    wmPrintClient,
    wmCtlColorBtn,

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

    -- * Drawn buttons
    drawItemWindow,
    drawItemDC,
    drawItemRect,
    drawItemState,
    odsSelected,
    odsFocus,
    odsNoFocusRect,
) where

#include <windows.h>

import Foreign.C.Types (CInt)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (FunPtr, Ptr, wordPtrToPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)
import Graphics.Win32.GDI.Types (HCURSOR, HDC, HWND)
import Graphics.Win32.Message (WindowMessage)
import Graphics.Win32.Window (WindowClosure)
import System.Win32.Types (HINSTANCE, INT, LPCTSTR, UINT)

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

gwlpUserData :: INT
gwlpUserData = #{const GWLP_USERDATA}

idcHand :: LPCTSTR
idcHand = wordPtrToPtr #{const IDC_HAND}

wmPrintClient, wmCtlColorBtn :: WindowMessage
wmPrintClient = #{const WM_PRINTCLIENT}
wmCtlColorBtn = #{const WM_CTLCOLORBTN}

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

-- | A DRAWITEMSTRUCT's control.
drawItemWindow :: Ptr () -> IO HWND
drawItemWindow = #{peek DRAWITEMSTRUCT, hwndItem}

drawItemDC :: Ptr () -> IO HDC
drawItemDC = #{peek DRAWITEMSTRUCT, hDC}

-- | A DRAWITEMSTRUCT's left, top, right and bottom.
drawItemRect :: Ptr () -> IO (CInt, CInt, CInt, CInt)
drawItemRect p =
    (,,,)
        <$> #{peek DRAWITEMSTRUCT, rcItem.left} p
        <*> #{peek DRAWITEMSTRUCT, rcItem.top} p
        <*> #{peek DRAWITEMSTRUCT, rcItem.right} p
        <*> #{peek DRAWITEMSTRUCT, rcItem.bottom} p

drawItemState :: Ptr () -> IO UINT
drawItemState = #{peek DRAWITEMSTRUCT, itemState}

odsSelected, odsFocus, odsNoFocusRect :: UINT
odsSelected = #{const ODS_SELECTED}
odsFocus = #{const ODS_FOCUS}
odsNoFocusRect = #{const ODS_NOFOCUSRECT}
