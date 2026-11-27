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
    wmContextMenu,

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

    -- * The tray icon
    withNotifyIconData,
    nimAdd,
    nimModify,
    nimDelete,
    nifIcon,
    nifTip,
    nifMessage,
) where

#include <windows.h>
#include <shellapi.h>

import Foreign.C.String (withCWStringLen)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (copyArray)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (FunPtr, Ptr, castPtr, plusPtr, wordPtrToPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)
import Graphics.Win32.GDI.Types (HCURSOR, HDC, HICON, HWND)
import Graphics.Win32.Message (WindowMessage)
import Graphics.Win32.Window (WindowClosure)
import System.Win32.Types (DWORD, HINSTANCE, INT, LPCTSTR, UINT)

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

wmPrintClient, wmCtlColorBtn, wmContextMenu :: WindowMessage
wmPrintClient = #{const WM_PRINTCLIENT}
wmCtlColorBtn = #{const WM_CTLCOLORBTN}
wmContextMenu = #{const WM_CONTEXTMENU}

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

-- | A NOTIFYICONDATAW of a window, icon ID, flags, callback message, icon
-- and tooltip, the rest zero; the tooltip cut to fit szTip.
withNotifyIconData :: HWND -> UINT -> UINT -> WindowMessage -> HICON -> String -> (Ptr () -> IO a) -> IO a
withNotifyIconData w uid flags callback icon tip act =
    allocaBytes size $ \p -> do
        fillBytes p 0 size
        #{poke NOTIFYICONDATAW, cbSize} p (fromIntegral size :: DWORD)
        #{poke NOTIFYICONDATAW, hWnd} p w
        #{poke NOTIFYICONDATAW, uID} p uid
        #{poke NOTIFYICONDATAW, uFlags} p flags
        #{poke NOTIFYICONDATAW, uCallbackMessage} p callback
        #{poke NOTIFYICONDATAW, hIcon} p icon
        withCWStringLen tip $ \(s, n) ->
            copyArray (castPtr (p `plusPtr` #{offset NOTIFYICONDATAW, szTip})) s (min n (tipMax - 1))
        act p
  where
    size = #{size NOTIFYICONDATAW}

-- | NOTIFYICONDATAW's szTip, in characters with the NUL.
tipMax :: Int
tipMax = 128

nimAdd, nimModify, nimDelete :: DWORD
nimAdd = #{const NIM_ADD}
nimModify = #{const NIM_MODIFY}
nimDelete = #{const NIM_DELETE}

nifIcon, nifTip, nifMessage :: UINT
nifIcon = #{const NIF_ICON}
nifTip = #{const NIF_TIP}
nifMessage = #{const NIF_MESSAGE}
