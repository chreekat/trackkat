-- | What the parts of trackkat's window drawn in Haskell take from
-- Windows' headers that the Win32 package doesn't give.
module TrackKat.Gui.Win32 (
    -- * Setting up
    withActCtx,
    withInitCommonControls,
    logPixelsY,
    LogFont,
    withNonClientMetrics,
    messageFontOf,
    spiGetNonClientMetrics,
    logFontSize,
    logFontHeight,
    setLogFontHeight,
    setLogFontBold,
    setLogFontFixed,
    setLogFontFace,

    -- * Window classes
    withWndClass,
    gwlpUserData,
    idcHand,

    -- * Messages
    wmPrintClient,
    wmCtlColorBtn,
    wmCtlColorStatic,
    wmContextMenu,
    wmMouseWheel,
    setMinTrackSize,
    setMaxTrackSize,
    iconSmall,
    sizeMinimized,

    -- * Scrolling
    sbLineUp,
    sbLineDown,
    sbPageUp,
    sbPageDown,
    sbThumbTrack,
    sbThumbPosition,
    sbTop,
    sbBottom,
    swScrollChildren,
    swInvalidate,
    swErase,
    withScrollRange,

    -- * The screen
    spiGetWorkArea,
    gwlStyle,

    -- * Buttons and edit controls
    bmGetCheck,
    bmSetCheck,
    bstChecked,
    ssNotify,
    ssEtchedHorz,
    emGetSel,
    emSetSel,
    emReplaceSel,
    emGetFirstVisibleLine,
    emLineFromChar,
    emLineIndex,
    emLineScroll,
    emScrollCaret,
    emSetLimitText,
    sbVert,
    withScrollInfo,

    -- * Tooltips
    ttsAlwaysTip,
    ttsNoPrefix,
    ttmSetMaxTipWidth,
    ttmSetDelayTime,
    ttdtAutoPop,
    ttmAddTool,
    withToolInfo,

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
#include <commctrl.h>

import Data.Bits ((.|.))
import Foreign.C.String (withCWString, withCWStringLen)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (copyArray)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (FunPtr, Ptr, castPtr, plusPtr, wordPtrToPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)
import Data.Word (Word8)
import Graphics.Win32.GDI.Types (HBRUSH, HCURSOR, HDC, HICON, HWND)
import Graphics.Win32.Message (WindowMessage)
import Graphics.Win32.Window (WindowClosure)
import System.Win32.Types (DWORD, HINSTANCE, INT, LPCTSTR, UINT, WPARAM)

-- | An ACTCTXW for a manifest among a module's resources, by its path and
-- resource number.
withActCtx :: String -> Int -> (Ptr () -> IO a) -> IO a
withActCtx source resource act =
    withCWString source $ \s -> allocaBytes size $ \p -> do
        fillBytes p 0 size
        #{poke ACTCTXW, cbSize} p (fromIntegral size :: UINT)
        #{poke ACTCTXW, dwFlags} p (#{const ACTCTX_FLAG_RESOURCE_NAME_VALID} :: DWORD)
        #{poke ACTCTXW, lpSource} p s
        #{poke ACTCTXW, lpResourceName} p (wordPtrToPtr (fromIntegral resource) :: LPCTSTR)
        act p
  where
    size = #{size ACTCTXW}

-- | An INITCOMMONCONTROLSEX for the classes the window uses: trackbars,
-- tooltips, up-down arrows and the standard controls.
withInitCommonControls :: (Ptr () -> IO a) -> IO a
withInitCommonControls act =
    allocaBytes size $ \p -> do
        #{poke INITCOMMONCONTROLSEX, dwSize} p (fromIntegral size :: DWORD)
        #{poke INITCOMMONCONTROLSEX, dwICC} p (#{const ICC_BAR_CLASSES} .|. #{const ICC_STANDARD_CLASSES} .|. #{const ICC_UPDOWN_CLASS} :: DWORD)
        act p
  where
    size = #{size INITCOMMONCONTROLSEX}

logPixelsY :: CInt
logPixelsY = #{const LOGPIXELSY}

-- | A LOGFONTW.
data LogFont

-- | A NONCLIENTMETRICSW, sized, and its size.
withNonClientMetrics :: (Ptr () -> UINT -> IO a) -> IO a
withNonClientMetrics act =
    allocaBytes size $ \p -> do
        fillBytes p 0 size
        #{poke NONCLIENTMETRICSW, cbSize} p (fromIntegral size :: UINT)
        act p (fromIntegral size)
  where
    size = #{size NONCLIENTMETRICSW}

-- | A NONCLIENTMETRICSW's font for messages.
messageFontOf :: Ptr () -> Ptr LogFont
messageFontOf p = p `plusPtr` #{offset NONCLIENTMETRICSW, lfMessageFont}

spiGetNonClientMetrics :: UINT
spiGetNonClientMetrics = #{const SPI_GETNONCLIENTMETRICS}

logFontSize :: Int
logFontSize = #{size LOGFONTW}

-- | Its height: negative for the characters', positive for the cells'.
logFontHeight :: Ptr LogFont -> IO CInt
logFontHeight = #{peek LOGFONTW, lfHeight}

setLogFontHeight :: Ptr LogFont -> CInt -> IO ()
setLogFontHeight = #{poke LOGFONTW, lfHeight}

setLogFontBold :: Ptr LogFont -> IO ()
setLogFontBold p = #{poke LOGFONTW, lfWeight} p (#{const FW_BOLD} :: CInt)

setLogFontFixed :: Ptr LogFont -> IO ()
setLogFontFixed p = #{poke LOGFONTW, lfPitchAndFamily} p (#{const FIXED_PITCH} .|. #{const FF_MODERN} :: Word8)

-- | Its face's name, cut to fit lfFaceName.
setLogFontFace :: Ptr LogFont -> String -> IO ()
setLogFontFace p face =
    withCWStringLen face $ \(s, n) -> do
        let field = castPtr (p `plusPtr` #{offset LOGFONTW, lfFaceName})
            room = #{const LF_FACESIZE} - 1
        fillBytes field 0 (#{const LF_FACESIZE} * 2)
        copyArray field s (min n room)

-- | A WNDCLASSW of a procedure, instance, cursor, background brush, icon
-- (or null) and name, the rest zero.
withWndClass :: FunPtr WindowClosure -> HINSTANCE -> HCURSOR -> HBRUSH -> HICON -> LPCTSTR -> (Ptr () -> IO a) -> IO a
withWndClass proc inst cursor background icon name act =
    allocaBytes size $ \p -> do
        fillBytes p 0 size
        #{poke WNDCLASSW, lpfnWndProc} p proc
        #{poke WNDCLASSW, hInstance} p inst
        #{poke WNDCLASSW, hCursor} p cursor
        #{poke WNDCLASSW, hbrBackground} p background
        #{poke WNDCLASSW, hIcon} p icon
        #{poke WNDCLASSW, lpszClassName} p name
        act p
  where
    size = #{size WNDCLASSW}

gwlpUserData :: INT
gwlpUserData = #{const GWLP_USERDATA}

idcHand :: LPCTSTR
idcHand = wordPtrToPtr #{const IDC_HAND}

wmPrintClient, wmCtlColorBtn, wmCtlColorStatic, wmContextMenu, wmMouseWheel :: WindowMessage
wmMouseWheel = #{const WM_MOUSEWHEEL}
wmPrintClient = #{const WM_PRINTCLIENT}
wmCtlColorBtn = #{const WM_CTLCOLORBTN}
wmCtlColorStatic = #{const WM_CTLCOLORSTATIC}
wmContextMenu = #{const WM_CONTEXTMENU}

-- | Set a MINMAXINFO's smallest size the window can be dragged to.
setMinTrackSize :: Ptr () -> CInt -> CInt -> IO ()
setMinTrackSize p w h = do
    #{poke MINMAXINFO, ptMinTrackSize.x} p w
    #{poke MINMAXINFO, ptMinTrackSize.y} p h

-- | Set a MINMAXINFO's largest size the window can be dragged to.
setMaxTrackSize :: Ptr () -> CInt -> CInt -> IO ()
setMaxTrackSize p w h = do
    #{poke MINMAXINFO, ptMaxTrackSize.x} p w
    #{poke MINMAXINFO, ptMaxTrackSize.y} p h

sizeMinimized :: WPARAM
sizeMinimized = #{const SIZE_MINIMIZED}

iconSmall :: WPARAM
iconSmall = #{const ICON_SMALL}

sbLineUp, sbLineDown, sbPageUp, sbPageDown, sbThumbTrack, sbThumbPosition, sbTop, sbBottom :: WPARAM
sbLineUp = #{const SB_LINEUP}
sbLineDown = #{const SB_LINEDOWN}
sbPageUp = #{const SB_PAGEUP}
sbPageDown = #{const SB_PAGEDOWN}
sbThumbTrack = #{const SB_THUMBTRACK}
sbThumbPosition = #{const SB_THUMBPOSITION}
sbTop = #{const SB_TOP}
sbBottom = #{const SB_BOTTOM}

swScrollChildren, swInvalidate, swErase :: UINT
swScrollChildren = #{const SW_SCROLLCHILDREN}
swInvalidate = #{const SW_INVALIDATE}
swErase = #{const SW_ERASE}

-- | A SCROLLINFO setting a scroll bar's range, from 0 to a largest
-- position, and its page.
withScrollRange :: CInt -> UINT -> (Ptr () -> IO a) -> IO a
withScrollRange top page act =
    allocaBytes size $ \p -> do
        fillBytes p 0 size
        #{poke SCROLLINFO, cbSize} p (fromIntegral size :: UINT)
        #{poke SCROLLINFO, fMask} p (#{const SIF_RANGE} .|. #{const SIF_PAGE} :: UINT)
        #{poke SCROLLINFO, nMax} p top
        #{poke SCROLLINFO, nPage} p page
        act p
  where
    size = #{size SCROLLINFO}

spiGetWorkArea :: UINT
spiGetWorkArea = #{const SPI_GETWORKAREA}

gwlStyle :: INT
gwlStyle = #{const GWL_STYLE}

bmGetCheck, bmSetCheck :: WindowMessage
bmGetCheck = #{const BM_GETCHECK}
bmSetCheck = #{const BM_SETCHECK}

bstChecked :: WPARAM
bstChecked = #{const BST_CHECKED}

ssNotify, ssEtchedHorz :: DWORD
ssNotify = #{const SS_NOTIFY}
ssEtchedHorz = #{const SS_ETCHEDHORZ}

emGetSel, emSetSel, emReplaceSel, emGetFirstVisibleLine, emLineFromChar, emLineIndex, emLineScroll, emScrollCaret, emSetLimitText :: WindowMessage
emGetSel = #{const EM_GETSEL}
emSetSel = #{const EM_SETSEL}
emReplaceSel = #{const EM_REPLACESEL}
emGetFirstVisibleLine = #{const EM_GETFIRSTVISIBLELINE}
emLineFromChar = #{const EM_LINEFROMCHAR}
emLineIndex = #{const EM_LINEINDEX}
emLineScroll = #{const EM_LINESCROLL}
emScrollCaret = #{const EM_SCROLLCARET}
emSetLimitText = #{const EM_SETLIMITTEXT}

sbVert :: CInt
sbVert = #{const SB_VERT}

-- | A SCROLLINFO asking for everything, to fill, and then its page,
-- position, largest position, and where its box is being dragged.
withScrollInfo :: (Ptr () -> IO a) -> IO (a, (UINT, CInt, CInt, CInt))
withScrollInfo act =
    allocaBytes size $ \p -> do
        fillBytes p 0 size
        #{poke SCROLLINFO, cbSize} p (fromIntegral size :: UINT)
        #{poke SCROLLINFO, fMask} p (#{const SIF_ALL} :: UINT)
        r <- act p
        page <- #{peek SCROLLINFO, nPage} p
        pos <- #{peek SCROLLINFO, nPos} p
        top <- #{peek SCROLLINFO, nMax} p
        dragged <- #{peek SCROLLINFO, nTrackPos} p
        pure (r, (page, pos, top, dragged))
  where
    size = #{size SCROLLINFO}

ttsAlwaysTip, ttsNoPrefix :: DWORD
ttsAlwaysTip = #{const TTS_ALWAYSTIP}
ttsNoPrefix = #{const TTS_NOPREFIX}

ttmSetMaxTipWidth, ttmSetDelayTime, ttmAddTool :: WindowMessage
ttmSetMaxTipWidth = #{const TTM_SETMAXTIPWIDTH}
ttmSetDelayTime = #{const TTM_SETDELAYTIME}
ttmAddTool = #{const TTM_ADDTOOLW}

ttdtAutoPop :: CInt
ttdtAutoPop = #{const TTDT_AUTOPOP}

-- | A TTTOOLINFOW for help on a control, shown over a window, with a text:
-- the size every version of the common controls understands, the control
-- as its ID, and the tooltip watching its mouse itself.
withToolInfo :: HWND -> HWND -> LPCTSTR -> (Ptr () -> IO a) -> IO a
withToolInfo owner control text act =
    allocaBytes size $ \p -> do
        fillBytes p 0 size
        #{poke TTTOOLINFOW, cbSize} p (#{const TTTOOLINFOW_V2_SIZE} :: UINT)
        #{poke TTTOOLINFOW, uFlags} p (#{const TTF_IDISHWND} .|. #{const TTF_SUBCLASS} :: UINT)
        #{poke TTTOOLINFOW, hwnd} p owner
        #{poke TTTOOLINFOW, uId} p control
        #{poke TTTOOLINFOW, lpszText} p text
        act p
  where
    size = #{size TTTOOLINFOW}

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
