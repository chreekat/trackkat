{-# LANGUAGE LambdaCase #-}

-- | The live view across the window, with window = haskell: the head view
-- in its corner ("TrackKat.UI.HeadView"), and beside it the axes, lights,
-- figures and switches ("TrackKat.UI.Live"). The switches and head view are
-- cbits/gui.c's buttons, drawn here.
module TrackKat.Gui.Live (registerLive) where

import Data.Bits ((.&.))
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr, nullPtr, wordPtrToPtr)
import Graphics.Win32.GDI.Types (HWND)
import Graphics.Win32.Message (wM_COMMAND, wM_DRAWITEM, wM_ERASEBKGND, wM_PAINT, wM_SETCURSOR)
import Graphics.Win32.Window (WindowClosure, defWindowProc, getClientRect, getParent, sendMessage)
import System.Win32.Types (HANDLE, LPCTSTR)

import TrackKat.Config (HeadView (..))
import TrackKat.Gui.Kit (Kit, kitDpi)
import TrackKat.Gui.Paint
import TrackKat.Gui.Shown (snapshot)
import TrackKat.Gui.Win32 (drawItemDC, drawItemRect, drawItemState, drawItemWindow, idcHand, odsFocus, odsNoFocusRect, odsSelected)
import TrackKat.UI.Draw (Shape (..), scaledAt)
import TrackKat.UI.HeadView
import TrackKat.UI.Live

foreign import ccall unsafe "hts_gui_head_view" c_head_view :: IO CInt
foreign import ccall unsafe "hts_gui_live_control" c_live_control :: HWND -> IO CInt
foreign import ccall unsafe "windows.h SetCursor" c_SetCursor :: HANDLE -> IO HANDLE
foreign import ccall unsafe "windows.h LoadCursorW" c_LoadCursor :: Ptr () -> LPCTSTR -> IO HANDLE

-- | Register the live view's window class, reporting what fails in it.
registerLive :: Kit -> (String -> IO ()) -> IO (Either Int ())
registerLive kit report = registerPart report "The live view" "trackkat_live" Unfilled (liveProc kit)

-- | One of the live view's controls drawn here.
data Control = SwitchControl Switch | HeadViewControl

control :: HWND -> IO (Maybe Control)
control w =
    c_live_control w >>= \i -> pure $ case i of
        4 -> Just HeadViewControl
        _ | i >= 0 && i < 4 -> Just (SwitchControl (toEnum (fromIntegral i)))
        _ -> Nothing

liveProc :: Kit -> WindowClosure
liveProc kit w msg wp lp
    | msg == wM_ERASEBKGND = pure 1
    | msg == wM_PAINT = do
        painted kit w $ \dc -> do
            (_, _, right, bottom) <- getClientRect w
            let dpi = kitDpi kit
            live <- snapshot
            buffered dc (0, 0, fromIntegral right, fromIntegral bottom) $ \mem ->
                mapM_ (draw mem) (livePane dpi (fromIntegral right, fromIntegral bottom) live)
        pure 0
    | msg == wM_DRAWITEM = do
        let item = wordPtrToPtr (fromIntegral lp)
        drawItemWindow item >>= control >>= mapM_ (drawItem kit item)
        pure 1
    -- A hand over the head view, which a click switches.
    | msg == wM_SETCURSOR =
        control (wordPtrToPtr (fromIntegral wp)) >>= \case
            Just HeadViewControl -> 1 <$ (c_SetCursor =<< c_LoadCursor nullPtr idcHand)
            _ -> defWindowProc (Just w) msg wp lp
    -- The switches' and head view's clicks are the window's to act on.
    | msg == wM_COMMAND = getParent w >>= \parent -> sendMessage parent msg wp lp
    | otherwise = defWindowProc (Just w) msg wp lp

-- | A switch or the head view, with a focus rectangle while it has the
-- keyboard. The head view is drawn off screen first, so it doesn't flicker.
drawItem :: Kit -> Ptr () -> Control -> IO ()
drawItem kit item which = do
    dc <- Canvas kit <$> drawItemDC item
    (left, top, right, bottom) <- drawItemRect item
    itemState <- drawItemState item
    let dpi = kitDpi kit
    live <- snapshot
    view <- headViewShown
    let focused = itemState .&. odsFocus /= 0 && itemState .&. odsNoFocusRect == 0
        s = scaledAt dpi
        (l, t, r, b) = (fromIntegral left, fromIntegral top, fromIntegral right, fromIntegral bottom)
    case which of
        HeadViewControl ->
            buffered dc (left, top, right - left, bottom - top) $ \mem ->
                mapM_ (draw mem) $
                    headScene dpi (min (r - l) (b - t)) view live
                        ++ [Focus (s 2, s 2, r - l - s 2, b - t - s 2) | focused]
        SwitchControl sw ->
            mapM_ (draw dc) $
                switchFace dpi (l, t, r, b) (if itemState .&. odsSelected /= 0 then Pressed else Released) live sw
                    ++ [Focus (l + s 4, t + s 4, r - s 4, b - s 4) | focused]

-- | The head view shown, as cbits/gui.c numbers them.
headViewShown :: IO HeadView
headViewShown =
    c_head_view >>= \case
        1 -> pure PhysicalView
        2 -> pure CameraView
        _ -> pure VirtualView
