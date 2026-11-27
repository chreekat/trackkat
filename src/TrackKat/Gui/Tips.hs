-- | Help shown when the mouse rests on a control, in the windows made in
-- Haskell.
module TrackKat.Gui.Tips (
    Tips,
    makeTips,
    tip,
) where

import Control.Monad (void)
import Data.Bits ((.|.))
import Foreign.C.String (withCWString)
import Graphics.Win32.GDI.Types (HWND)
import Graphics.Win32.Window (cW_USEDEFAULT, wS_EX_TOPMOST, wS_POPUP)

import TrackKat.Gui.Calls (createWindow, pointerParam, send)
import TrackKat.Gui.Paint (scaled)
import TrackKat.Gui.Win32 (ttdtAutoPop, ttmAddTool, ttmSetDelayTime, ttmSetMaxTipWidth, ttsAlwaysTip, ttsNoPrefix, withToolInfo)

-- | The tooltips over a window.
data Tips = Tips HWND HWND

-- | Tooltips over a window, the longest wrapped, each shown for half a
-- minute.
makeTips :: HWND -> IO Tips
makeTips owner = do
    t <- createWindow wS_EX_TOPMOST "tooltips_class32" "" (wS_POPUP .|. ttsAlwaysTip .|. ttsNoPrefix) (d, d, d, d) (Just owner) 0
    width <- scaled 360
    _ <- send t ttmSetMaxTipWidth 0 (fromIntegral width)
    _ <- send t ttmSetDelayTime (fromIntegral ttdtAutoPop) 30000
    pure (Tips t owner)
  where
    d = fromIntegral cW_USEDEFAULT

-- | Show help when the mouse rests on a control; a label needs SS_NOTIFY to
-- notice the mouse.
tip :: Tips -> HWND -> String -> IO ()
tip (Tips t owner) control text =
    withCWString text $ \s -> withToolInfo owner control s $ \info -> void (send t ttmAddTool 0 (pointerParam info))
