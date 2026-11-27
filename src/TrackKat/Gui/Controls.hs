-- | The standard controls in the groups of the window made in Haskell, and
-- the settings they show.
module TrackKat.Gui.Controls (
    control,
    Switch (..),
    makeSwitch,
) where

import Data.Bits ((.|.))
import Graphics.Win32.Control (bS_AUTOCHECKBOX)
import Graphics.Win32.GDI.Types (HWND)
import Graphics.Win32.Message (wM_SETFONT)
import Graphics.Win32.Window (wS_CHILD, wS_CLIPSIBLINGS, wS_EX_CLIENTEDGE, wS_TABSTOP, wS_VISIBLE)
import System.Win32.Types (DWORD)

import TrackKat.Gui.Calls (createWindow, pointerParam, send)
import TrackKat.Gui.Kit (Kit, fontHandle, scaled)
import TrackKat.Gui.Settings (Field (..), Settings, addField)
import TrackKat.Gui.Tips (Tips, tip)
import TrackKat.Gui.Win32 (bmGetCheck, bmSetCheck, bstChecked)
import TrackKat.UI.Draw (Font (..))
import TrackKat.UI.Settings (Check (..), Toggle, toggleChecked, toggleValue)

-- | A visible control in a window, of a class, text and style, at x, y,
-- w, h at 96 dpi, with an ID and a font; an edit box sunken.
control :: Kit -> HWND -> String -> String -> DWORD -> (Int, Int, Int, Int) -> Int -> Font -> IO HWND
control kit parent cls text style (x, y, w, h) ident font = do
    c <- createWindow (if cls == "EDIT" then wS_EX_CLIENTEDGE else 0) cls text (wS_CHILD .|. wS_VISIBLE .|. wS_CLIPSIBLINGS .|. style) (s x, s y, s w, s h) (Just parent) ident
    _ <- send c wM_SETFONT (pointerParam (fontHandle kit font)) 0
    pure c
  where
    s = scaled kit . fromIntegral

-- | A check box showing a setting.
data Switch = Switch
    { switchId :: Int
    , switchSetting :: String
    , switchToggle :: Toggle
    , switchWords :: String
    , switchPlace :: (Int, Int, Int, Int)
    -- ^ x, y, w, h at 96 dpi
    , switchHelp :: String
    }

-- | Make a switch's check box in a window, with its help, showing its
-- setting.
makeSwitch :: Kit -> HWND -> Tips -> Settings -> Switch -> IO ()
makeSwitch kit w tips settings sw = do
    box <- control kit w "BUTTON" (switchWords sw) (wS_TABSTOP .|. bS_AUTOCHECKBOX) (switchPlace sw) (switchId sw) UiFont
    tip tips box (switchHelp sw)
    addField settings (toggleField (switchSetting sw) (switchToggle sw) box)

-- | A setting of a name shown by a check box.
toggleField :: String -> Toggle -> HWND -> Field
toggleField name toggle box =
    Field
        { fieldName = name
        , showValue = \v -> () <$ send box bmSetCheck (case toggleChecked toggle v of Checked -> bstChecked; Unchecked -> 0) 0
        , valueShown = (\c -> toggleValue toggle (if c == bstChecked then Checked else Unchecked)) . fromIntegral <$> send box bmGetCheck 0 0
        }
