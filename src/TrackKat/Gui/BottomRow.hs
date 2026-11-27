-- | The row along the bottom of trackkat's window, with window = haskell:
-- how trackkat runs, editing the settings file, and quitting.
module TrackKat.Gui.BottomRow (makeBottomRow) where

import Data.Bits ((.&.), (.|.))
import Graphics.Win32.Control (bS_PUSHBUTTON)
import Graphics.Win32.GDI.Types (HWND)
import Graphics.Win32.Message (wM_COMMAND, wM_CREATE)
import Graphics.Win32.Window (WindowClosure, defWindowProc, wS_TABSTOP)
import System.Win32.Types (WPARAM)

import TrackKat.Gui.Controls (Switch (..), control, makeSwitch)
import TrackKat.Gui.Kit (Kit)
import TrackKat.Gui.Paint (Font (..), makeGroup)
import TrackKat.Gui.Settings
import TrackKat.Gui.Tips (makeTips, tip)
import TrackKat.Gui.Win32 (ssEtchedHorz)
import TrackKat.UI.Settings (Check (..), Toggle (..))
import TrackKat.UI.Window (bottomRowHeight, contentHeight, contentWidth)

-- While cbits/gui.c keeps settings of its own, they are saved with these.
foreign import ccall unsafe "hts_gui_save_now" c_save_now :: IO ()

-- | The row, across the bottom of a window, reporting what fails
-- in it; Quit asks with the first action given, and Edit settings file with
-- the second, once the changes here and in cbits/gui.c are saved. Left is a
-- Windows error code.
makeBottomRow :: Kit -> (String -> IO ()) -> Settings -> IO () -> IO () -> HWND -> IO (Either Int HWND)
makeBottomRow kit report settings quit editSettings window =
    makeGroup kit report "The bottom row" "trackkat_bottom" window (0, contentHeight - bottomRowHeight, contentWidth, bottomRowHeight) (rowProc kit settings quit editSettings)

rowProc :: Kit -> Settings -> IO () -> IO () -> WindowClosure
rowProc kit settings quit editSettings w msg wp lp
    | msg == wM_CREATE = build kit settings w >> pure 0
    | msg == wM_COMMAND = do
        let ident = wp .&. 0xFFFF
        case lookup ident [(fromIntegral (switchId sw), switchSetting sw) | sw <- switches] of
            Just name -> changed settings 1 name
            Nothing
                | ident == editId -> saveNow settings >> c_save_now >> editSettings
                | ident == quitId -> quit
                | otherwise -> pure ()
        pure 0
    | otherwise = defWindowProc (Just w) msg wp lp

switches :: [Switch]
switches =
    [ Switch 201 "startup" (Toggle "on" "off" Unchecked) "Start with Windows" (14, 12, 130, 22) "Start trackkat in the tray when you log in; anti-cheat mode needs it running"
    , Switch 202 "updates" (Toggle "on" "off" Unchecked) "Keep up to date" (156, 12, 116, 22) "Once installed, fetch newer versions in the background as they come out"
    , Switch 203 "npclient" (Toggle "trackir" "trackkat" Unchecked) "Anti-cheat mode" (284, 12, 120, 22) "For anti-cheat that blocks trackkat's NPClient: games load NaturalPoint's"
    ]

editId, quitId :: WPARAM
editId = 204
quitId = 205

-- | A line along the top, the switches, Edit settings file and Quit
-- trackkat, with their help, the switches showing their settings.
build :: Kit -> Settings -> HWND -> IO ()
build kit settings w = do
    tips <- makeTips kit w
    let make cls text style rect ident = control kit w cls text style rect (fromIntegral ident) UiFont
    _ <- make "STATIC" "" ssEtchedHorz (0, 0, contentWidth, 2) (0 :: Int)
    mapM_ (makeSwitch kit w tips settings) switches
    edit <- make "BUTTON" "Edit settings file" (wS_TABSTOP .|. bS_PUSHBUTTON) (440, 10, 130, 26) editId
    tip tips edit "Open trackkat.conf in a text editor; trackkat applies it each time it is saved"
    quitButton <- make "BUTTON" "Quit trackkat" (wS_TABSTOP .|. bS_PUSHBUTTON) (590, 10, 116, 26) quitId
    tip tips quitButton "Stop tracking and quit; closing the window instead keeps trackkat in the tray"
