-- | The masthead across the top of trackkat's window, with window = haskell:
-- trackkat, its version, and what the last look for a newer one found, or an
-- offer on amber ("TrackKat.UI.Masthead"). Its buttons are cbits/gui.c's,
-- children of it.
module TrackKat.Gui.Masthead (registerMasthead) where

import Control.Monad (when)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (ptrToIntPtr, wordPtrToPtr)
import Graphics.Win32.GDI.Types (HWND)
import Graphics.Win32.Message (wM_COMMAND, wM_ERASEBKGND, wM_PAINT)
import Graphics.Win32.Window (WindowClosure, defWindowProc, getClientRect, getParent, sendMessage)
import System.Win32.Types (HANDLE)

import TrackKat.Display (Shown)
import TrackKat.Gui.Kit (Kit, scaled)
import TrackKat.Gui.Paint
import TrackKat.Gui.Shown (snapshot)
import TrackKat.Gui.Win32 (dtLeft, wmCtlColorBtn, wmPrintClient)
import TrackKat.UI.Masthead

-- | Register the masthead's window class, reporting what fails in it.
registerMasthead :: Kit -> (String -> IO ()) -> IO (Either Int ())
registerMasthead kit report = do
    -- The brushes behind its buttons' rounded corners, for the life of the
    -- process.
    quiet <- solidBrush stripColour
    offering <- solidBrush offerColour
    -- As cbits/gui.c's build_window makes it.
    registerPart report "The masthead" "trackkat_strip" Unfilled (mastheadProc kit quiet offering)

mastheadProc :: Kit -> HANDLE -> HANDLE -> WindowClosure
mastheadProc kit quiet offering w msg wp lp
    | msg == wM_ERASEBKGND = pure 1
    | msg == wM_PAINT = do
        painted kit w (paintIn w)
        pure 0
    -- Behind the buttons' rounded corners.
    | msg == wmPrintClient = paintIn w (Canvas kit (wordPtrToPtr (fromIntegral wp))) >> pure 0
    | msg == wmCtlColorBtn = do
        s <- snapshot
        let brush = case mastheadSays s of SaysOffer _ -> offering; _ -> quiet
        pure (fromIntegral (ptrToIntPtr brush))
    | msg == wM_COMMAND = getParent w >>= \parent -> sendMessage parent msg wp lp
    | otherwise = defWindowProc (Just w) msg wp lp

paintIn :: HWND -> Canvas -> IO ()
paintIn w dc = do
    (_, _, right, bottom) <- getClientRect w
    paint dc (fromIntegral right) (fromIntegral bottom) =<< snapshot

-- | trackkat, its version and what the last look found; or, with an offer,
-- the offer, on amber. The button at the right is a child of its own.
paint :: Canvas -> CInt -> CInt -> Shown -> IO ()
paint dc w h shown = do
    let says = mastheadSays shown
        offer = case says of SaysOffer _ -> True; _ -> False
    fillRect dc (0, 0, fromIntegral w, fromIntegral h) (if offer then offerColour else stripColour)
    -- A line of one pixel along the bottom.
    fillRect dc (0, fromIntegral (h - 1), fromIntegral w, fromIntegral h) (if offer then offerEdge else stripEdge)
    let size = s 16
    icon <- resourceIcon programIcon size
    drawIcon dc icon (s 10) ((h - size) `div` 2) size
    let x0 = s 10 + size + s 8
    name <- textWidth dc BoldFont "trackkat"
    textAt dc BoldFont (RGB 27 27 27) (x0, 0, name, h) dtLeft "trackkat"
    let version = mastheadVersion shown
        x1 = x0 + name + s 8
    versionWidth <- textWidth dc UiFont version
    textAt dc UiFont (RGB 85 85 85) (x1, 0, versionWidth, h) dtLeft version
    let x = x1 + versionWidth + s 12
        right = w - s (buttonWidth + 20)
    when (right > x) $ case says of
        SaysOffer text -> do
            -- On one line or two, in the smaller font if it needs more.
            tall <- textHeight dc UiFont Wrapped (right - x) text
            let font = if tall > h - s 8 then SmallFont else UiFont
            th <- min (h - s 4) <$> textHeight dc font Wrapped (right - x) text
            textIn dc font offerText Wrapped (x, (h - th) `div` 2, right - x, th) text
        SaysStatus verdict text -> do
            x' <- case verdict of
                Good -> do
                    let cy = h `div` 2
                    polyline dc Solid (s 2) goodColour [(x + s 1, cy), (x + s 4, cy + s 3), (x + s 10, cy - s 4)]
                    pure (x + s 15)
                Plain -> pure x
            textAt dc UiFont (case verdict of Good -> goodColour; Plain -> RGB 85 85 85) (x', 0, right - x', h) dtLeft text
        SaysNothing -> pure ()
  where
    s = scaled (canvasKit dc)

-- As cbits/gui.c's STRIP_BUTTON_W and C_ colours.
buttonWidth :: CInt
buttonWidth = 150

stripColour, stripEdge, offerColour, offerEdge, offerText, goodColour :: Colour
stripColour = RGB 228 228 228
stripEdge = RGB 207 207 207
offerColour = RGB 255 244 214
offerEdge = RGB 230 207 138
offerText = RGB 106 75 0
goodColour = RGB 46 107 52
