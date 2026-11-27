-- | Each axis's response plot, with window = haskell: what
-- "TrackKat.UI.Curve" says it shows. The controls beside it are
-- cbits/gui.c's.
module TrackKat.Gui.Curve (registerCurve) where

import Foreign.C.String (CWString, peekCWString)
import Foreign.C.Types (CInt (..), CIntPtr (..))
import Foreign.Marshal.Array (allocaArray)
import Graphics.Win32.GDI.Types (HDC, HWND)
import Graphics.Win32.Message (wM_ERASEBKGND, wM_PAINT)
import Graphics.Win32.Window (WindowClosure, defWindowProc, getClientRect)
import System.Win32.Types (INT)

import TrackKat.Display (Tracked (..), shownHead, tracked)
import TrackKat.Gui.Paint
import TrackKat.Gui.Shown (snapshot)
import TrackKat.Gui.Win32 (dtCenter, gwlpUserData)
import TrackKat.Pose (HeadPose (..))
import TrackKat.Response (leanRange, pitchRange, rollRange, yawRange)
import TrackKat.UI.Curve
import TrackKat.UI.Palette

foreign import ccall unsafe "hts_gui_curve_text" c_curve_text :: CInt -> CWString -> IO ()
foreign import ccall unsafe "windows.h GetWindowLongPtrW" c_GetWindowLongPtr :: HWND -> INT -> IO CIntPtr

-- | Register the response plots' window class, reporting what fails in
-- them.
registerCurve :: (String -> IO ()) -> IO (Either Int ())
registerCurve report = registerPart report "A response plot" "trackkat_curve" Unfilled curveProc

curveProc :: WindowClosure
curveProc w msg wp lp
    | msg == wM_ERASEBKGND = pure 1
    | msg == wM_PAINT = do
        painted w $ \dc -> do
            (_, _, right, bottom) <- getClientRect w
            let (width, height) = (fromIntegral right, fromIntegral bottom)
            -- cbits/gui.c's build_window numbers each plot by its axis.
            axis <- c_GetWindowLongPtr w gwlpUserData
            buffered dc (0, 0, width, height) (\mem -> paint mem (fromIntegral axis) width height)
        pure 0
    | otherwise = defWindowProc (Just w) msg wp lp

-- | The axis's curve over a grid through the centre, and the head on it; or
-- a question mark, for text that isn't a curve.
paint :: HDC -> CInt -> CInt -> CInt -> IO ()
paint dc axis w h = do
    fillRect dc (0, 0, fromIntegral w, fromIntegral h) boxColour
    polyline dc Dotted 1 gridColour [(w `div` 2, 0), (w `div` 2, h)]
    polyline dc Dotted 1 gridColour [(0, h `div` 2), (w, h `div` 2)]
    text <- curveText axis
    shown <- snapshot
    let x = case drop (fromIntegral axis) [yaw, pitch, roll, tx, ty, tz] of
            part : _ -> part (shownHead shown)
            [] -> 0
    [s2, s3] <- mapM scaled [2, 3]
    case curvePlot (range axis) text x (fromIntegral w, fromIntegral h) (fromIntegral s2) of
        Unreadable -> textAt dc SmallFont amber (0, 0, w, h) dtCenter "?"
        Plotted p -> do
            mapM_ (\(a, b) -> polyline dc Dotted 1 dim [pixel a, pixel b]) (plotPlain p)
            polyline dc Solid s2 accent (map pixel (plotCurve p))
            disc dc (case tracked shown of TrackedNow -> headTick; NotTrackedNow -> dim) 1 boxColour (pixel (plotHead p)) s3
  where
    pixel (a, b) = (fromIntegral a, fromIntegral b)

-- | The ends of each axis's view, numbered as cbits/gui.c numbers them.
range :: CInt -> Double
range axis = case drop (fromIntegral axis) [yawRange, pitchRange, rollRange, leanRange, leanRange, leanRange] of
    r : _ -> r
    [] -> leanRange

-- | An axis's curve as typed in cbits/gui.c's field for it.
curveText :: CInt -> IO String
curveText axis = allocaArray fieldMax $ \text -> c_curve_text axis text >> peekCWString text
  where
    -- As FIELD_MAX in cbits/gui.c.
    fieldMax = 4096
