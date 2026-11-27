-- | Each axis's response plot, with window = haskell: what
-- "TrackKat.UI.Curve" says it shows. The controls beside it are
-- cbits/gui.c's.
module TrackKat.Gui.Curve (registerCurve) where

import Foreign.C.String (CWString, peekCWString)
import Foreign.C.Types (CDouble (..), CInt (..), CIntPtr (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import Graphics.Win32.GDI.Types (HDC, HWND)
import Graphics.Win32.Message (wM_ERASEBKGND, wM_PAINT)
import Graphics.Win32.Window (WindowClosure, defWindowProc, getClientRect)
import System.Win32.Types (INT)

import TrackKat.Gui.Paint
import TrackKat.Gui.Win32 (dtCenter, gwlpUserData)
import TrackKat.Response (leanRange, pitchRange, rollRange, yawRange)
import TrackKat.UI.Curve
import TrackKat.UI.Palette

foreign import ccall unsafe "hts_gui_curve_state"
    c_curve_state :: CInt -> CWString -> Ptr CDouble -> Ptr CInt -> IO ()
foreign import ccall unsafe "windows.h GetWindowLongPtrW" c_GetWindowLongPtr :: HWND -> INT -> IO CIntPtr

-- | Register the response plots' window class, reporting what fails in
-- them.
registerCurve :: (String -> IO ()) -> IO (Either Int ())
registerCurve report = registerPart report "A response plot" "trackkat_curve" curveProc

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
    (text, x, sight) <- curveState axis
    [s2, s3] <- mapM scaled [2, 3]
    case curvePlot (range axis) text x (fromIntegral w, fromIntegral h) (fromIntegral s2) of
        Unreadable -> textAt dc SmallFont amber (0, 0, w, h) dtCenter "?"
        Plotted p -> do
            mapM_ (\(a, b) -> polyline dc Dotted 1 dim [pixel a, pixel b]) (plotPlain p)
            polyline dc Solid s2 accent (map pixel (plotCurve p))
            disc dc (case sight of Tracked -> headTick; Untracked -> dim) 1 boxColour (pixel (plotHead p)) s3
  where
    pixel (a, b) = (fromIntegral a, fromIntegral b)

-- | The ends of each axis's view, numbered as cbits/gui.c numbers them.
range :: CInt -> Double
range axis = case drop (fromIntegral axis) [yawRange, pitchRange, rollRange, leanRange, leanRange, leanRange] of
    r : _ -> r
    [] -> leanRange

-- | Whether the head shown is the head as tracked now.
data Sight = Tracked | Untracked

-- | What cbits/gui.c last saw for an axis's plot.
curveState :: CInt -> IO (String, Double, Sight)
curveState axis =
    allocaArray fieldMax $ \text -> alloca $ \x -> alloca $ \live -> do
        c_curve_state axis text x live
        t <- peekCWString text
        CDouble v <- peek x
        l <- peek live
        pure (t, v, if l /= 0 then Tracked else Untracked)
  where
    -- As FIELD_MAX in cbits/gui.c.
    fieldMax = 4096
