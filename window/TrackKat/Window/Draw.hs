-- | The pictures in trackkat's window, drawn with ImGui's draw list: the
-- head as games see it, each axis's bar, and the response curves.
module TrackKat.Window.Draw (
    Canvas (..),
    canvas,
    drawHead,
    drawBar,
    drawCurve,
    dot,
    fill,
    label,
) where

import Control.Monad (forM_, when)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Foreign.C.Types (CFloat (..))
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (nullPtr)

import DearImGui hiding (end, flags, size, spacing, w, x, y, z)
import qualified DearImGui.Internal.Text as Text
import qualified DearImGui.Raw as Raw
import DearImGui.Raw.DrawList (DrawList)
import qualified DearImGui.Raw.DrawList as DL
import DearImGui.Raw.Font (Font (..))

import TrackKat.Pose (HeadPose (..))
import TrackKat.Response (Curve (..), Multiplier (..), applyCurve)
import TrackKat.Window.Curves
import TrackKat.Window.Style

-- | A rectangle of the window to draw in: its top left corner on the
-- screen, its width and height, and the window's draw list.
data Canvas = Canvas
    { cvList :: DrawList
    , cvLeft :: Float
    , cvTop :: Float
    , cvWidth :: Float
    , cvHeight :: Float
    }

-- | Take a rectangle of a width and height at the cursor, as an item that
-- can have a tooltip, to draw in.
canvas :: Float -> Float -> IO Canvas
canvas w h = do
    ImVec2 l t <- Raw.getCursorScreenPos
    dummy (pure (ImVec2 w h) :: IO ImVec2)
    list <- Raw.getWindowDrawList
    pure (Canvas list l t w h)

line :: DrawList -> Colour -> Float -> (Float, Float) -> (Float, Float) -> IO ()
line dl c thickness (ax, ay) (bx, by) =
    with (ImVec2 ax ay) $ \a -> with (ImVec2 bx by) $ \b -> DL.addLine dl a b (u32 c) (CFloat thickness)

rect :: DrawList -> Colour -> Float -> (Float, Float) -> (Float, Float) -> IO ()
rect dl c rounding (l, t) (r, b) =
    with (ImVec2 l t) $ \p -> with (ImVec2 r b) $ \q -> DL.addRectFilled dl p q (u32 c) (CFloat rounding) ImDrawFlags_None

disc :: DrawList -> Colour -> (Float, Float) -> Float -> IO ()
disc dl c (x, y) r = with (ImVec2 x y) $ \p -> DL.addCircleFilled dl p (CFloat r) (u32 c) 0

ring :: DrawList -> Colour -> Float -> (Float, Float) -> Float -> IO ()
ring dl c thickness (x, y) r = with (ImVec2 x y) $ \p -> DL.addCircle dl p (CFloat r) (u32 c) 0 (CFloat thickness)

polyline :: DrawList -> Colour -> Float -> [(Float, Float)] -> IO ()
polyline dl c thickness pts =
    withArrayLen [ImVec2 x y | (x, y) <- pts] $ \n p -> DL.addPolyLine dl p (fromIntegral n) (u32 c) (CFloat thickness) ImDrawFlags_None

-- | A dashed line, for guides that are not the thing drawn.
dashed :: DrawList -> Colour -> Float -> (Float, Float) -> (Float, Float) -> IO ()
dashed dl c dash (ax, ay) (bx, by) =
    forM_ [0, 2 .. n - 1] $ \i ->
        line dl c 1 (at (i / n)) (at (min 1 ((i + 1) / n)))
  where
    len = sqrt ((bx - ax) ^ (2 :: Int) + (by - ay) ^ (2 :: Int))
    n = fromIntegral (max 1 (ceiling (len / dash) :: Int))
    at f = (ax + (bx - ax) * f, ay + (by - ay) * f)

-- | Fill the canvas, with corners rounded to a radius.
fill :: Canvas -> Colour -> Float -> IO ()
fill (Canvas dl l t w h) c rounding = rect dl c rounding (l, t) (l + w, t + h)

-- | Text with its top left at a point, in a font at a size in pixels.
label :: Canvas -> Font -> Float -> Colour -> (Float, Float) -> String -> IO ()
label cv (Font f) size c (x, y) s =
    with (ImVec2 x y) $ \p -> Text.withCString (T.pack s) $ \cs ->
        DL.addText (cvList cv) f (CFloat size) p (u32 c) cs nullPtr 0 nullPtr

-- | A status light.
dot :: Canvas -> Colour -> IO ()
dot cv c = disc (cvList cv) c (cvLeft cv + cvWidth cv / 2, cvTop cv + cvHeight cv / 2) (min (cvWidth cv) (cvHeight cv) / 2)

-- | Faint lines through the middle, across and down.
crosshair :: Canvas -> Float -> IO ()
crosshair (Canvas dl l t w h) scale = do
    dashed dl grid (3 * scale) (l + w / 2, t) (l + w / 2, t + h)
    dashed dl grid (3 * scale) (l, t + h / 2) (l + w, t + h / 2)

-- | Turn a point on the head, in head axes (x the user's right, y up, z out
-- of the face), by the view's roll, then pitch, then yaw, as seen in a
-- mirror: turning right turns the drawing to the right.
turn :: HeadPose -> (Double, Double, Double) -> (Double, Double, Double)
turn v (x0, y0, z0) = (x3, y2, z3)
  where
    rad = (* (pi / 180))
    (a, b, c) = (rad (yaw v), rad (pitch v), rad (roll v))
    -- roll left is positive
    (x1, y1) = (x0 * cos c - y0 * sin c, x0 * sin c + y0 * cos c)
    -- pitch down is positive
    (y2, z2) = (y1 * cos b - z0 * sin b, y1 * sin b + z0 * cos b)
    -- yaw right is positive
    (x3, z3) = (x1 * cos a + z2 * sin a, -x1 * sin a + z2 * cos a)

-- | The head as games see it, seen from in front: a face that turns, tilts
-- and moves with the view, and a line out of the nose showing where it
-- looks; dimmed unless live.
drawHead :: Canvas -> Float -> HeadPose -> Bool -> IO ()
drawHead cv@(Canvas dl l t size _) scale v live = do
    rect dl box (4 * scale) (l, t) (l + size, t + size)
    crosshair cv scale
    with (ImVec2 l t) $ \p -> with (ImVec2 (l + size) (t + size)) $ \q -> DL.pushClipRect dl p q 1
    -- What is behind the head is drawn before it, what faces us after.
    forM_ [False, True] $ \facing -> do
        when facing $ face (cx, cy) r
        forM_ [-1, 1] $ \side -> do
            let (p, front) = project (side * 0.98, 0, 0)
            when (front == facing) $ face p (r * 0.2)
        let (nose, front) = project (0, -0.05, 1)
        when (front == facing) $ line dl (if live then accent else dim) w nose (fst (project (0, -0.05, 2.2)))
    forM_ [-1, 1] $ \side -> do
        let (p, front) = project (side * 0.36, 0.22, 0.9)
        when front $ disc dl feature p (r * 0.09)
    let (noseTip, noseFront) = project (0, -0.05, 1)
    when noseFront $ line dl feature w noseTip (fst (project (0, -0.12, 1.28)))
    let smile = [project (x, y, sqrt (1 - x * x - y * y)) | i <- [0 .. 8 :: Int], let x = -0.32 + 0.64 * fromIntegral i / 8; y = -0.48 - 0.08 * sin (fromIntegral i * pi / 8)]
    forM_ (zip smile (drop 1 smile)) $ \((p, pf), (q, qf)) -> when (pf && qf) $ line dl feature w p q
    DL.popClipRect dl
  where
    w = 2 * scale
    -- 300 mm from the centre to the edge; nearer is bigger.
    k = realToFrac size / 2 / 300 :: Double
    zNear = max (-300) (tz v)
    cx = l + size / 2 - realToFrac (tx v * k)
    cy = t + size / 2 + realToFrac (ty v * k)
    r = size * 0.2 * realToFrac (600 / (600 + zNear))
    skin = if live then rgb 78 88 104 else rgb 58 62 70
    edge = if live then rgb 205 212 224 else dim
    feature = if live then rgb 240 244 250 else dim
    face p radius = disc dl skin p radius >> ring dl edge w p radius
    project q =
        let (x, y, z) = turn v q
         in ((cx + realToFrac x * r, cy - realToFrac y * r), z > 0)

-- | An axis's bar: where games see it, out to the end of its range either
-- side of the middle, with a tick where the head is.
drawBar :: Canvas -> Float -> PoseAxis -> Double -> Double -> Bool -> IO ()
drawBar (Canvas dl l t w h) scale a headValue viewValue live = do
    rect dl box (2 * scale) (l, t) (l + w, t + h)
    let mid = l + w / 2
        at value = mid + realToFrac (max (-1) (min 1 (value / axisRange a))) * (w / 2)
        end = at viewValue
    rect dl (if live then accent else withAlpha 0.5 accent) 0 (min mid end, t + 2 * scale) (max mid end + 1, t + h - 2 * scale)
    line dl grid 1 (mid, t) (mid, t + h)
    let tick = at headValue
    rect dl (if live then headTick else dim) 0 (tick - 1.5 * scale, t - 1 * scale) (tick + 1.5 * scale, t + h + 1 * scale)

-- | A curve's plot: across, the head a quarter further than the
-- multiplier takes to reach the end of the view's range, so the curve's
-- shape fills the plot whatever the multiplier; up, the view from one end
-- of its range to the other. A dot shows the head now; with a precision, a
-- dashed line shows the multiplier alone.
drawCurve :: Canvas -> Float -> PoseAxis -> CurveForm -> Double -> Bool -> IO ()
drawCurve cv@(Canvas dl l t w h) scale a form headValue live = do
    rect dl box (3 * scale) (l, t) (l + w, t + h)
    crosshair cv scale
    with (ImVec2 l t) $ \p -> with (ImVec2 (l + w) (t + h)) $ \q -> DL.pushClipRect dl p q 1
    case form of
        Unreadable _ -> pure ()
        MultiplierForm f p -> do
            when (p > 0) $
                dashed dl dim (3 * scale) (point (-across) (-across)) (point across across)
            plot (Expo (Multiplier f p range)) f
        PointsForm c -> plot c (fromMaybe 1 (formFactor form))
    DL.popClipRect dl
  where
    range = axisRange a
    across = 1.25 * range
    point x y =
        ( l + w / 2 + realToFrac (x / across) * (w / 2)
        , t + h / 2 - realToFrac (max (-1.5) (min 1.5 (y / range))) * (h / 2 - 2 * scale)
        )
    plot c f = do
        let reachX = across / f
            scaled x = x * f
            pts = [point (scaled x) (applyCurve c x) | i <- [0 .. 128 :: Int], let x = reachX * (fromIntegral i / 64 - 1)]
        polyline dl accent (2 * scale) pts
        let (px, py) = point (scaled headValue) (applyCurve c headValue)
            clamp lo hi = max lo . min hi
        disc dl box (clamp l (l + w) px, clamp t (t + h) py) (4 * scale)
        disc dl (if live then headTick else dim) (clamp l (l + w) px, clamp t (t + h) py) (3 * scale)
