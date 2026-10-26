-- | Shaping the head pose for games: rejecting impossible jumps, smoothing,
-- per-axis response curves, and turning leans to follow the view.
module TrackKat.Response (
    Curve (..),
    applyCurve,
    Smoothing (..),
    tuned,
    Lean (..),
    leanWithView,
    Response (..),
    Shaper,
    newShaper,
    shape,
) where

import TrackKat.Pose (HeadPose (..))

-- | A response curve: points of (input, output), with input increasing and
-- an implied point at the origin. In degrees for rotations, mm for
-- translations. The same on both sides of centre.
newtype Curve = Curve [(Double, Double)]
    deriving (Eq, Show)

-- | Interpolate linearly between the curve's points, and past the last one
-- continue along the last segment.
applyCurve :: Curve -> Double -> Double
applyCurve (Curve points) x = signum x * go (0, 0) points
  where
    a = abs x
    go (x0, y0) ((x1, y1) : rest)
        | a <= x1 || null rest = y0 + (a - x0) * slope x0 y0 x1 y1
        | otherwise = go (x1, y1) rest
    go _ [] = a
    slope x0 y0 x1 y1 = if x1 == x0 then 0 else (y1 - y0) / (x1 - x0)

-- | The 1€ filter's settings (Casiez, Roussel and Vogel, CHI 2012): how
-- strongly to smooth a still head, and how quickly smoothing gives way as it
-- moves.
data Smoothing
    = Smoothing
        { minCutoff :: Double
        -- ^ Hz; lower is smoother at rest
        , beta :: Double
        -- ^ added cutoff per unit/s of speed; higher lags less when moving
        }
    | NoSmoothing
    deriving (Eq, Show)

-- | Smoothing from steadiness and responsiveness, each 0 to 100, on log
-- scales: steadiness takes the cutoff from 20 Hz down to 0.2 Hz, and
-- responsiveness takes beta from 0.001 up to 1.
tuned :: Double -> Double -> Smoothing
tuned steadiness responsiveness =
    Smoothing
        { minCutoff = 20 * 0.01 ** (steadiness / 100)
        , beta = 0.001 * 1000 ** (responsiveness / 100)
        }

-- | Which way leaning moves the game's head.
data Lean
    = -- | Relative to the view: the lean is turned by the yaw the game shows.
      LeanWithView
    | -- | Relative to the screen, whichever way the view looks.
      LeanWithScreen
    deriving (Eq, Show)

-- | Turn a pose's horizontal movement by its yaw, so that it is relative to
-- where the view looks rather than to the screen: with the view turned to
-- look behind, a lean to the left, which is to the left of the screen, moves
-- the head to the left of the view, out of the right side of the cockpit.
--
-- Only yaw counts. When you look around a real cockpit your body turns with
-- your gaze, but it stays upright when you look up or down; following pitch
-- would make a lean toward the screen dive the head into the panel while
-- you look down at it, and would carry the pitch's jitter, multiplied by its
-- gain, into the movement.
leanWithView :: HeadPose -> HeadPose
leanWithView p = p{tx = tx p * c + tz p * s, tz = tz p * c - tx p * s}
  where
    -- Yaw is positive to the user's right, x to their left, z behind them.
    c = cos (yaw p * pi / 180)
    s = sin (yaw p * pi / 180)

data Response = Response
    { smoothing :: Smoothing
    , yawCurve, pitchCurve, rollCurve, xCurve, yCurve, zCurve :: Curve
    , lean :: Lean
    }
    deriving (Eq, Show)

-- | One axis's 1€ filter state: the filtered value and its filtered rate of
-- change.
data Axis = Axis !Double !Double

data Shaper = Shaper
    { shLast :: Maybe (Double, HeadPose)
    -- ^ time and pose last accepted, before smoothing
    , shRejected :: !Int
    -- ^ frames in a row rejected as impossible jumps
    , shAxes :: Maybe [Axis]
    }

newShaper :: Shaper
newShaper = Shaper Nothing 0 Nothing

-- | Largest change in one frame that a head can make: about 1800 degrees or
-- 5 m a second.
maxStepDegrees, maxStepMm :: Double
maxStepDegrees = 15
maxStepMm = 40

-- | Frames a jump must persist before it counts as real.
jumpPersistence :: Int
jumpPersistence = 3

-- | Shape a pose seen at the given time in seconds. Impossible jumps are
-- ignored unless they persist, then each axis is smoothed and put through
-- its curve, and leaning follows the view if the response says so. Leaning
-- is turned last, by the yaw the game will show.
shape :: Response -> Double -> HeadPose -> Shaper -> (HeadPose, Shaper)
shape r now pose s = case shLast s of
    Just (_, previous)
        | jump previous pose && shRejected s + 1 < jumpPersistence ->
            (output (shAxes s) previous, s{shRejected = shRejected s + 1})
    _ ->
        let axes = smoothAll (fst <$> shLast s) (shAxes s)
         in (output (Just axes) pose, Shaper (Just (now, pose)) 0 (Just axes))
  where
    values p = [yaw p, pitch p, roll p, tx p, ty p, tz p]
    jump a b =
        or (zipWith3 (\limit u v -> abs (u - v) > limit) (replicate 3 maxStepDegrees ++ replicate 3 maxStepMm) (values a) (values b))
    smoothAll lastTime axes = case (smoothing r, lastTime, axes) of
        (Smoothing mc b, Just t, Just old) | now > t -> zipWith (oneEuro mc b (now - t)) old (values pose)
        _ -> [Axis v 0 | v <- values pose]
    output axes fallback = case maybe (values fallback) (map (\(Axis v _) -> v)) axes of
        [y, p, ro, x, yy, z] ->
            leaning
                HeadPose
                    { yaw = applyCurve (yawCurve r) y
                    , pitch = applyCurve (pitchCurve r) p
                    , roll = applyCurve (rollCurve r) ro
                    , tx = applyCurve (xCurve r) x
                    , ty = applyCurve (yCurve r) yy
                    , tz = applyCurve (zCurve r) z
                    }
        _ -> fallback
    leaning = case lean r of
        LeanWithView -> leanWithView
        LeanWithScreen -> id

-- | One step of the 1€ filter.
oneEuro :: Double -> Double -> Double -> Axis -> Double -> Axis
oneEuro mc b dt (Axis previous rate) x =
    let rate' = lowPass (alpha derivativeCutoff) rate ((x - previous) / dt)
        cutoff = mc + b * abs rate'
     in Axis (lowPass (alpha cutoff) previous x) rate'
  where
    alpha cutoff = 1 / (1 + 1 / (2 * pi * cutoff * dt))
    lowPass a old new = old + a * (new - old)
    derivativeCutoff = 1
