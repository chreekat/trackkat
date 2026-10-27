-- | Shaping the head pose for games: rejecting impossible jumps, smoothing,
-- per-axis response curves, trimming where forward is, and turning leans to
-- follow the view.
module TrackKat.Response (
    Curve (..),
    Multiplier (..),
    applyCurve,
    reach,
    yawRange,
    pitchRange,
    rollRange,
    leanRange,
    Smoothing (..),
    tuned,
    Lean (..),
    leanWithView,
    Trim (..),
    trimTo,
    applyTrim,
    Response (..),
    Shaper,
    newShaper,
    shape,
) where

import TrackKat.Pose (HeadPose (..))

-- | A response curve, the same on both sides of centre: in degrees for
-- rotations, mm for translations.
data Curve
    = -- | Points of (input, output), with input increasing and an implied
      -- point at the origin, joined by straight lines. Past the last point
      -- the curve continues along the last segment, so one point is a
      -- straight line.
      Points [(Double, Double)]
    | -- | A multiplier made gentle near centre and steeper toward the edge,
      -- keeping the head movement that reaches the end of the view's range
      -- where the multiplier alone puts it: the expo of radio-controlled
      -- models.
      Expo Multiplier
    deriving (Eq, Show)

-- | A multiplier with a centre precision.
data Multiplier = Multiplier
    { factor :: Double
    -- ^ above 0
    , precision :: Double
    -- ^ 0 to 100: 0 is the multiplier alone, 100 is flat at centre
    , viewRange :: Double
    -- ^ the end of the view's range, as one of the ranges below
    }
    deriving (Eq, Show)

-- | The ends of the views' ranges: looking straight behind, straight up or
-- down, and tipped onto your side; and leaning 300 mm, about as far as a
-- cockpit lets you.
yawRange, pitchRange, rollRange, leanRange :: Double
yawRange = 180
pitchRange = 90
rollRange = 90
leanRange = 300

-- | The head movement that, multiplied, reaches the end of the view's range.
reach :: Multiplier -> Double
reach m = viewRange m / factor m

-- | Where a curve takes a head movement.
--
-- An expo with multiplier m, reach R and precision as a fraction k takes a
-- movement x up to R to R·m·((1 - k)·u + k·u³), with u = x / R: at centre
-- the slope is m·(1 - k), rising to m·(1 + 2k) at the reach, where the
-- output is the end of the range whatever the precision. The slope is never
-- negative, so the view never turns back. Past the reach the curve
-- continues along its slope there.
applyCurve :: Curve -> Double -> Double
applyCurve (Points points) x = signum x * go (0, 0) points
  where
    a = abs x
    go (x0, y0) ((x1, y1) : rest)
        | a <= x1 || null rest = y0 + (a - x0) * slope x0 y0 x1 y1
        | otherwise = go (x1, y1) rest
    go _ [] = a
    slope x0 y0 x1 y1 = if x1 == x0 then 0 else (y1 - y0) / (x1 - x0)
applyCurve (Expo e) x
    | a <= r = signum x * viewRange e * ((1 - k) * u + k * u ^ (3 :: Int))
    | otherwise = signum x * (viewRange e + (a - r) * factor e * (1 + 2 * k))
  where
    a = abs x
    r = reach e
    u = a / r
    k = precision e / 100

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

-- | Where forward is for the view: an offset added to the view's yaw and
-- pitch, in degrees, after the curves. Roll and movement are never trimmed.
data Trim
    = NoTrim
    | Trim Double Double
    deriving (Eq, Show)

-- | Trim so that where the view looks now is forward. The view already
-- includes any earlier trim, so trims add up.
trimTo :: HeadPose -> Trim
trimTo p = Trim (wrapDegrees (yaw p)) (clampPitch (pitch p))

-- | Turn a view by a trim. A trimmed view's yaw is kept within 180 degrees
-- either side, and its pitch within 90, straight up or down.
applyTrim :: Trim -> HeadPose -> HeadPose
applyTrim NoTrim p = p
applyTrim (Trim y pt) p = p{yaw = wrapDegrees (yaw p + y), pitch = clampPitch (pitch p + pt)}

-- | An angle in degrees, from -180 up to 180.
wrapDegrees :: Double -> Double
wrapDegrees a = a - 360 * fromIntegral (floor ((a + 180) / 360) :: Integer)

clampPitch :: Double -> Double
clampPitch = max (-90) . min 90

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
-- its curve, the view is trimmed, and leaning follows the view if the
-- response says so. Leaning is turned last, by the yaw the game will show.
shape :: Response -> Trim -> Double -> HeadPose -> Shaper -> (HeadPose, Shaper)
shape r forward now pose s = case shLast s of
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
            leaning . applyTrim forward $
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
