-- | Shaping the head pose for games: rejecting impossible jumps, smoothing,
-- and per-axis response curves.
module TrackKat.Response (
    Curve (..),
    applyCurve,
    Smoothing (..),
    tuned,
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

data Response = Response
    { smoothing :: Smoothing
    , yawCurve, pitchCurve, rollCurve, xCurve, yCurve, zCurve :: Curve
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
-- its curve.
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
            HeadPose
                { yaw = applyCurve (yawCurve r) y
                , pitch = applyCurve (pitchCurve r) p
                , roll = applyCurve (rollCurve r) ro
                , tx = applyCurve (xCurve r) x
                , ty = applyCurve (yCurve r) yy
                , tz = applyCurve (zCurve r) z
                }
        _ -> fallback

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
