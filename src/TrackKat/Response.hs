-- | Shaping the head pose for games: rejecting impossible jumps, smoothing,
-- per-axis response curves, aiming, trimming where forward is, and turning
-- leans to follow the view.
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
    Aim (..),
    Aiming (..),
    aimGlide,
    Response (..),
    Shaper,
    newShaper,
    shape,
) where

import TrackKat.Pose (HeadPose (..))
import TrackKat.World (Instant, Seconds, World (..), inDegrees, inMillimetres, inSeconds, seconds, world, (.-.))

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
-- cockpit lets you. cbits/gui.c draws the axes to the same ranges.
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

-- | Where forward is for the view: the view taken as forward, added to the
-- view after the curves. Its turning is added before leaning follows the
-- view, so leaning follows the trimmed view; its movement after, so the
-- view stays where it was moved to, wherever it then looks.
data Trim
    = NoTrim
    | Trim HeadPose
    deriving (Eq, Show)

-- | Trim so that the view now is forward. The view already includes any
-- earlier trim, so trims add up.
trimTo :: HeadPose -> Trim
trimTo p = Trim p{yaw = wrapDegrees (yaw p), pitch = clampPitch (pitch p), roll = wrapDegrees (roll p)}

-- | Turn a view by a trim. A trimmed view's yaw and roll are kept within
-- 180 degrees either side, and its pitch within 90, straight up or down.
turnByTrim :: Trim -> HeadPose -> HeadPose
turnByTrim NoTrim p = p
turnByTrim (Trim t) p =
    p{yaw = wrapDegrees (yaw p + yaw t), pitch = clampPitch (pitch p + pitch t), roll = wrapDegrees (roll p + roll t)}

-- | Move a view by a trim.
moveByTrim :: Trim -> HeadPose -> HeadPose
moveByTrim NoTrim p = p
moveByTrim (Trim t) p = p{tx = tx p + tx t, ty = ty p + ty t, tz = tz p + tz t}

-- | An angle in degrees, from -180 up to 180.
wrapDegrees :: Double -> Double
wrapDegrees a = a - 360 * fromIntegral (floor ((a + 180) / 360) :: Integer)

clampPitch :: Double -> Double
clampPitch = max (-90) . min 90

-- Note [Aiming]
-- ~~~~~~~~~~~~~
-- The curves make a small turn of the head a large turn of the view, to
-- look around a cockpit without looking away from the screen: too large to
-- lay a gunsight on something by head. Aiming is for that: from when it
-- begins, the view turns from where it then was by the head's turning
-- since, through gentler curves ('Aim') and with a smoothing of its own.
--
--  * A frame's view never depends on whether aiming is asked for in that
--    frame, only on what was asked before it: the frame aiming begins on
--    is shaped as if it hadn't, and is where the view and the head are
--    taken from; the frame it ends on is shaped as if it hadn't ended. So
--    the view never jumps as aiming begins or ends.
--
--  * While aiming, a head that turns by some angle from where aiming began
--    turns the view by the aim's curve of that angle, from where the view
--    was then: the curve is of the turn since, not of the head's turn from
--    centre.
--
--  * When aiming ends the view glides back onto what the curves make of
--    the head: how far the aimed view was from that is added on, shrinking
--    to nothing over 'aimGlide'. Aiming begun again meanwhile begins from
--    the view as it then is.
--
--  * Only turning is aimed. The view's movement is what it is without
--    aiming, smoothing included.
--
--  * The smoothing's memory of the head carries across, so changing from
--    one smoothing to the other moves nothing either.
--
-- Trimming and leaning with the view come after, as they do without
-- aiming.

-- | How the view turns as the head does while aiming: a smoothing for the
-- head's turning, and a curve for each axis of it, taking how far the head
-- has turned since aiming began to how far the view has. See Note [Aiming].
data Aim = Aim
    { aimSmoothing :: Smoothing
    , aimYaw, aimPitch, aimRoll :: Curve
    }
    deriving (Eq, Show)

-- | Whether aiming is asked for.
data Aiming = NotAiming | Aiming
    deriving (Eq, Show)

-- | How long the view takes to glide back once aiming ends.
aimGlide :: Seconds
aimGlide = seconds 0.2

-- | How the head's pose becomes the view games see: smoothing, a curve
-- for each axis, what leaning follows, and how aiming turns the view.
data Response = Response
    { smoothing :: Smoothing
    , yawCurve, pitchCurve, rollCurve, xCurve, yCurve, zCurve :: Curve
    , lean :: Lean
    , aim :: Aim
    }
    deriving (Eq, Show)

-- | Yaw, pitch and roll, in degrees.
data Turning = Turning !Double !Double !Double

-- | How aiming stands. See Note [Aiming].
data Anchorage
    = -- | not aiming: the view is what the curves make of the head
      Free
    | -- | aiming: the smoothed head's turning when it began, and the view's
      Anchored !Turning !Turning
    | -- | aiming ended then, with the view so far from what the curves made
      -- of the head
      Gliding !Instant !Turning

-- | One axis's 1€ filter state: the filtered value and its filtered rate of
-- change.
data Axis = Axis !Double !Double

-- | What shaping keeps from one frame to the next.
data Shaper = Shaper
    { shLast :: !(Maybe (Instant, HeadPose))
    -- ^ time and pose last accepted, before smoothing
    , shRejected :: !Int
    -- ^ frames in a row rejected as impossible jumps
    , shAxes :: !(Maybe [Axis])
    -- ^ all evaluated, so that shaping whose output isn't used, as while
    -- paused, doesn't hold on to every frame before
    , shAim :: !Anchorage
    }

-- | A shaper that has seen no frame, and isn't aiming.
newShaper :: Shaper
newShaper = Shaper Nothing 0 Nothing Free

-- | Largest change in one frame that a head can make: the world's
-- 'headStepTurn' and 'headStepMove', about 1800 degrees or 5 m a second.
-- Tracking scores how likely a pose's turn and move are, by a head's
-- speed, and takes what is likeliest; this catches what that lets
-- through on an axis of the pose games see.
maxStepDegrees, maxStepMm :: Double
maxStepDegrees = inDegrees (headStepTurn world)
maxStepMm = inMillimetres (headStepMove world)

-- | Frames a jump must persist before it counts as real.
jumpPersistence :: Int
jumpPersistence = 3

-- | Shape a pose seen at the given time. Impossible jumps are
-- ignored unless they persist, then each axis is smoothed and put through
-- its curve, or its turning through the aim's as Note [Aiming] has it, the
-- view is turned by the trim, leaning follows the view if the response
-- says so, by the yaw the game will show, and the view is moved by the
-- trim. Whether aiming is asked for tells on the frames after this one.
shape :: Response -> Trim -> Aiming -> Instant -> HeadPose -> Shaper -> (HeadPose, Shaper)
shape r forward wanted now pose s = case shLast s of
    Just (_, previous)
        | jump previous pose && shRejected s + 1 < jumpPersistence ->
            (output (maybe (values previous) (map (\(Axis v _) -> v)) (shAxes s)) previous, s{shRejected = shRejected s + 1})
    _ ->
        let axes = smoothAll (fst <$> shLast s) (shAxes s)
            smoothed = map (\(Axis v _) -> v) axes
         in (output smoothed pose, foldr seq (Shaper (Just (now, pose)) 0 (Just axes) (anchorage smoothed)) axes)
  where
    values p = [yaw p, pitch p, roll p, tx p, ty p, tz p]
    jump a b =
        or (zipWith3 (\limit u v -> abs (u - v) > limit) (replicate 3 maxStepDegrees ++ replicate 3 maxStepMm) (values a) (values b))
    smoothAll lastTime axes = case (lastTime, axes) of
        (Just t, Just old) | now > t -> zipWith3 (smooth (inSeconds (now .-. t))) smoothings old (values pose)
        _ -> [Axis v 0 | v <- values pose]
    smooth dt (Smoothing mc b) old v = oneEuro mc b dt old v
    smooth _ NoSmoothing _ v = Axis v 0
    smoothings = replicate 3 turningSmoothing ++ replicate 3 (smoothing r)
    turningSmoothing = case shAim s of
        Anchored _ _ -> aimSmoothing (aim r)
        _ -> smoothing r
    output smoothed fallback = case smoothed of
        [y, p, ro, x, yy, z] ->
            let Turning vy vp vr = turning (Turning y p ro)
             in moveByTrim forward . leaning . turnByTrim forward $
                    HeadPose
                        { yaw = vy
                        , pitch = vp
                        , roll = vr
                        , tx = applyCurve (xCurve r) x
                        , ty = applyCurve (yCurve r) yy
                        , tz = applyCurve (zCurve r) z
                        }
        _ -> fallback
    leaning = case lean r of
        LeanWithView -> leanWithView
        LeanWithScreen -> id
    -- The view's turning for the smoothed head's, as aiming stood before
    -- this frame.
    turning h = case shAim s of
        Free -> curved h
        Anchored h0 v0 -> v0 `plus` aimed (h `minus` h0)
        Gliding since off -> curved h `plus` scaled (glideLeft (now .-. since)) off
    curved (Turning y p ro) = Turning (applyCurve (yawCurve r) y) (applyCurve (pitchCurve r) p) (applyCurve (rollCurve r) ro)
    aimed (Turning y p ro) = Turning (applyCurve (aimYaw (aim r)) y) (applyCurve (aimPitch (aim r)) p) (applyCurve (aimRoll (aim r)) ro)
    -- How aiming stands after this frame.
    anchorage smoothed = case (smoothed, wanted, shAim s) of
        (y : p : ro : _, Aiming, Free) -> anchor (Turning y p ro)
        (y : p : ro : _, Aiming, Gliding _ _) -> anchor (Turning y p ro)
        (y : p : ro : _, NotAiming, Anchored _ _) -> let h = Turning y p ro in Gliding now (turning h `minus` curved h)
        (_, NotAiming, Gliding since _) | now .-. since >= aimGlide -> Free
        (_, _, unchanged) -> unchanged
    anchor h = Anchored h (turning h)

plus, minus :: Turning -> Turning -> Turning
plus (Turning a b c) (Turning x y z) = Turning (a + x) (b + y) (c + z)
minus (Turning a b c) (Turning x y z) = Turning (a - x) (b - y) (c - z)

scaled :: Double -> Turning -> Turning
scaled k (Turning x y z) = Turning (k * x) (k * y) (k * z)

-- | How much of what the aimed view was off by is still to go, so long
-- after aiming ended: all of it, easing out and in to none of it after
-- 'aimGlide', never more as time goes on.
glideLeft :: Seconds -> Double
glideLeft since
    | u <= 0 = 1
    | u >= 1 = 0
    | otherwise = 1 - u * u * (3 - 2 * u)
  where
    u = inSeconds since / inSeconds aimGlide

-- | One step of the 1€ filter, of so many seconds.
oneEuro :: Double -> Double -> Double -> Axis -> Double -> Axis
oneEuro mc b dt (Axis previous rate) x =
    let rate' = lowPass (alpha derivativeCutoff) rate ((x - previous) / dt)
        cutoff = mc + b * abs rate'
     in Axis (lowPass (alpha cutoff) previous x) rate'
  where
    alpha cutoff = 1 / (1 + 1 / (2 * pi * cutoff * dt))
    lowPass a old new = old + a * (new - old)
    derivativeCutoff = 1
