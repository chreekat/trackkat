-- | Which of the clip's reflectors the camera can see at a pose: those
-- that face it, and that the head isn't in front of.
--
-- See Note [Seeing the reflectors].
module TrackKat.Visibility (
    HeadShape (..),
    usualHead,
    Sighted (..),
    sighted,
    behindHead,
) where

import TrackKat.Linear
import TrackKat.Pose

-- Note [Seeing the reflectors]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A reflector goes out of the camera's sight two ways. Tape on a curved
-- tab faces one way, and once the camera is round the tab beyond the
-- tape's edge and a quarter turn more, none of it faces the camera (see
-- Note [Curved reflectors] in "TrackKat.Pose"); points face every way.
-- And the head itself can be in the way: taken to be an ellipsoid about a
-- centre some way above the neck pivot, a reflector is behind it when the
-- line of sight to the reflector passes through the head well before it
-- reaches it. The clip is worn on the head, not in it, so a head whose
-- ellipsoid would take in the middle of the clip's lower reflectors (where
-- the pivot is not yet learnt, say) is taken to be smaller, just reaching
-- it. Reflectors are on the head's surface, a little out from it or a
-- little into the ellipsoid as heads go, so only a line of sight that
-- enters the head more than 'surfaceMM' before the reflector counts.
--
-- Both are predictions, from a pose and a head shape that are only about
-- right; near where a reflector goes out of sight it may be seen or not.
--
-- Tracking doesn't use them. In simulation (a clip standing off the
-- temple, the head turning 110 degrees each way, looking up 25 to 50
-- degrees while glancing, and sweeping and glancing as usual, the head
-- hiding what it is in front of), not looking for a reflector predicted
-- out of sight, and following the other two for longer while it is, made
-- no difference turning and sweeping with the pivot learnt; looking up, it
-- had tracking more than 5 degrees off 12-44% less often but lost 55-90%
-- more often; and with the pivot not yet learnt, the head taken to be in
-- the wrong place, it put tracking 5 degrees off 1.7 to 2.3 times as often
-- turning far.

-- | A head, as an ellipsoid, in the head's axes (see Note [Mounting] in
-- "TrackKat.Pose"): its centre from the neck pivot, and its half-widths
-- across, up and down, and front to back, mm.
data HeadShape = HeadShape
    { headCentre :: V3
    , headRadii :: V3
    }
    deriving (Show)

-- | A head of the usual size: 150 mm across, 210 from chin to crown and 190
-- from front to back, its centre 60 mm above the top of the neck and 20 in
-- front of it.
usualHead :: HeadShape
usualHead = HeadShape (V3 0 (-60) (-20)) (V3 75 105 95)

-- | How mm a line of sight must pass through the head before reaching a
-- reflector for the head to hide it.
surfaceMM :: Double
surfaceMM = 20

-- | Whether a reflector can be seen.
data Sighted = InSight | FacingAway | BehindHead
    deriving (Eq, Show)

-- | Whether the camera can see a reflector of a clip on a head of the given
-- shape, turning about the given pivot (in the model's axes), at a pose.
sighted :: HeadShape -> V3 -> Model -> Pose -> Reflector -> Sighted
sighted shape pivot m pose which
    | tapeFacesAway m pose which = FacingAway
    | behindHead shape pivot identity pose (at which) = BehindHead
    | otherwise = InSight
  where
    at LeftReflector = mLeft m
    at RightReflector = mRight m
    at TopReflector = mTop m

-- | Whether a point, in the model's axes, is behind the head at a pose:
-- given the head's shape, the pivot (in the model's axes), and the turn from
-- the head's axes to the model's.
behindHead :: HeadShape -> V3 -> M33 -> Pose -> V3 -> Bool
behindHead (HeadShape centre (V3 a0 b0 c0)) pivot toModel (Pose r t) p = case entry of
    Just enters -> enters < distance - surfaceMM
    Nothing -> False
  where
    -- In the head's axes, about its centre, scaled so that the head is a
    -- unit sphere: the camera, and the way to the point.
    toHead v = mulMV (transpose toModel) v
    centreModel = pivot ^+^ mulMV toModel centre
    -- No bigger than to reach the middle of the clip, the model's origin.
    reach = norm (scaledBy a0 b0 c0 (toHead ((-1) *^ centreModel)))
    shrink = min 1 reach
    (a, b, c) = (shrink * a0, shrink * b0, shrink * c0)
    scaledBy x0 y0 z0 (V3 x y z) = V3 (x / x0) (y / y0) (z / z0)
    scaled = scaledBy a b c
    cameraModel = mulMV (transpose r) ((-1) *^ t)
    distance = norm (p ^-^ cameraModel)
    way = (1 / distance) *^ (p ^-^ cameraModel)
    o = scaled (toHead (cameraModel ^-^ centreModel))
    d = scaled (toHead way)
    -- Where, in mm from the camera, the line of sight enters the head.
    qa = dot d d
    qb = 2 * dot o d
    qc = dot o o - 1
    disc = qb * qb - 4 * qa * qc
    entry
        | disc < 0 = Nothing
        | otherwise = Just ((negate qb - sqrt disc) / (2 * qa))
