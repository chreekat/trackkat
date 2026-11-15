-- | The terms a hypothesis about a frame is judged by (see "TrackKat.Term"):
-- the gates that say whether a pose can be the clip's, and the scores that
-- hypotheses are compared by.
--
-- Each is a function of just what it judges by: where a head can be
-- ('Reach'), the time since the last pose, the sizes the reflectors have
-- had. What they promise is tested one by one: a gate that lets something
-- through lets through anything nearer, slower or bigger, and a score
-- falls the further off its figure is.
module TrackKat.Tracker.Judge (
    -- * What is judged
    Reach (..),
    Moved (..),
    Found (..),
    metres,

    -- * Gates
    canBeThere,
    jumpable,
    followable,
    bigEnough,
    clipLike,

    -- * Scores
    misfit,
    continuing,
    anywhere,
    afreshCost,
    usualSize,
    blobsSay,

    -- * Limits
    maxTurn,
    maxMove,
    nearest,
    farthest,
    maxFit,
    smallestShare,
) where

import Data.Foldable (toList)

import TrackKat.Frame (Blob (..))
import TrackKat.Linear (V3, dot, norm, normalize, (*^), (^-^))
import TrackKat.Match
import TrackKat.Pose
import TrackKat.Term
import TrackKat.World (Seconds, World (..), during, inDegrees, inMillimetres, inPerSecond, inPixels, seconds, times, world)

-- | What says where a head can be.
data Reach = Reach
    { reachPivot :: V3
    -- ^ the point of the clip's axes the head turns about
    , reachCentre :: Maybe Pose
    -- ^ the clip's pose at the last recenter
    }

-- | A pose following another.
data Moved = Moved
    { movedPose :: Pose
    , movedFit :: Double
    -- ^ RMS reprojection error, pixels
    , movedTurn :: Double
    -- ^ degrees turned from the other
    , movedMove :: Double
    -- ^ mm its pivot moved from the other's
    }

-- | Three blobs taken for the clip by their layout: the pose, and the
-- blobs taken for the left, right and top reflectors.
data Found = Found Pose (Three Blob)

-- | How far a pose is from the camera, in metres.
metres :: Pose -> Double
metres pose = norm (poseT pose) / 1000

-- | The most a head turns (degrees) and moves (mm, at its pivot) between
-- frames the given number of seconds apart: from one frame to the next,
-- the world's 'headStepTurn' and 'headStepMove', which is what the shaping
-- for games allows, and more the longer apart, by its 'headTurnRate' and
-- 'headMoveRate'. A bigger jump means the blobs were matched to the wrong
-- reflectors.
maxTurn, maxMove :: Double -> Double
maxTurn dt = inDegrees (headStepTurn world) + inPerSecond (headTurnRate world) * dt
maxMove dt = inMillimetres (headStepMove world) + inPerSecond (headMoveRate world) * dt

-- | The spreads of how far a head turns (degrees) and its pivot moves (mm)
-- in the given seconds: the world's 'headTurnSpeed' and 'headMoveSpeed'
-- for that long, or for a frame if that is longer.
turnSpread, moveSpread :: Double -> Double
turnSpread dt = inDegrees (headTurnSpeed world `during` sinceSeen dt)
moveSpread dt = inMillimetres (headMoveSpeed world `during` sinceSeen dt)

sinceSeen :: Double -> Seconds
sinceSeen dt = seconds (max dt (1 / inPerSecond (frameRate world)))

-- | How much likelier, in nats, a pose is where a pose so many seconds
-- before says it most likely is than where a head most likely is with
-- nothing known of where it was: what finding the clip afresh gives up
-- (see Note [Following or finding afresh] in "TrackKat.Tracker.Choose").
--
-- It is the log of how much more room a head has anywhere than a head
-- followed has: in turning, the world's 'headTurnSpread' against
-- 'turnSpread', each of three ways; in place, the box 'canBeThere' allows
-- ('maxReachMove' to either side, and up or down, and from 'nearest' to
-- 'farthest') against a gaussian of 'moveSpread' each way. Neither is
-- less than nothing: after long enough, a pose says no more than that the
-- head is somewhere.
afreshCost :: Double -> Double
afreshCost dt = 3 * log (max 1 (inDegrees (headTurnSpread world) / turnSpread dt)) + log (max 1 (box / (moveSpread dt * sqrt (2 * pi)) ^ (3 :: Int)))
  where
    box = (2 * maxReachMove) ^ (2 :: Int) * (farthest - nearest)

-- | The farthest a head turns (degrees) and moves (mm, at its pivot) from
-- where it was recentered: three of the world's 'headTurnSpread', and its
-- 'headReachAside', the move to either side, or up or down, as the camera
-- sees it. Toward or away from the camera it goes as near as 'nearest' or
-- as far as 'farthest': people lean in, and sit back after a break. Beyond
-- that the blobs aren't the clip, but something like a phone's light.
maxReachTurn, maxReachMove :: Double
maxReachTurn = inDegrees (times 3 (headTurnSpread world))
maxReachMove = inMillimetres (headReachAside world)

-- | The nearest and farthest, in mm, the clip can be from the camera: the
-- world's 'cameraNearest' and 'cameraFarthest'.
nearest, farthest :: Double
nearest = inMillimetres (cameraNearest world)
farthest = inMillimetres (cameraFarthest world)

-- | The worst RMS reprojection error, in pixels, of a pose matched from a
-- prediction: twice the world's 'centroidNoise'.
maxFit :: Double
maxFit = inPixels (times 2 (centroidNoise world))

-- | When finding the clip afresh, none of its blobs lights less than one
-- in this many of the pixels its reflector has lately, at the pose's
-- distance.
smallestShare :: Double
smallestShare = 8

-- | Somewhere a head can be: at a sane distance from the camera, and not
-- too far to the side of where it was recentered, though as near or as
-- far from the camera as it likes.
canBeThere :: Reach -> Term Pose
canBeThere (Reach pivot centre) =
    within
        ( Name "near" "too close to the camera"
        , Name "far" "too far from the camera"
        )
        "mm"
        (nearest, farthest)
        (norm . poseT)
        <> maybe mempty reach centre
  where
    reach ref =
        atMost (Name "turned" "turned further from where you recentered than a head turns") "degrees" maxReachTurn (\pose -> fst (poseChange pivot ref pose))
            <> atMost (Name "aside" "too far to the side of where you recentered") "mm" maxReachMove (aside ref)
    aside ref pose =
        let fromCamera = normalize (poseAt ref pivot)
            moved = poseAt pose pivot ^-^ poseAt ref pivot
            back = dot moved fromCamera
         in norm (moved ^-^ back *^ fromCamera)

-- | A pose isn't an impossible jump from the last, so many seconds
-- before.
jumpable :: Double -> Term Moved
jumpable dt =
    gate
        (Name "turn" "the head would have turned faster than heads turn")
        (\moved -> movedTurn moved / maxTurn dt <= 1)
        (\moved -> Measure (movedTurn moved) AtMost (maxTurn dt) "degrees")
        <> gate
            (Name "move" "the head would have moved faster than heads move")
            (\moved -> movedMove moved / maxMove dt <= 1)
            (\moved -> Measure (movedMove moved) AtMost (maxMove dt) "mm")

-- | A pose follows the last, so many seconds before: it fits what was
-- seen, it isn't an impossible jump, and it is somewhere a head can be.
followable :: Reach -> Double -> Term Moved
followable reach dt =
    atMost (Name "fit" "the lights in view don't fit the clip's shape") "px" maxFit movedFit
        <> jumpable dt
        <> about movedPose (canBeThere reach)

-- | None of the blobs far smaller than its reflector has lately been, of
-- the sizes given, at the pose's distance: lights much nearer the camera
-- than the head, laid out like the clip, light far fewer pixels than its
-- reflectors would there. Its figure is that of the blob smallest for its
-- reflector: its pixels, against the least its reflector would light.
bigEnough :: Three Size -> Term Found
bigEnough sizes =
    gate
        (Name "size" "the lights in view are much smaller than the clip's reflectors have been")
        (\(Found pose three) -> and (liftA2 (\size b -> fromIntegral (blobPoints b) * smallestShare * metres pose ^ (2 :: Int) >= sizeUsual size) sizes three))
        ( \(Found pose three) ->
            let least size = sizeUsual size / (smallestShare * metres pose ^ (2 :: Int))
                Three l r top = liftA2 (\size b -> (fromIntegral (blobPoints b), least size)) sizes three
                smaller a b = if fst a - snd a <= fst b - snd b then a else b
                (lit, wanted) = smaller l (smaller r top)
             in Measure lit AtLeast wanted "px"
        )

-- | Three blobs at a pose are the clip: the pose is somewhere a head can
-- be, and the blobs are big enough.
clipLike :: Reach -> Three Size -> Term Found
clipLike reach sizes = about (\(Found pose _) -> pose) (canBeThere reach) <> bigEnough sizes

-- | How likely the reflectors are seen so far from where a pose puts
-- them: each a centre out by the fit, under the world's 'centroidNoise'.
misfit :: Term Moved
misfit = gaussian (Name "misfit" "the lights in view don't quite fit the clip's shape") "px" (inPixels (centroidNoise world) / sqrt 3) movedFit

-- | How likely a head turned and moved as far as a pose is from the
-- last, so many seconds before.
continuing :: Double -> Term Moved
continuing dt =
    gaussian (Name "turning" "the head would have turned faster than heads mostly turn") "degrees" (turnSpread dt) movedTurn
        <> gaussian (Name "moving" "the head would have moved faster than heads mostly move") "mm" (moveSpread dt) movedMove

-- | How likely a head is at a pose with nothing known of where it was,
-- beside how likely one followed for so many seconds is where it was:
-- anywhere it can be, and turned from the centre as heads are.
anywhere :: Reach -> Double -> Term Moved
anywhere (Reach pivot centre) dt =
    cost (Name "afresh" "nothing says where the clip would be") (const (afreshCost dt))
        <> maybe mempty (\ref -> gaussian (Name "reach" "turned further than heads mostly turn") "degrees" (inDegrees (headTurnSpread world)) (\moved -> fst (poseChange pivot ref (movedPose moved)))) centre

-- | How likely each blob that is a reflector's own is the size its
-- reflector has been lately, of the sizes given, at the pose's distance,
-- as a match's terms for sizes score a size. (Against the usual size, not
-- the latest as matching goes by: the latest is whatever the last frame
-- took for the reflector, which is what is in question.)
usualSize :: Three Size -> Term (Pose, Three (Maybe Sight))
usualSize sizes =
    likelihood (Name "usual size" "the lights in view aren't the size the clip's reflectors have been") $ \(pose, sights) ->
        sum [nats (sizeScore (sizeUsual size / metres pose ^ (2 :: Int)) (sightPixels sight)) | (size, Just sight) <- toList ((,) <$> sizes <*> sights), sightKind sight /= Merged]

-- | What the blobs taken for the reflectors at a pose say of it: how
-- likely they are the sizes the reflectors have been ('usualSize'), and
-- how well they fit it ('misfit').
blobsSay :: Three Size -> Pose -> Three (Maybe Sight) -> Term Moved
blobsSay sizes pose sights = about (const (pose, sights)) (usualSize sizes) <> misfit
