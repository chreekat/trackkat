-- | Simulated heads and clips in front of the camera, tracked by the old
-- tracker: "Test.Simulation"'s clips, heads and scenes, with its tracking
-- and what tracking came to as the old tracker gives them.
module Test.Old.Simulation (
    -- * Clips, heads and scenes
    module Test.Simulation,

    -- * Tracking
    Row,
    trackFrames,
    trackFramesEvents,
    trackFramesEventsWith,
    trackPath,
    trackNoisy,

    -- * What tracking came to
    corrections,
    isMirrored,
    mirroredOn,
    lastMirrored,
    lastMirroredOn,
    outputError,
    turnError,
    lostCount,
    isLost,
    worstError,
    seens,
    unmerged,
    after,
    summed,
) where

import Data.List (mapAccumL)

import Test.Simulation hiding (
    Row,
    after,
    corrections,
    isLost,
    isMirrored,
    lastMirrored,
    lastMirroredOn,
    lostCount,
    mirroredOn,
    outputError,
    seens,
    summed,
    trackFrames,
    trackFramesEvents,
    trackFramesEventsWith,
    trackNoisy,
    trackPath,
    turnError,
    unmerged,
    worstError,
 )
import TrackKat.Calibration
import TrackKat.Config
import TrackKat.Frame
import TrackKat.Linear
import TrackKat.Old.Tracker
import TrackKat.Pose
import TrackKat.Synth
import TrackKat.World (clockAt)

-- | A frame's time, the head's true pose, what tracking made of the frame,
-- and the pose tracked.
type Row = (Double, Pose, Outcome, Maybe Pose)

-- | Track frames of (time, true pose, stripes) from a fresh tracker: each
-- frame's time, true pose, outcome, and the pose tracked.
trackFrames :: Model -> [(Double, Pose, [Stripe])] -> [Row]
trackFrames m = map fst . snd . trackFramesEvents m

-- | Track frames as 'trackFrames' does, with each frame's events, and the
-- tracker at the end.
trackFramesEvents :: Model -> [(Double, Pose, [Stripe])] -> (Tracker, [(Row, [Event])])
trackFramesEvents = trackFramesEventsWith (lightMasking defaultConfig)

-- | Track frames as 'trackFramesEvents' does, still lights masked or not.
trackFramesEventsWith :: Masking -> Model -> [(Double, Pose, [Stripe])] -> (Tracker, [(Row, [Event])])
trackFramesEventsWith masking m = mapAccumL frame (newTracker defaultIntrinsics m masking defaultCalibration)
  where
    frame t (time, truth, stripes) = let (t', o, evs) = step (clockAt time) stripes t in (t', ((time, truth, o, trackerPose t'), evs))

-- | Track a head along a path of (time, true pose, how lit each reflector is).
trackPath :: Model -> [(Double, Pose, (Double, Double, Double))] -> [Row]
trackPath m path = trackFrames m (rendered m path)

-- | Track a head along a path as 'trackPath' does, with each reflector's
-- centre moved by up to half the given pixels, differently each frame, as
-- noise moves a blob's centroid.
trackNoisy :: Model -> Double -> [(Double, Pose, (Double, Double, Double))] -> (Tracker, [(Row, [Event])])
trackNoisy m amp path =
    trackFramesEvents m [(t, pose, renderDiscs (jitter amp i (pointDiscs defaultIntrinsics m lit pose))) | (i, (t, pose, lit)) <- zip [0 :: Int ..] path]

-- | Times of the frames where a pose was corrected.
corrections :: [(Row, [Event])] -> [Double]
corrections rows = [t | ((t, _, _, _), evs) <- rows, any isCorrected evs]
  where
    isCorrected Corrected{} = True
    isCorrected _ = False

-- | Whether the pose tracked is the mirror image of the true pose rather
-- than the true pose.
isMirrored :: Row -> Bool
isMirrored = mirroredOn clipBent

-- | Whether the pose tracked is the mirror image of the true pose of a
-- clip of the given shape rather than the true pose.
mirroredOn :: Model -> Row -> Bool
mirroredOn m (_, truth, _, tracked) = case (mirrorPose defaultIntrinsics m truth, tracked) of
    (Just other, Just p) -> fst (poseError other p) < fst (poseError truth p) && fst (poseError truth p) > 3 && fst (poseError truth other) > 10
    _ -> False

-- | The time of the last frame tracked on the mirror image, or 0.
lastMirrored :: [Row] -> Double
lastMirrored = lastMirroredOn clipBent

-- | The time of the last frame tracked on the mirror image of a clip of
-- the given shape, or 0.
lastMirroredOn :: Model -> [Row] -> Double
lastMirroredOn m rows = maximum (0 : [t | r@(t, _, _, _) <- rows, mirroredOn m r])

-- | The worst error, in degrees, of the yaw and pitch reported, against
-- the head's true yaw and pitch at each time.
outputError :: (Double -> (Double, Double)) -> [Row] -> Double
outputError turned rows = maximum (0 : [max (abs (yaw hp - y)) (abs (pitch hp - p)) | (t, _, Tracked{outPose = hp}, _) <- rows, let (y, p) = turned t])

lostCount :: [Row] -> Int
lostCount rows = length [() | (_, _, Lost{}, _) <- rows]

isLost :: Outcome -> Bool
isLost Lost{} = True
isLost Tracked{} = False

-- | The worst error of the poses tracked: degrees, mm.
worstError :: [Row] -> (Double, Double)
worstError rows = (maximum (0 : map fst errors), maximum (0 : map snd errors))
  where
    errors = [poseError truth p | (_, truth, Tracked{}, Just p) <- rows]

seens :: [Row] -> [Seen]
seens rows = [s | (_, _, Tracked{outSeen = s}, _) <- rows]

-- | The frames where no reflectors shared a blob.
unmerged :: [Row] -> [Row]
unmerged rows = [r | r@(_, _, Tracked{outSeen = Seen{seenMerged = 0}}, _) <- rows]

-- | The frames from the given time on.
after :: Double -> [Row] -> [Row]
after t0 = filter (\(t, _, _, _) -> t >= t0)

-- | The mean and worst errors of the turning and movement tracked from
-- the first frame, against the head's: degrees, and mm at the clip's
-- middle.
turnError :: [Row] -> ((Double, Double), (Double, Double))
turnError rows = case [(truth, p) | (_, truth, Tracked{}, Just p) <- rows] of
    (truth0, p0) : rest ->
        let errors = [(fst (poseError turned seen), norm (middle turned ^-^ middle seen)) | (truth, p) <- rest, let turned = moved truth0 truth; seen = moved p0 p]
            mean xs = sum xs / fromIntegral (max 1 (length xs))
         in ((mean (map fst errors), mean (map snd errors)), (maximum (0 : map fst errors), maximum (0 : map snd errors)))
    [] -> ((0, 0), (0, 0))
  where
    -- The rigid movement from one pose to another.
    moved (Pose r0 t0) (Pose r t) = let r' = r `mulMM` transpose r0 in Pose r' (t ^-^ mulMV r' t0)
    middle pose = poseAt pose (V3 0 0 900)

-- | How many frames were lost, and the worst error of the poses tracked.
summed :: [Row] -> (Int, (Double, Double))
summed rows = (lostCount rows, worstError rows)
