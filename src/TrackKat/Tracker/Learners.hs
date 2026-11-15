-- | What learns from the poses tracked: the level and the neck's pivot
-- (see "TrackKat.Calibration"), the clip's shape (see "TrackKat.Shape"),
-- and a calibration of level asked for, from a few seconds of turning.
module TrackKat.Tracker.Learners (
    Learners (..),
    newLearners,
    changeClipOf,
    lostSightOf,
    startOver,
    learnFrom,

    -- * A calibration asked for
    calibrationSeconds,
    startSweep,
    sweepOn,
) where

import TrackKat.Calibration
import TrackKat.Linear (M33, V3, mulMM, transpose)
import TrackKat.Pose
import TrackKat.Shape
import TrackKat.Tracker.Outcome (Event (..))

-- | A requested calibration of level in progress.
data Sweep
    = Sweep
        -- | deadline, in seconds on the tracker's clock
        Double
        -- | rotations relative to the reference, newest first
        [M33]

-- | What learns from the poses tracked.
data Learners = Learners
    { learnersPivot :: !Learner
    -- ^ of the level and the neck's pivot
    , learnersShape :: !ShapeLearner
    , learnersSweep :: !(Maybe Sweep)
    }

-- | Nothing learnt yet of a clip, beyond the calibration given.
newLearners :: Model -> Calibration -> Learners
newLearners m c = Learners (newLearner m c) (newShapeLearner m) Nothing

-- | Learning of a differently shaped clip.
changeClipOf :: Model -> Learners -> Learners
changeClipOf m learners = learners{learnersPivot = changeClip m (learnersPivot learners), learnersShape = newShapeLearner m}

-- | After a frame whose pose isn't learnt from.
lostSightOf :: Learners -> Learners
lostSightOf learners = learners{learnersPivot = lostSight (learnersPivot learners), learnersShape = shapeLostSight (learnersShape learners)}

-- | After a pose was switched for its mirror image: learning in progress
-- was of the mirror image, and so was a requested calibration's turning.
startOver :: Learners -> Learners
startOver learners = (lostSightOf learners){learnersSweep = fmap (\(Sweep deadline _) -> Sweep deadline []) (learnersSweep learners)}

-- | Learn from a pose tracked at the given time, the head turning about
-- the given point of the clip's axes: what was learnt, and what of it is
-- worth telling.
learnFrom :: Intrinsics -> Double -> V3 -> Pose -> Learners -> (Learners, [Refinement], Maybe ShapeEstimate)
learnFrom k now pivot pose learners = (learners{learnersPivot = learner, learnersShape = shapes}, refinements, shapeNews)
  where
    (learner, refinements) = learn now pose (learnersPivot learners)
    (shapes, shapeNews) = watchShape k now pivot pose (learnersShape learners)

-- | Seconds of head turning to calibrate level from.
calibrationSeconds :: Double
calibrationSeconds = 5

-- | Start calibrating level, at the given time.
startSweep :: Double -> Learners -> Learners
startSweep now learners = learners{learnersSweep = Just (Sweep (now + calibrationSeconds) [])}

-- | Take a pose tracked at the given time, and the pose recentered on,
-- into a calibration of level in progress, if one is; once it has
-- watched for long enough, the level is calibrated from the turning seen
-- and applied at once, or it has failed.
sweepOn :: Double -> Pose -> Pose -> Learners -> (Learners, [Event])
sweepOn now pose ref learners = case learnersSweep learners of
    Nothing -> (learners, [])
    Just (Sweep deadline rots)
        | now < deadline ->
            (learners{learnersSweep = Just (Sweep deadline (poseR pose `mulMM` transpose (poseR ref) : rots))}, [])
        | otherwise -> case levelFromYaws rots of
            Nothing -> (learners{learnersSweep = Nothing}, [CalibrationFailed])
            Just l ->
                let learner = calibrateLevel l (learnersPivot learners)
                 in (learners{learnersSweep = Nothing, learnersPivot = learner}, [Calibrated (learnerCalibration learner)])
