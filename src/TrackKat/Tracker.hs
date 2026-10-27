-- | Turning camera frames into head poses, frame by frame.
module TrackKat.Tracker (
    Tracker,
    newTracker,
    trackerCalibration,
    trackerCentre,
    Request (..),
    request,
    Event (..),
    Outcome (..),
    step,
) where

import TrackKat.Calibration
import TrackKat.Frame (Stripe, blobX, blobY, stripesToBlobs)
import TrackKat.Linear (M33, mulMM, norm, transpose)
import TrackKat.Pose

-- | Something the user asked for, carried out on the next tracked frame.
data Request = Recenter | Calibrate
    deriving (Eq, Show)

-- | Something that happened while handling a frame.
data Event
    = Recentered
    | CalibrationStarted
    | -- | The level was calibrated on request and applied at once; the
      -- calibration with it.
      Calibrated Calibration
    | CalibrationFailed
    | -- | Everyday movement changed the calibration noticeably; what is
      -- applied follows it slowly.
      Refined Refinement Calibration
    deriving (Show)

-- | What a frame showed.
data Outcome
    = Tracked
        { outPose :: HeadPose
        , outDistance :: Double
        -- ^ mm from the camera to the clip
        , outFit :: Double
        -- ^ RMS reprojection error, pixels
        }
    | -- | Not exactly three reflectors; how many blobs there were.
      Lost Int
    deriving (Show)

-- | A requested calibration of level in progress.
data Sweep
    = Sweep
        -- | deadline, in seconds on the clock given to 'step'
        Double
        -- | rotations relative to the reference, newest first
        [M33]

data Tracker = Tracker
    { trIntrinsics :: Intrinsics
    , trModel :: Model
    , trPrevious :: Maybe Pose
    -- ^ last frame's pose, the solver's starting point
    , trReference :: Maybe Pose
    , trLearner :: Learner
    , trSweep :: Maybe Sweep
    , trPending :: [Request]
    -- ^ oldest first
    }

-- | A tracker that starts from the given calibration and recenters on the
-- first frame it tracks.
newTracker :: Intrinsics -> Model -> Calibration -> Tracker
newTracker k m c = Tracker k m Nothing Nothing (newLearner c) Nothing [Recenter]

-- | What the tracker has learnt so far.
trackerCalibration :: Tracker -> Calibration
trackerCalibration = learnerCalibration . trLearner

-- | The clip's pose at the last recenter.
trackerCentre :: Tracker -> Maybe Pose
trackerCentre = trReference

request :: Request -> Tracker -> Tracker
request r t = t{trPending = trPending t ++ [r]}

-- | Seconds of head turning to calibrate level from.
calibrationSeconds :: Double
calibrationSeconds = 5

-- | Blobs smaller or larger than this many pixels are not reflectors.
minBlobPixels, maxBlobPixels :: Int
minBlobPixels = 15
maxBlobPixels = 2500

-- | Handle one frame's stripes, seen at the given time in seconds. Pending
-- requests wait for a frame where the clip is tracked.
step :: Double -> [Stripe] -> Tracker -> (Tracker, Outcome, [Event])
step now stripes t =
    case identify [(blobX b, blobY b) | b <- blobs] >>= solvePose (trIntrinsics t) (trModel t) (trPrevious t) of
        Nothing -> (t{trPrevious = Nothing, trLearner = lostSight (trLearner t)}, Lost (length blobs), [])
        Just (pose, fit) ->
            let (t1, requested) = foldl (handle pose) (t{trPrevious = Just pose, trPending = []}, []) (trPending t)
                ref = maybe pose id (trReference t1)
                (learner, refinements) = learn now pose (trLearner t1)
                (t2, calibrated) = calibrate pose ref t1{trLearner = learner}
                applied = trLearner t2
             in ( t2
                , Tracked (relativePose (appliedLevel applied) (appliedPivot applied) ref pose) (norm (poseT pose)) fit
                , reverse requested ++ [Refined r (learnerCalibration learner) | r <- refinements] ++ calibrated
                )
  where
    blobs = stripesToBlobs minBlobPixels maxBlobPixels stripes
    handle pose (acc, evs) Recenter = (acc{trReference = Just pose}, Recentered : evs)
    handle _ (acc, evs) Calibrate =
        (acc{trSweep = Just (Sweep (now + calibrationSeconds) [])}, CalibrationStarted : evs)
    calibrate pose ref acc = case trSweep acc of
        Nothing -> (acc, [])
        Just (Sweep deadline rots)
            | now < deadline ->
                (acc{trSweep = Just (Sweep deadline (poseR pose `mulMM` transpose (poseR ref) : rots))}, [])
            | otherwise -> case levelFromYaws rots of
                Nothing -> (acc{trSweep = Nothing}, [CalibrationFailed])
                Just l ->
                    let acc' = acc{trSweep = Nothing, trLearner = calibrateLevel l (trLearner acc)}
                     in (acc', [Calibrated (trackerCalibration acc')])
