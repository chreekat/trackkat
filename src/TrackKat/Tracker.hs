-- | Turning camera frames into head poses, frame by frame.
module TrackKat.Tracker (
    Tracker,
    newTracker,
    Request (..),
    request,
    Event (..),
    Outcome (..),
    step,
) where

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
    | Calibrated Level
    | CalibrationFailed
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

data Calibration
    = Calibration
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
    , trLevel :: Level
    , trCalibration :: Maybe Calibration
    , trPending :: [Request]
    -- ^ oldest first
    }

-- | A tracker that recenters on the first frame it tracks.
newTracker :: Intrinsics -> Model -> Tracker
newTracker k m = Tracker k m Nothing Nothing cameraLevel Nothing [Recenter]

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
        Nothing -> (t{trPrevious = Nothing}, Lost (length blobs), [])
        Just (pose, fit) ->
            let (t1, requested) = foldl (handle pose) (t{trPrevious = Just pose, trPending = []}, []) (trPending t)
                ref = maybe pose id (trReference t1)
                (t2, calibrated) = calibrate pose ref t1
             in ( t2
                , Tracked (relativePose (trModel t2) (trLevel t2) ref pose) (norm (poseT pose)) fit
                , reverse requested ++ calibrated
                )
  where
    blobs = stripesToBlobs minBlobPixels maxBlobPixels stripes
    handle pose (acc, evs) Recenter = (acc{trReference = Just pose}, Recentered : evs)
    handle _ (acc, evs) Calibrate =
        (acc{trCalibration = Just (Calibration (now + calibrationSeconds) [])}, CalibrationStarted : evs)
    calibrate pose ref acc = case trCalibration acc of
        Nothing -> (acc, [])
        Just (Calibration deadline rots)
            | now < deadline ->
                (acc{trCalibration = Just (Calibration deadline (poseR pose `mulMM` transpose (poseR ref) : rots))}, [])
            | otherwise -> case levelFromYaws rots of
                Nothing -> (acc{trCalibration = Nothing}, [CalibrationFailed])
                Just l -> (acc{trCalibration = Nothing, trLevel = l}, [Calibrated l])
