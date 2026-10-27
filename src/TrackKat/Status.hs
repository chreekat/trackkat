-- | trackkat's state as its window and tray icon show it.
module TrackKat.Status (
    Status (..),
    initialStatus,
    CameraState (..),
    Sighting (..),
    Output (..),
) where

import TrackKat.Pose (HeadPose (..))
import TrackKat.Response (Trim (..))
import TrackKat.Tracker (Seen)

-- | What trackkat's window and tray icon show: the camera, the head, what
-- games see, and figures for bug reports.
data Status = Status
    { stCamera :: !CameraState
    , stSighting :: !Sighting
    -- ^ what the last frame showed of the clip
    , stView :: !HeadPose
    -- ^ the pose games are sent
    , stOutput :: !Output
    , stTrim :: !Trim
    -- ^ where forward is for the view
    , stGames :: !Int
    -- ^ games tracking
    , stStats :: !String
    -- ^ the performance figures of the last second
    , stCalibration :: !String
    -- ^ what trackkat has learnt about the camera and neck, perhaps over
    -- several lines
    }

-- | Before anything is known: looking for the camera, nothing seen.
initialStatus :: Status
initialStatus =
    Status
        { stCamera = Searching
        , stSighting = OutOfSight 0
        , stView = HeadPose 0 0 0 0 0 0
        , stOutput = Live
        , stTrim = NoTrim
        , stGames = 0
        , stStats = ""
        , stCalibration = ""
        }

data CameraState
    = Searching
    | Tracking
    | -- | why not
      CameraDown String

-- | What a frame showed of the clip.
data Sighting
    = -- | the head, and how the reflectors appeared
      InSight HeadPose Seen
    | -- | how many reflectors were seen
      OutOfSight Int

-- | Whether games see the head move.
data Output = Live | Paused
