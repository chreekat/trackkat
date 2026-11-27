-- | What trackkat's window is told to show; "TrackKat.Gui" tells it.
module TrackKat.Display (
    CameraState (..),
    Sighting (..),
    Output (..),
    UpdateCheck (..),
    UpdateStatus (..),
    Notice (..),
    Shown (..),
    nothingShown,
    shownHead,
    Tracked (..),
    tracked,
) where

import TrackKat.Frame (Blob)
import TrackKat.Linear (V3)
import TrackKat.Pose (HeadPose (..), Reflector)
import TrackKat.Response (Aiming (..), Trim (..))
import TrackKat.Trackers (BlobRole)

-- | What the camera is doing, for the window's Camera light.
data CameraState
    = Searching
    | Tracking
    | -- | why not
      CameraDown String
    deriving (Eq, Show)

-- | What a frame showed of the clip.
data Sighting
    = -- | the head, and the reflectors out of sight
      InSight HeadPose [Reflector]
    | -- | how many reflectors were seen
      OutOfSight Int
    deriving (Eq, Show)

-- | Whether games see the head move.
data Output
    = Live
    | -- | the view held
      Paused HeadPose
    deriving (Eq, Show)

-- | Whether trackkat is looking for a newer version.
data UpdateCheck = CheckRunning | CheckIdle
    deriving (Eq, Show)

-- | What the last look for a newer version found, in a few words.
data UpdateStatus
    = -- | none is newer, as the words say; shown with a tick
      UpToDate String
    | -- | anything else, such as that it couldn't look
      UpdateNote String
    deriving (Eq, Show)

-- | Something offered in the strip across the top of the window.
data Notice = Notice
    { noticeText :: String
    -- ^ what the strip says
    , noticeButton :: String
    -- ^ what its button says
    , noticeTray :: Maybe String
    -- ^ its item in the tray's menu, if it has one
    }
    deriving (Eq, Show)

-- | Everything the window has been told to show, as the last of each told
-- it.
data Shown = Shown
    { shownVersion :: !String
    , shownCamera :: !CameraState
    , shownSighting :: !Sighting
    , shownView :: !HeadPose
    -- ^ the pose games are sent
    , shownBlobs :: ![(Blob, BlobRole)]
    -- ^ the last frame's, heaviest first
    , shownAreas :: ![((Double, Double), Double)]
    -- ^ masked around still lights: centre and radius in the camera's image
    , shownClip :: !(Maybe (V3, V3, V3))
    -- ^ the left, right and top reflectors in mm from the neck pivot along
    -- the head's axes, once a calibration puts them
    , shownOutput :: !Output
    , shownTrim :: !Trim
    , shownAiming :: !Aiming
    , shownGames :: !Int
    -- ^ tracking
    , shownGameName :: !(Maybe String)
    -- ^ one of them, if its name is known
    , shownStats :: !String
    , shownCheck :: !UpdateCheck
    , shownStatus :: !(Maybe UpdateStatus)
    -- ^ what the last look for a newer version found, if there was one
    , shownOffer :: !(Maybe Notice)
    }
    deriving (Eq, Show)

-- | What the window shows before it is told anything.
nothingShown :: Shown
nothingShown =
    Shown
        { shownVersion = "development build"
        , shownCamera = Searching
        , shownSighting = OutOfSight 0
        , shownView = still
        , shownBlobs = []
        , shownAreas = []
        , shownClip = Nothing
        , shownOutput = Live
        , shownTrim = NoTrim
        , shownAiming = NotAiming
        , shownGames = 0
        , shownGameName = Nothing
        , shownStats = ""
        , shownCheck = CheckIdle
        , shownStatus = Nothing
        , shownOffer = Nothing
        }
  where
    still = HeadPose 0 0 0 0 0 0

-- | The head the window shows: as last seen, or while the clip is out of
-- sight, the pose games are sent.
shownHead :: Shown -> HeadPose
shownHead s = case shownSighting s of
    InSight hp _ -> hp
    OutOfSight _ -> shownView s

-- | Whether the head the window shows is the head as tracked now.
data Tracked = TrackedNow | NotTrackedNow
    deriving (Eq, Show)

-- | Tracked now while the camera tracks and the clip is in sight.
tracked :: Shown -> Tracked
tracked s = case (shownCamera s, shownSighting s) of
    (Tracking, InSight _ _) -> TrackedNow
    _ -> NotTrackedNow
