-- | What trackkat's window is told to show; "TrackKat.Gui" tells it.
module TrackKat.Display (
    CameraState (..),
    Sighting (..),
    Output (..),
    UpdateCheck (..),
    UpdateStatus (..),
    Notice (..),
) where

import TrackKat.Pose (HeadPose, Reflector)

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
