-- | Head poses in the conventions games receive them in.
module TrackKat.GamePose (
    GamePose (..),
    toGamePose,
    still,
) where

import TrackKat.Pose (HeadPose (..))

-- | FreeTrack's conventions, which our NPClient DLL passes on.
data GamePose = GamePose
    { gpYaw :: Float
    -- ^ radians, positive to the left
    , gpPitch :: Float
    -- ^ radians, positive down
    , gpRoll :: Float
    -- ^ radians, positive to the left
    , gpX :: Float
    -- ^ mm, positive to the user's left
    , gpY :: Float
    -- ^ mm, positive up
    , gpZ :: Float
    -- ^ mm, positive away from the camera
    }
    deriving (Eq, Show)

-- | Convert a head pose, with the signs DCS expects.
toGamePose :: HeadPose -> GamePose
toGamePose hp =
    GamePose
        { gpYaw = rad (negate (yaw hp))
        , gpPitch = rad (pitch hp)
        , gpRoll = rad (roll hp)
        , gpX = realToFrac (tx hp)
        , gpY = realToFrac (negate (ty hp))
        , gpZ = realToFrac (tz hp)
        }
  where
    rad d = realToFrac (d * pi / 180)

-- | The head at its centre.
still :: GamePose
still = GamePose 0 0 0 0 0 0
