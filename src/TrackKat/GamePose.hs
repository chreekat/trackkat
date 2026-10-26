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
    -- ^ mm, positive to the user's right
    , gpY :: Float
    -- ^ mm, positive up
    , gpZ :: Float
    -- ^ mm, positive away from the camera
    }
    deriving (Eq, Show)

-- | Convert a head pose. Yaw and pitch signs are as DCS expects; roll and
-- translation signs are a best guess pending testing in games.
toGamePose :: HeadPose -> GamePose
toGamePose hp =
    GamePose
        { gpYaw = rad (negate (yaw hp))
        , gpPitch = rad (pitch hp)
        , gpRoll = rad (roll hp)
        , gpX = realToFrac (negate (tx hp))
        , gpY = realToFrac (negate (ty hp))
        , gpZ = realToFrac (tz hp)
        }
  where
    rad d = realToFrac (d * pi / 180)

-- | The head at its centre.
still :: GamePose
still = GamePose 0 0 0 0 0 0
