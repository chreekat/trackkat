-- | What a way of making out the clip in a frame makes of it.
module TrackKat.Tracker.Sighting (
    Sighting (..),
) where

import TrackKat.Pose (Pose)
import TrackKat.Sight (Sight, Three)
import TrackKat.Term (Finding)

-- | The clip made out in a frame.
data Sighting = Sighting
    { sightingPose :: Pose
    , sightingFit :: Double
    -- ^ RMS reprojection error, pixels
    , sightingSights :: Three (Maybe Sight)
    -- ^ how each reflector showed
    , sightingStrays :: Int
    -- ^ blobs that weren't reflectors
    , sightingMasked :: Int
    -- ^ blobs ignored as part of a still light
    , sightingJudged :: [Finding]
    -- ^ what each term of its score found
    }
