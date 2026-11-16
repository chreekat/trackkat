-- | What every way of making out the clip in a frame goes by, and what
-- each makes of it.
module TrackKat.Tracker.Sighting (
    Known (..),
    Sighting (..),
) where

import TrackKat.Match (Sight, Size, Three)
import TrackKat.Pose (Intrinsics, Model, Pose)
import TrackKat.Term (Finding)
import TrackKat.Tracker.Judge (Reach)

-- | What is known of the camera and the clip going into a frame.
data Known = Known
    { knownCamera :: Intrinsics
    , knownClip :: Model
    , knownReach :: Reach
    , knownSizes :: Three Size
    -- ^ how big each reflector's blob has been lately
    , knownLast :: Maybe Pose
    -- ^ the clip's pose as last tracked, however long ago
    }

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
    -- ^ what each term that accepted the pose found
    }
