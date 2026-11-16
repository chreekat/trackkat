-- | What the tests of the tracker's stages take as known going into a
-- frame: the headset clip before the default camera, with nothing learnt
-- or as after tracking it at a pose.
module Test.Tracker.Known (
    pivot,
    facing,
    knowing,
    taught,
    lastAt,
    tracking,
    lastTracked,
    aFrameOn,
    asReflectors,
) where

import Data.List (minimumBy)
import Data.Ord (comparing)

import Test.Gen
import Test.Simulation (headset)
import TrackKat.Frame (Blob (..))
import TrackKat.Linear
import TrackKat.Match (Three (..), reflectors)
import TrackKat.Pose
import TrackKat.Tracker.Account
import TrackKat.Tracker.Blobs (at, distance2)
import TrackKat.Tracker.Presence (Presence (..))
import TrackKat.Tracker.Sizes
import TrackKat.Tracker.Trail (Recent (..), Trail (..))
import TrackKat.World (Instant, clockAt, seconds, (.+^))

-- | Where the head turns about, in the clip's axes.
pivot :: V3
pivot = V3 0 100 100

-- | The clip squarely facing the camera, 700 mm off.
facing :: Pose
facing = Pose identity (V3 0 0 700)

-- | What is known with nothing learnt: the headset's clip before the
-- default camera, recentered nowhere, its reflectors not yet seen, the
-- clip never made out, nothing doubted.
knowing :: Known
knowing = Known defaultIntrinsics headset (Reach pivot Nothing) (pure unlearnt) Nothing Nothing 0 Nothing

-- | The blob the camera shows of each reflector of the clip at a pose
-- where each shows as a blob of its own: that nearest where the pose puts
-- the reflector.
asReflectors :: Placing -> Maybe (Three Blob)
asReflectors placing = case clipBlobs headset placing of
    blobs@[_, _, _] -> Just ((\which -> minimumBy (comparing (distance2 (seenAt defaultIntrinsics headset (placed placing) which) . at)) blobs) <$> reflectors)
    _ -> Nothing

-- | What is known after tracking the clip at a pose where each reflector
-- shows as a blob of its own, until 'lastTracked': its sizes learnt from
-- the blobs it shows there, every reflector showing, the clip made out.
taught :: Placing -> Known
taught placing =
    knowing
        { knownSizes = maybe (pure unlearnt) (fmap learn) (asReflectors placing)
        , knownShowed = Just (pure Present, lastTracked)
        , knownMadeOut = Just lastTracked
        , knownLast = Just pose
        }
  where
    pose = placed placing
    learn b = iterate (learntFrom (metres pose) (fromIntegral (blobPoints b))) unlearnt !! 200

-- | A pose tracked at 'lastTracked', all three reflectors showing, and
-- tracked since the clock began.
lastAt :: Pose -> Recent
lastAt pose = Recent pose lastTracked lastTracked (clockAt 0)

-- | The poses that predict after tracking the clip at a pose: that one.
tracking :: Placing -> Trail
tracking placing = Trail (Just (lastAt (placed placing))) Nothing

-- | When 'lastAt' has its pose tracked, and a frame after.
lastTracked, aFrameOn :: Instant
lastTracked = clockAt 1
aFrameOn = lastTracked .+^ seconds (1 / 120)
