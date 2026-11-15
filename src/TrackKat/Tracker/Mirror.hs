-- | Which of the two poses that fit a tracked frame is the head's (see
-- Note [Which pose is the head's] in "TrackKat.Branch"), as the tracker
-- keeps it: what is believed of the pose tracked and the one recentered
-- on, how clearly a frame shows the reflectors, and what a frame's pose
-- comes to once that is weighed.
module TrackKat.Tracker.Mirror (
    Centre (..),
    recenterOn,
    clarity,
    Told (..),
    tellApart,
) where

import Data.Maybe (fromMaybe, isJust)

import TrackKat.Branch
import TrackKat.Frame (Blob)
import TrackKat.Linear (V3)
import TrackKat.Match
import TrackKat.Pose
import TrackKat.Tracker.Blobs (at, distance2)
import TrackKat.Tracker.Judge (metres)

-- | Where the head was recentered, and whether the poses tracked and
-- recentered on are the head's.
data Centre = Centre
    { centrePose :: !(Maybe Pose)
    -- ^ the clip's pose at the last recenter
    , centreBelief :: !Branch
    }

-- | Recentered on a pose.
recenterOn :: Pose -> Centre -> Centre
recenterOn pose (Centre _ belief) = Centre (Just pose) (recentered belief)

-- | How clearly a frame shows the reflectors, given their sizes, the
-- frame's blobs, masked or not, and the pose and sights made of them:
-- clearly if each reflector is a blob of its own, with no other blob
-- within its usual diameter, where a piece of it could be.
clarity :: Three Size -> [Blob] -> Pose -> Three (Maybe Sight) -> Clarity
clarity sizes blobs pose sights = case sequenceA sights of
    Just three | and (alone <$> sizes <*> three) -> Clear
    _ -> Doubtful
  where
    alone size s =
        let across = 2 * sqrt (max (sizeUsual size / (metres pose ^ (2 :: Int))) (sightPixels s) / pi)
         in sightKind s == Whole && length [() | b <- blobs, distance2 (sightAt s) (at b) <= across ^ (2 :: Int)] <= 1

-- | What a frame's pose comes to, once it is weighed which pose is the
-- head's.
data Told = Told
    { toldPose :: Pose
    -- ^ the pose tracked, switched for its mirror image if it was that
    , toldCentre :: Centre
    -- ^ the centre, switched likewise, and what is now believed
    , toldCorrection :: Correction
    -- ^ which of them were switched
    , toldEvidence :: Evidence
    -- ^ what the belief went by
    }

-- | Weigh a frame's pose, seen at the given time so clearly, with each
-- reflector seen as given, against what is believed going into it, the
-- head turning about the given point of the clip's axes; and switch for
-- its mirror image whichever of the pose and the centre is surely that.
tellApart :: Intrinsics -> Model -> V3 -> Double -> Clarity -> Pose -> Three (Maybe Sight) -> Centre -> Told
tellApart k m pivot now clear seenPose sights (Centre centre believed) =
    Told pose (Centre reference (corrected fix seenPose watched)) fix (branchEvidence watched)
  where
    others = if all isJust sights then Just (alternatives k m seenPose) else Nothing
    watched = watch now pivot centre clear seenPose others believed
    (pose, fix) = case (correction watched, followedMirror watched) of
        (c, Just other) | c `elem` [MirrorTracked, MirrorBoth] -> (other, c)
        (MirrorReference, _) -> (seenPose, MirrorReference)
        _ -> (seenPose, MirrorNeither)
    reference
        | fix `elem` [MirrorReference, MirrorBoth] = (\old -> fromMaybe old (mirrorPose k m old)) <$> centre
        | otherwise = centre
