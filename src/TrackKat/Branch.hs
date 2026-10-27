-- | Telling the head's pose from its mirror image by how the head moves.
--
-- Three reflectors fit two poses equally well: the clip's triangle tilted
-- toward the camera, and tilted away (see 'mirrorPose'). On the wrong one,
-- turns about axes across the line of sight are reversed: yaw and pitch go
-- the wrong way while the clip's position is right. Single frames can't
-- tell them apart, but movement can. A head turns about its neck, behind
-- and below the clip, so over half a second of turning the true poses keep
-- a point near the learnt pivot still, while the mirror-image poses would
-- have the head turn about a point in front of the clip, or about no point
-- at all.
--
-- See Note [Which pose is the head's].
module TrackKat.Branch (
    Branch,
    newBranch,
    watch,
    foundAfresh,
    recentered,
    Correction (..),
    correction,
    corrected,
    followedMirror,
    settled,
) where

import Data.Maybe (fromMaybe)

import TrackKat.Calibration
import TrackKat.Linear (V3)
import TrackKat.Pose

-- Note [Which pose is the head's]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Two things are in doubt: whether the tracked pose is the head's or its
-- mirror image, and the same of the pose recentered on. The reference
-- matters as much: on a pose taken from the mirror image, the view is off
-- by however far the mirror image is from the head's pose. The belief is a
-- probability for each of the four combinations.
--
-- Each half second of turning, seen with all three reflectors, is evidence
-- about the tracked pose: how badly the tracked poses, and their mirror
-- images, fit turning about a point near the pivot (see 'turnFit'). Their
-- difference, scaled by how much bodies sway and capped so that no single
-- window decides, multiplies the odds.
--
-- Where the two poses meet, with the triangle squarely facing the camera,
-- tracking can come out of the meeting on either one; so entering it makes
-- the tracked pose's truth partly independent of what it was. Finding the
-- clip afresh makes it wholly so. Recentering makes the reference's truth
-- the tracked pose's.
--
-- The clip's triangle has two equal sides, and seen nearly square on such
-- a triangle fits up to four poses, two more coming and going close to the
-- head's as it turns a little. So the mirror image isn't picked afresh
-- each frame but followed from the last, and any pose that fits coming
-- close counts as a meeting. Tracking can wander onto those two for a
-- moment, but they last only while the head is nearly still, and it comes
-- back as they go.
--
-- When the tracked pose is very likely the mirror image, it is switched
-- for its mirror, and the reference too if that is more likely than not
-- the mirror image, so that both change at the same moment; the reference
-- alone is switched only when that is very likely.

-- | Probabilities of the four combinations of the tracked pose and the
-- reference being the head's or the mirror image.
data Belief = Belief
    { bothTrue :: !Double
    , referenceMirrored :: !Double
    , trackedMirrored :: !Double
    , bothMirrored :: !Double
    }

-- | Whether the tracked pose and its mirror image are close.
data Closeness = Apart | Together
    deriving (Eq)

data Branch = Branch
    { brModel :: !Model
    , brBelief :: !Belief
    , brCloseness :: !Closeness
    , brMirror :: !(Maybe Pose)
    -- ^ the tracked pose's mirror image, followed from frame to frame
    , brWindows :: !(Maybe (TurnWindow, TurnWindow))
    -- ^ the tracked poses and their mirror images in the window so far
    }

-- | Nothing known yet about a clip of the given shape.
newBranch :: Model -> Branch
newBranch m = Branch m (Belief 0.25 0.25 0.25 0.25) Apart Nothing Nothing

-- | The tracked pose's mirror image, when all three reflectors were seen
-- in the last frame and the two hadn't met.
followedMirror :: Branch -> Maybe Pose
followedMirror = brMirror

-- | How likely the tracked pose is the mirror image.
trackedMirrorChance :: Belief -> Double
trackedMirrorChance b = trackedMirrored b + bothMirrored b

-- | How likely the reference is the mirror image.
referenceMirrorChance :: Belief -> Double
referenceMirrorChance b = referenceMirrored b + bothMirrored b

-- | Probability needed to act on: to switch to a mirror image, or to
-- learn from the tracked pose.
sure :: Double
sure = 0.99

-- | The least any combination's probability gets, so that a long run of
-- evidence one way doesn't make the other unrecoverable.
leastChance :: Double
leastChance = 1e-3

-- | The chance that tracking comes out of a meeting of the two poses on the
-- other one from the one it went in on.
crossingChance :: Double
crossingChance = 0.3

-- | Degrees between the tracked pose and its mirror image within which the
-- two have met.
togetherDegrees :: Double
togetherDegrees = 8

-- | Spread, in mm, of how well a head's turning fits a fixed point near the
-- pivot, from swaying and from where the pivot is; and how far, in mm, the
-- point may lie from the pivot as cheaply as moving by that spread.
swayMM, pivotSpreadMM :: Double
swayMM = 5
pivotSpreadMM = 80

-- | Degrees a window must turn through to say anything. Less than what
-- learning the pivot needs, since a point in front of the clip shows
-- plainly even in small turns.
windowDegrees :: Double
windowDegrees = 4

-- | The most a window changes the log-odds.
windowWeight :: Double
windowWeight = 3

normalise :: Belief -> Belief
normalise b = Belief (f bothTrue) (f referenceMirrored) (f trackedMirrored) (f bothMirrored)
  where
    floored = Belief (g bothTrue) (g referenceMirrored) (g trackedMirrored) (g bothMirrored)
    g sel = max leastChance (sel b)
    total = bothTrue floored + referenceMirrored floored + trackedMirrored floored + bothMirrored floored
    f sel = sel floored / total

-- | The tracked pose's truth becomes, with the given chance, the opposite
-- of what it was.
crossed :: Double -> Belief -> Belief
crossed chance (Belief tt tm mt mm) =
    Belief (stay tt mt) (stay tm mm) (stay mt tt) (stay mm tm)
  where
    stay same other = (1 - chance) * same + chance * other

-- | The odds for the tracked pose being the mirror image multiplied by
-- e to the given power.
evidence :: Double -> Belief -> Belief
evidence logOdds (Belief tt tm mt mm) = normalise (Belief tt tm (e * mt) (e * mm))
  where
    e = exp logOdds

-- | Take in a tracked pose seen at the given time in seconds, given the
-- pivot in the clip's axes, and the other poses that fit what was seen if
-- all three reflectors were seen (see 'alternatives'). Its mirror image is
-- the one nearest the mirror image in the last frame, or nearest the pose
-- reflected (see 'reflectedPose') if there wasn't one. Where any comes
-- close, tracking could slip onto it.
watch :: Double -> V3 -> Pose -> Maybe [Pose] -> Branch -> Branch
watch now pivot pose seen br = case seen of
    Nothing -> br{brMirror = Nothing, brWindows = Nothing}
    Just others
        | any ((< togetherDegrees) . turnBetweenPoses pose) others || null others ->
            br
                { brBelief = if brCloseness br == Apart then crossed crossingChance (brBelief br) else brBelief br
                , brCloseness = Together
                , brMirror = mirror
                , brWindows = Nothing
                }
        | otherwise -> case (mirror, brWindows br) of
            (Nothing, _) -> br{brMirror = Nothing, brWindows = Nothing}
            (Just other, windows) -> case maybe (startWindow now pose, startWindow now other) (\(a, b) -> (addPose pose a, addPose other b)) windows of
                (a, b)
                    | now - windowStart a >= windowSeconds ->
                        br{brBelief = maybe id evidence (logOdds a b) (brBelief br), brCloseness = Apart, brMirror = mirror, brWindows = Nothing}
                    | otherwise -> br{brCloseness = Apart, brMirror = mirror, brWindows = Just (a, b)}
      where
        mirror = nearestPose (fromMaybe (reflectedPose (brModel br) pose) (brMirror br)) others
  where
    weight = (swayMM / pivotSpreadMM) ^ (2 :: Int)
    logOdds a b
        | windowTurn a < windowDegrees = Nothing
        | otherwise = Just (max (-windowWeight) (min windowWeight ((turnFit weight pivot a - turnFit weight pivot b) / (2 * swayMM * swayMM))))

-- | The clip was found afresh, on either pose as likely.
foundAfresh :: Branch -> Branch
foundAfresh br = br{brBelief = crossed 0.5 (brBelief br), brCloseness = Apart, brMirror = Nothing, brWindows = Nothing}

-- | The tracked pose became the reference.
recentered :: Branch -> Branch
recentered br = br{brBelief = Belief (tt + tm) 0 0 (mt + mm)}
  where
    Belief tt tm mt mm = brBelief br

-- | Which poses to switch for their mirror images.
data Correction = MirrorNeither | MirrorTracked | MirrorReference | MirrorBoth
    deriving (Eq, Show)

correction :: Branch -> Correction
correction br
    | trackedMirrorChance b >= sure = if referenceMirrorChance b > 0.5 then MirrorBoth else MirrorTracked
    | referenceMirrorChance b >= sure = MirrorReference
    | otherwise = MirrorNeither
  where
    b = brBelief br

-- | The poses were switched as given; the given pose is the one the
-- tracked pose was, which is now its mirror image if that was switched.
corrected :: Correction -> Pose -> Branch -> Branch
corrected c was br = case c of
    MirrorNeither -> br
    MirrorReference -> br{brBelief = swapReference (brBelief br)}
    MirrorTracked -> trackedSwitched (swapTracked (brBelief br))
    MirrorBoth -> trackedSwitched (swapTracked (swapReference (brBelief br)))
  where
    swapTracked (Belief tt tm mt mm) = Belief mt mm tt tm
    swapReference (Belief tt tm mt mm) = Belief tm tt mm mt
    trackedSwitched b = br{brBelief = b, brMirror = Just was, brWindows = Nothing}

-- | Whether the tracked pose is surely the head's, so that what it shows
-- can be learnt from.
settled :: Branch -> Bool
settled br = brCloseness br == Apart && trackedMirrorChance (brBelief br) <= 1 - sure
