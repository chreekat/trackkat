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
    Clarity (..),
    watch,
    foundAfresh,
    recentered,
    Correction (..),
    correction,
    corrected,
    followedMirror,
    settled,
    Evidence (..),
    branchEvidence,
    describeEvidence,
    branchWindows,
) where

import Data.Maybe (fromMaybe)
import Text.Printf (printf)

import TrackKat.Calibration
import TrackKat.Linear (V3)
import TrackKat.Pose
import TrackKat.Term (Bound (..), Measure (..), Name (..), Score (..), Tally, Term, accepts, gate, gaussian, likelihood, noTally, score, tally, verdict)
import TrackKat.World (World (..), inDegrees, inMillimetres, times, world)

-- Note [Which pose is the head's]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Two things are in doubt: whether the tracked pose is the head's or its
-- mirror image, and the same of the pose recentered on. The reference
-- matters as much: on a pose taken from the mirror image, the view is off
-- by however far the mirror image is from the head's pose. The belief is a
-- probability for each of the four combinations.
--
-- Each half second of turning, seen clearly with all three reflectors, is
-- evidence about the tracked pose: how badly the tracked poses, and their
-- mirror images, fit turning about a point near the pivot (see
-- 'turnFit'). Each fit is taken as that much less likely, against how
-- much bodies sway; but a window that neither fits, of a head moving
-- otherwise than by turning about its neck or of reflectors not where
-- they were seen, is as likely either way. So the odds are multiplied by
-- how much likelier the window is on one than the other, out of a little
-- that either has of being such a window, which also keeps any one window
-- from deciding.
--
-- Windows that end in about the same pose count together for no more than
-- one. However a head turns there, and however its reflectors show there
-- (each is tape on a curved tab, its centre where the tape faces the
-- camera), may fit one pose better throughout; so many windows of
-- fidgeting while looking one way are no more evidence than one, and
-- what adds up comes from turning through different poses.
--
-- Only clearly seen reflectors say how the head moved. One that runs into
-- another, or that breaks into pieces, or that has something else beside
-- it, as reflectors seen at a slant do, has its centre out by a few
-- pixels, which is the size of the difference between the two poses'
-- movement; and the pose tracked from it can be neither.
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
-- What to act on also weighs where the head would now be. A head stays
-- within reach of where it was centred, mostly well within; while on the
-- mirror image of either pose but not the other, it would have turned
-- however far the tracked pose's mirror image is from the reference, which
-- is often further than heads turn. (On both, or neither, it has turned
-- as far as the tracked pose is from the reference, since the two mirror
-- images are turned alike.) This isn't evidence that adds up, being only
-- where the head is now, so it isn't kept in the belief.
--
-- When the tracked pose is very likely the mirror image, it is switched
-- for its mirror, and the reference too if, the tracked pose being the
-- mirror image, the evidence has the reference more likely than not the
-- mirror image too, so that both change at the same moment; where the
-- head would be says little about that, with the head turned alike either
-- way. The reference alone is switched only when that is very likely.
--
-- The evidence is in nats throughout, and its parts are terms (see
-- "TrackKat.Term"): how well a window fits turning about the pivot
-- ('turning'), which windows say anything ('telling'), and where a head
-- would be ('reaching'). A correction says what it was made on
-- ('Evidence'), and windows are counted by whether they said anything
-- ('branchWindows').

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

-- | How the reflectors were seen in a frame: each as a blob of its own,
-- with nothing else near enough to be a piece of it; or otherwise.
data Clarity = Clear | Doubtful
    deriving (Eq, Show)

-- | Degrees the head has turned from the reference: on the tracked pose
-- and the reference both the head's or both the mirror image (alike), and
-- on just one of them (unalike).
data Turned
    = Turned
        -- | alike
        !Double
        -- | unalike
        !Double

data Branch = Branch
    { brModel :: !Model
    , brBelief :: !Belief
    , brCloseness :: !Closeness
    , brMirror :: !(Maybe Pose)
    -- ^ the tracked pose's mirror image, followed from frame to frame
    , brWindows :: !(Maybe (TurnWindow, TurnWindow))
    -- ^ the tracked poses and their mirror images in the window so far
    , brTurned :: !(Maybe Turned)
    -- ^ in the last frame, if there was a reference and a mirror image
    , brPlaces :: ![Place]
    -- ^ where the windows that counted lately ended, the latest first, all
    -- evaluated
    , brTally :: !Tally
    -- ^ windows that said something, and those that didn't, by why
    }

-- | About where windows of turning ended, and how much, together, they
-- changed the log-odds.
data Place = Place !Pose !Double

-- | Nothing known yet about a clip of the given shape.
newBranch :: Model -> Branch
newBranch m = Branch m (Belief 0.25 0.25 0.25 0.25) Apart Nothing Nothing Nothing [] noTally

-- | How many windows of turning have been weighed since the clip was
-- last set, and how many said nothing, by the term that says so.
branchWindows :: Branch -> Tally
branchWindows = brTally

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
--
-- The spread is the world's 'bodySway'. The point's allowance is twice the
-- world's 'pivotSpread': the pivot applied is itself an estimate, as far
-- again from where it is.
swayMM, pivotSpreadMM :: Double
swayMM = inMillimetres (bodySway world)
pivotSpreadMM = inMillimetres (times 2 (pivotSpread world))

-- | Degrees a window must turn through to say anything. Less than what
-- learning the pivot needs, since a point in front of the clip shows
-- plainly even in small turns.
windowDegrees :: Double
windowDegrees = 4

-- | The most a window changes the log-odds: the log of how much likelier a
-- window that fits turning about the pivot is than one that fits nothing.
windowWeight :: Double
windowWeight = 3

-- | Degrees within which windows of turning end in about the same place,
-- where they have their errors alike; and how many of the places windows
-- lately ended in are remembered.
placeDegrees :: Double
placeDegrees = 15

placesRemembered :: Int
placesRemembered = 4

-- | Spread, in degrees, of how far a head turns from where it was
-- centred: most turns are within this, and few beyond twice it. The
-- world's 'headTurnSpread'.
headReachDegrees :: Double
headReachDegrees = inDegrees (headTurnSpread world)

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
-- pivot in the model's axes, the reference if there is one, how clearly
-- the reflectors were seen, and the other poses that fit what was seen if
-- all three reflectors were seen (see 'alternatives'). Its mirror image is
-- the one nearest the mirror image in the last frame, or nearest the pose
-- reflected (see 'reflectedPose') if there wasn't one. Where any comes
-- close, tracking could slip onto it.
watch :: Double -> V3 -> Maybe Pose -> Clarity -> Pose -> Maybe [Pose] -> Branch -> Branch
watch now pivot reference clarity pose seen br = case seen of
    Nothing -> br{brMirror = Nothing, brWindows = Nothing, brTurned = Nothing}
    Just others
        | any ((< togetherDegrees) . turnBetweenPoses pose) others || null others ->
            br
                { brBelief = if brCloseness br == Apart then crossed crossingChance (brBelief br) else brBelief br
                , brCloseness = Together
                , brMirror = mirror
                , brWindows = Nothing
                , brTurned = turned
                }
        | otherwise -> case (mirror, brWindows br, clarity) of
            (Nothing, _, _) -> br{brMirror = Nothing, brWindows = Nothing, brTurned = Nothing}
            (Just _, _, Doubtful) -> br{brCloseness = Apart, brMirror = mirror, brWindows = Nothing, brTurned = turned}
            (Just other, windows, Clear) -> case maybe (startWindow now pose, startWindow now other) (\(a, b) -> (addPose pose a, addPose other b)) windows of
                (a, b)
                    | now - windowStart a >= windowSeconds ->
                        let (belief, places) = maybe (brBelief br, brPlaces br) counted (logOdds a b)
                         in br{brBelief = belief, brCloseness = Apart, brMirror = mirror, brWindows = Nothing, brTurned = turned, brPlaces = places, brTally = tally (verdict telling a) (brTally br)}
                    | otherwise -> br{brCloseness = Apart, brMirror = mirror, brWindows = Just (a, b), brTurned = turned}
      where
        mirror = nearestPose (fromMaybe (reflectedPose (brModel br) pose) (brMirror br)) others
        turned = (\ref other -> Turned (turnBetweenPoses ref pose) (turnBetweenPoses ref other)) <$> reference <*> mirror
        -- A window's log-odds, counted only as far as those of the windows
        -- that ended near the same pose come to at most one window's worth.
        counted odds =
            let (near, further) = break (\(Place there _) -> turnBetweenPoses there pose < placeDegrees) (brPlaces br)
                (Place at before, rest) = case further of
                    p : later -> (p, near ++ later)
                    [] -> (Place pose 0, near)
                after = max (negate windowWeight) (min windowWeight (before + odds))
                places = Place at after : take (placesRemembered - 1) rest
             in (evidence (after - before) (brBelief br), foldr seq places places)
  where
    weight = (swayMM / pivotSpreadMM) ^ (2 :: Int)
    -- The log of a window's likelihood on its poses being the head's, given
    -- how badly they fit turning about a point near the pivot: that of a
    -- window that fits, or of one that fits nothing.
    turning :: Term TurnWindow
    turning = likelihood (Name "turning" "the head didn't turn about the neck") $ \w ->
        logAddExp (negate (turnFit weight pivot w / (2 * swayMM * swayMM))) (negate windowWeight)
    -- How much likelier, in nats, a window is on the mirror image's poses
    -- than on the tracked ones, if the head turned enough to tell.
    logOdds a b
        | accepts telling a = Just (nats (score turning b) - nats (score turning a))
        | otherwise = Nothing

-- | Whether a window of tracked poses turned enough to say which pose is
-- the head's.
telling :: Term TurnWindow
telling =
    gate
        (Name "turn" "the head turned too little")
        (\w -> not (windowTurn w < windowDegrees))
        (\w -> Measure (windowTurn w) AtLeast windowDegrees "degrees")

-- | How likely a head is to be turned so many degrees from where it was
-- centred, against not turned at all.
reaching :: Term Double
reaching = gaussian (Name "reach" "turned further than heads mostly turn") "degrees" headReachDegrees id

-- | log (exp a + exp b), without overflow.
logAddExp :: Double -> Double -> Double
logAddExp a b = max a b + log (1 + exp (negate (abs (a - b))))

-- | The clip was found afresh, on either pose as likely.
foundAfresh :: Branch -> Branch
foundAfresh br = br{brBelief = crossed 0.5 (brBelief br), brCloseness = Apart, brMirror = Nothing, brWindows = Nothing, brTurned = Nothing, brPlaces = []}

-- | The tracked pose became the reference.
recentered :: Branch -> Branch
recentered br = br{brBelief = Belief (tt + tm) 0 0 (mt + mm), brTurned = Nothing}
  where
    Belief tt tm mt mm = brBelief br

-- | The belief, weighed by how far the head would have turned from the
-- reference on each combination.
judged :: Branch -> Belief
judged br = case brTurned br of
    Nothing -> b
    Just (Turned alike unalike) ->
        let near = reach alike
            far = reach unalike
            total = near * (bothTrue b + bothMirrored b) + far * (referenceMirrored b + trackedMirrored b)
         in Belief (near * bothTrue b / total) (far * referenceMirrored b / total) (far * trackedMirrored b / total) (near * bothMirrored b / total)
  where
    b = brBelief br
    reach = exp . nats . score reaching

-- | Which poses to switch for their mirror images.
data Correction = MirrorNeither | MirrorTracked | MirrorReference | MirrorBoth
    deriving (Eq, Show)

correction :: Branch -> Correction
correction br
    | trackedMirrorChance b >= sure = if bothMirrored (brBelief br) > trackedMirrored (brBelief br) then MirrorBoth else MirrorTracked
    | referenceMirrorChance b >= sure = MirrorReference
    | otherwise = MirrorNeither
  where
    b = judged br

-- | The poses were switched as given; the given pose is the one the
-- tracked pose was, which is now its mirror image if that was switched.
corrected :: Correction -> Pose -> Branch -> Branch
corrected c was br = case c of
    MirrorNeither -> br
    MirrorReference -> br{brBelief = swapReference (brBelief br), brTurned = swapTurned <$> brTurned br}
    MirrorTracked -> trackedSwitched (swapTracked (brBelief br)) (swapTurned <$> brTurned br)
    MirrorBoth -> trackedSwitched (swapTracked (swapReference (brBelief br))) (brTurned br)
  where
    swapTracked (Belief tt tm mt mm) = Belief mt mm tt tm
    swapReference (Belief tt tm mt mm) = Belief tm tt mm mt
    -- Switching either pose for its mirror image swaps which combinations
    -- have the head turned alike.
    swapTurned (Turned alike unalike) = Turned unalike alike
    trackedSwitched b turned = br{brBelief = b, brMirror = Just was, brWindows = Nothing, brTurned = turned, brPlaces = []}

-- | What a correction is made on: which poses are switched, how likely the
-- tracked pose and the reference each are the mirror image, weighed by
-- where the head would be, and how many degrees the head would be turned
-- from the reference with the poses as they are and with one of them
-- switched, if that is known.
data Evidence = Evidence
    { evCorrection :: Correction
    , evTrackedMirrored :: Double
    , evReferenceMirrored :: Double
    , evTurned :: Maybe (Double, Double)
    }
    deriving (Eq, Show)

-- | What 'correction' goes on.
branchEvidence :: Branch -> Evidence
branchEvidence br =
    Evidence
        (correction br)
        (trackedMirrorChance b)
        (referenceMirrorChance b)
        ((\(Turned alike unalike) -> (alike, unalike)) <$> brTurned br)
  where
    b = judged br

-- | The evidence for a correction, for a developer.
describeEvidence :: Evidence -> String
describeEvidence (Evidence c tracked reference turned) =
    printf
        "%s: the tracked pose %.1f%% likely the mirror image, the centre %.1f%%%s"
        ( case c of
            MirrorNeither -> "nothing switched" :: String
            MirrorTracked -> "switched the tracked pose"
            MirrorReference -> "switched the centre"
            MirrorBoth -> "switched the tracked pose and the centre"
        )
        (100 * tracked)
        (100 * reference)
        (maybe "" (\(alike, unalike) -> printf "; the head turned %.0f degrees from centre as it was, %.0f with one switched" alike unalike :: String) turned)

-- | Whether the tracked pose is surely the head's, so that what it shows
-- can be learnt from.
settled :: Branch -> Bool
settled br = brCloseness br == Apart && trackedMirrorChance (judged br) <= 1 - sure
