-- | Turning camera frames into head poses, frame by frame.
--
-- A frame goes through stages, each a function of just what it reads, in
-- a module of its own under @TrackKat.Old.Tracker@: its blobs are sorted by
-- what the still lights learnt mask ("TrackKat.Old.Tracker.Blobs",
-- "TrackKat.Old.Tracker.Still"); the clip is made out in them, by following
-- the last pose or by their layout, whichever is likelier
-- ("TrackKat.Old.Tracker.Follow", "TrackKat.Old.Tracker.Layout",
-- "TrackKat.Old.Tracker.Choose"), each hypothesis judged by the same terms
-- ("TrackKat.Old.Tracker.Judge"); and what the frame showed is taken into
-- what is kept for the next: the poses to follow from
-- ("TrackKat.Old.Tracker.Trail"), the reflectors' sizes
-- ("TrackKat.Old.Tracker.Sizes"), which pose is the head's
-- ("TrackKat.Old.Tracker.Mirror"), the lights, and what learns from poses
-- ("TrackKat.Old.Tracker.Learners"). 'step' puts them together.
module TrackKat.Old.Tracker (
    Tracker,
    newTracker,
    Masking (..),
    trackerCalibration,
    trackerModel,
    trackerShape,
    trackerCentre,
    trackerPose,
    BlobRole (..),
    trackerBlobs,
    trackerLights,
    trackerWindows,
    Request (..),
    request,
    setModel,
    setMasking,
    Event (..),
    Outcome (..),
    Seen (..),
    Why (..),
    Looking (..),
    Reason (..),
    Way (..),
    Chosen (..),
    Tried (..),
    Fate (..),
    explainChosen,
    Held (..),
    explainHeld,
    whyRejectedBy,
    sameWhy,
    describeWhy,
    explainWhy,
    Absence,
    noAbsence,
    absence,
    Learning (..),
    NotTheClip (..),
    Moving (..),
    Evidence (..),
    describeEvidence,
    describeSeen,
    describeMasked,
    describeLightIgnored,
    describeLightForgotten,
    describeLightMoved,
    findBlobs,
    step,
) where

import Data.Foldable (toList)
import Data.List (mapAccumL)
import Data.Maybe (fromMaybe)

import TrackKat.Branch
import TrackKat.Calibration
import TrackKat.Frame (Blob, Stripe)
import TrackKat.Lights (Learning (..), Masking (..), Moving (..), NotTheClip (..), lightAreas, noLights)
import TrackKat.Linear (norm)
import TrackKat.Old.Match
import TrackKat.Old.Tracker.Absence
import TrackKat.Old.Tracker.Blobs
import TrackKat.Old.Tracker.Choose
import TrackKat.Old.Tracker.Judge
import TrackKat.Old.Tracker.Layout (findAfresh)
import TrackKat.Old.Tracker.Learners
import TrackKat.Old.Tracker.Mirror
import TrackKat.Old.Tracker.Outcome
import TrackKat.Old.Tracker.Sighting
import TrackKat.Old.Tracker.Sizes
import TrackKat.Old.Tracker.Still
import TrackKat.Old.Tracker.Trail
import TrackKat.Pose
import TrackKat.Shape
import TrackKat.Term
import TrackKat.World (Instant, seconds, (.-.))

-- Note [The old tracker]
-- ~~~~~~~~~~~~~~~~~~~~~~
-- This module and those under it, with "TrackKat.Old.Match" and
-- "TrackKat.Old.World", are the tracker as it was before every hypothesis
-- about a frame was scored by one account ("TrackKat.Tracker"), kept
-- beside that one so that a user can go back to it by a setting (see
-- "TrackKat.Trackers") while the new one proves itself. It is frozen:
-- nothing here is to be improved, only deleted, with "Test.Old" and the
-- setting, once the new tracker has taken its place.
--
-- It is the code as it was but for its modules' names, and for taking
-- from elsewhere what moved: how a reflector was seen, from
-- "TrackKat.Sight", and two of the world's figures, from
-- "TrackKat.Old.World". What both trackers share (the lights, the
-- learners' arithmetic, poses, terms) is as the old tracker had it, or
-- only added to. That it tracks as it did is held by its simulations'
-- figures and its scorecard ("Test.Old.Scorecard").

-- | Something the user asked for, carried out on the next tracked frame.
data Request = Recenter | Calibrate
    deriving (Eq, Show)

-- | What is kept from one frame to the next.
data Tracker = Tracker
    { trCamera :: !Intrinsics
    , trClip :: !Model
    , trTrail :: !Trail
    , trSizes :: !(Three Size)
    , trCentre :: !Centre
    , trLearners :: !Learners
    , trStill :: !Still
    , trPending :: ![Request]
    -- ^ oldest first
    , trBlobs :: ![(Blob, BlobRole)]
    -- ^ the last frame's, all evaluated, so as not to hold on to the frames
    -- before
    }

-- | A tracker that starts from the given calibration and recenters on the
-- first frame it tracks.
newTracker :: Intrinsics -> Model -> Masking -> Calibration -> Tracker
newTracker k m masking c = Tracker k m noTrail (pure noSize) (Centre Nothing (newBranch m)) (newLearners m c) (Still masking noLights) [Recenter] []

-- | What the tracker has learnt so far.
trackerCalibration :: Tracker -> Calibration
trackerCalibration = learnerCalibration . learnersPivot . trLearners

-- | The clip tracked.
trackerModel :: Tracker -> Model
trackerModel = trClip

-- | What has been learnt of the clip's shape.
trackerShape :: Tracker -> ShapeLearner
trackerShape = learnersShape . trLearners

-- | The clip's pose at the last recenter.
trackerCentre :: Tracker -> Maybe Pose
trackerCentre = centrePose . trCentre

-- | The clip's pose in the camera's axes, as last tracked.
trackerPose :: Tracker -> Maybe Pose
trackerPose = fmap recentPose . trailRecent . trTrail

-- | The last frame's blobs, heaviest first, and what each was taken for.
trackerBlobs :: Tracker -> [(Blob, BlobRole)]
trackerBlobs = trBlobs

-- | The still lights learnt, each as the centre and radius, in pixels, of
-- the area masked around it.
trackerLights :: Tracker -> [((Double, Double), Double)]
trackerLights = lightAreas . stillLights . trStill

-- | The windows of turning weighed so far by what learns from them: for
-- the neck pivot, the clip's shape, and telling the mirror image, how
-- many were taken and how many left out, by what rejected them.
trackerWindows :: Tracker -> [(String, Tally)]
trackerWindows t =
    [ ("pivot", learnerWindows (learnersPivot (trLearners t)))
    , ("clip shape", shapeWindows (learnersShape (trLearners t)))
    , ("mirror image", branchWindows (centreBelief (trCentre t)))
    ]

-- | Learn and mask still lights, or not, from the next frame; turned off,
-- what was learnt is forgotten.
setMasking :: Masking -> Tracker -> Tracker
setMasking masking t
    | masking == stillMasking (trStill t) = t
    | otherwise = t{trStill = Still masking noLights}

-- | Ask for something, after anything already asked for.
request :: Request -> Tracker -> Tracker
request r t = t{trPending = trPending t ++ [r]}

-- | Track a differently shaped clip from the next frame, recentering then,
-- since the model's axes move with its shape.
setModel :: Model -> Tracker -> Tracker
setModel m t = request Recenter t{trClip = m, trTrail = noTrail, trLearners = changeClipOf m (trLearners t), trCentre = (trCentre t){centreBelief = newBranch m}}

-- | A frame as it is looked at: what the stages that take it into what is
-- kept are given of it.
data Look
    = Look
        -- | when it was seen
        Instant
        -- | its blobs, heaviest first
        [Blob]
        -- | how the lights learnt sort them
        Shown
        -- | the poses that still predict where the clip is
        Trail
        -- | what the blobs are judged against
        Known

-- | Handle one frame's stripes, seen at the given time. Pending requests
-- wait for a frame where the clip is tracked.
step :: Instant -> [Stripe] -> Tracker -> (Tracker, Outcome, [Event])
step now stripes t = case makeOut known now from shown of
    Left (why, held) -> unseenFrame look why held t
    Right (chosen, sighting) -> seenFrame look chosen sighting t
  where
    look = Look now blobs shown from known
    blobs = findBlobs stripes
    from = stillPredicting now (trTrail t)
    -- Blobs in a still light's area are ignored, but for any where the
    -- clip is predicted to be.
    shown = sortBlobs (maskFor (trStill t) (maybe [] (reflectorsAt known . recentPose) (trailRecent from))) blobs
    known =
        Known
            { knownCamera = trCamera t
            , knownClip = trClip t
            , knownReach = Reach (appliedPivot (learnersPivot (trLearners t))) (centrePose (trCentre t))
            , knownSizes = trSizes t
            , knownLast = recentPose <$> trailRecent (trTrail t)
            }

-- | Where each reflector is, or would be, in the image at a pose.
reflectorsAt :: Known -> Pose -> [(Double, Double)]
reflectorsAt known pose = toList (seenAt (knownCamera known) (knownClip known) pose <$> reflectors)

-- | What is kept, and told, of a frame the clip wasn't made out in, for
-- the reason given, with any pose passed over.
unseenFrame :: Look -> Why -> Maybe Held -> Tracker -> (Tracker, Outcome, [Event])
unseenFrame (Look now blobs shown from known) why held t =
    ( t
        { trLearners = lostSightOf (trLearners t)
        , trStill = (trStill t){stillLights = lights}
        , trTrail = Trail untracked (trailLeft from)
        , trBlobs = roles (shownMask shown) [] blobs
        }
    , Lost (length (shownVisible shown)) (length (shownMasked shown)) why held
    , news
    )
  where
    (lights, news) = learnUnseen now blobs clipIn (trStill t)
    clipIn mask = either (const []) (map at . snd) (findAfresh known (sortBlobs mask blobs))
    -- Something tracked a moment ago in a light just learnt was the
    -- light, and isn't followed.
    untracked = case trailRecent (trTrail t) of
        Just r | not (inNewLight news (reflectorsAt known (recentPose r)) lights) -> Just r{recentSince = now}
        _ -> Nothing

-- | What is kept, and told, of a frame the clip was made out in, in the
-- way and as the sighting given.
seenFrame :: Look -> Chosen -> Sighting -> Tracker -> (Tracker, Outcome, [Event])
seenFrame (Look now blobs shown from known) chosen@(Chosen way _) (Sighting seenPose fit sights unused hidden judged) t =
    ( t
        { trTrail = Trail (if wasLight then Nothing else Just recent) left
        , trSizes = sizes
        , trCentre = asked
        , trLearners = learners
        , trStill = (trStill t){stillLights = lights}
        , trPending = []
        , trBlobs = roles (shownMask shown) [s | Just s <- toList sights] blobs
        }
    , Tracked (relativePose (appliedLevel applied) (appliedPivot applied) ref pose) (norm (poseT pose)) fit seen judged chosen
    , [Corrected evidence | correcting]
        ++ requested
        ++ [Refined r (learnerCalibration (learnersPivot learnt)) | r <- refinements]
        ++ [ShapeEstimated est | Just est <- [shapeNews]]
        ++ calibrated
        ++ news
    )
  where
    k = knownCamera known
    m = knownClip known
    pivot = reachPivot (knownReach known)
    lastTracked = trailRecent (trTrail t)
    Carried believed since leaving = carryOn way now (centreBelief (trCentre t)) from
    -- If what was tracked for a moment was a light, and is learnt as
    -- one, it is no longer followed.
    confirmed = standing now since
    (lights, news) = learnSeen now (clipView k m confirmed seenPose sights (biggestAt sizes (centrePose (trCentre t)))) blobs (trStill t)
    wasLight = confirmed == Unconfirmed && inNewLight news (reflectorsAt known seenPose) lights
    clear = clarity (trSizes t) blobs seenPose sights
    Told pose centre fix evidence = tellApart k m pivot now clear seenPose sights (Centre (centrePose (trCentre t)) believed)
    correcting = fix /= MirrorNeither
    recent = Recent pose now (if showing (trSizes t) (metres seenPose) sights == AllShowing then now else maybe now recentAllSeen lastTracked) since
    sizes = resized (maybe (seconds 0) (\r -> now .-. recentTime r) lastTracked) (metres pose) <$> trSizes t <*> sights
    ((asked, restarted), requested) = mapAccumL (carryOut now pose) (centre, if correcting then startOver (trLearners t) else trLearners t) (trPending t)
    ref = fromMaybe pose (centrePose asked)
    -- What was believed of a pose left behind was believed of the centre
    -- too, so it isn't kept past a correction or a recentering.
    left
        | correcting || Recenter `elem` trPending t = Nothing
        | otherwise = leaving
    -- Only a pose surely the head's, its reflectors seen clearly, is
    -- learnt from.
    (learnt, refinements, shapeNews) = case clear of
        Clear | settled (centreBelief asked) -> learnFrom k now pivot pose restarted
        _ -> (lostSightOf restarted, [], Nothing)
    (learners, calibrated) = sweepOn now pose ref learnt
    applied = learnersPivot learners
    seen =
        Seen
            [which | (which, Nothing) <- toList ((,) <$> reflectors <*> sights)]
            (length [() | Just s <- toList sights, sightKind s == Merged])
            unused
            hidden

-- | Carry out a request at the given time, on a frame tracked at the
-- given pose: what it changes, and the event that says so.
carryOut :: Instant -> Pose -> (Centre, Learners) -> Request -> ((Centre, Learners), Event)
carryOut _ pose (centre, learners) Recenter = ((recenterOn pose centre, learners), Recentered)
carryOut now _ (centre, learners) Calibrate = ((centre, startSweep now learners), CalibrationStarted)
