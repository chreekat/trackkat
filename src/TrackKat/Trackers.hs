-- | The two trackers behind one: whichever a setting asks for, with what
-- it says of a frame in one shape, for what goes on to shape the view,
-- show it, and record it.
--
-- See Note [Two trackers].
module TrackKat.Trackers (
    -- * Which tracker
    WhichTracker (..),
    Tracker,
    newTracker,
    whichTracker,
    takeOver,

    -- * What it tracks by
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

    -- * A frame
    step,
    Event (..),
    Outcome (..),
    Seen (..),
    How (..),
    explainHow,
    explainUnusual,
    Why (..),
    sameWhy,
    describeWhy,
    explainWhy,
    Absence,
    noAbsence,
    absence,

    -- * In words
    Evidence (..),
    describeEvidence,
    describeSeen,
    describeMasked,
    describeLightIgnored,
    describeLightForgotten,
    describeLightMoved,
) where

import Data.Maybe (fromMaybe)

import TrackKat.Branch (Evidence (..), describeEvidence)
import TrackKat.Calibration (Calibration)
import TrackKat.Config (WhichTracker (..))
import TrackKat.Frame (Blob, Stripe)
import TrackKat.Lights (Masking (..))
import qualified TrackKat.Old.Tracker as Old
import TrackKat.Pose (HeadPose, Intrinsics, Model, Pose)
import TrackKat.Shape (ShapeLearner)
import TrackKat.Term (Finding, Tally, renderFindings)
import TrackKat.Tracker (
    BlobRole (..),
    Event (..),
    Request (..),
    Seen (..),
    describeLightForgotten,
    describeLightIgnored,
    describeLightMoved,
    describeMasked,
    describeSeen,
 )
import qualified TrackKat.Tracker as New
import TrackKat.World (Instant, Seconds, seconds, (.-.))

-- Note [Two trackers]
-- ~~~~~~~~~~~~~~~~~~~
-- trackkat has two trackers while the new one ("TrackKat.Tracker") proves
-- itself on real cameras: users get only what is built from main, so the
-- way to try it, and to go back when it is worse, is a setting. The old
-- one is kept as it was (see Note [The old tracker] in
-- "TrackKat.Old.Tracker").
--
-- A 'Tracker' here is one or the other, never both: the one not asked for
-- isn't made, kept, or run. What the two say of a frame differs only in
-- how they explain themselves, so an 'Outcome' is the same of both but
-- for 'How' a pose was come by and 'Why' the clip was lost, each the
-- tracker's own, to be put into words by what here takes either. What
-- else the trackers tell (events, what was seen of the reflectors, what
-- each blob was taken for) is the same of both, and is told as the new
-- tracker's types have it.
--
-- One tracker takes over from the other ('takeOver') with what the user
-- would miss: the calibration learnt, where the view is centered, and
-- what is still asked for. The rest starts afresh, as on a tracker newly
-- made: the still lights learnt, which pose is the head's, the windows of
-- turning under way, and a calibration of level in progress.
--
-- When the new tracker has taken the old one's place, this module goes
-- with the old one, and what uses it uses "TrackKat.Tracker".

-- | What is kept from one frame to the next, by one tracker or the other.
data Tracker = Old !Old.Tracker | New !New.Tracker

-- | A tracker of the kind asked for, that starts from the given
-- calibration and recenters on the first frame it tracks.
newTracker :: WhichTracker -> Intrinsics -> Model -> Masking -> Calibration -> Tracker
newTracker OldTracker k m masking c = Old (Old.newTracker k m masking c)
newTracker NewTracker k m masking c = New (New.newTracker k m masking c)

-- | Which tracker this is.
whichTracker :: Tracker -> WhichTracker
whichTracker (Old _) = OldTracker
whichTracker (New _) = NewTracker

-- | A tracker newly made, taking over from one that tracked the same clip:
-- centered where that one was, and asked for what that one was still
-- asked for, a recentering on the clip's new shape among it. See Note
-- [Two trackers].
takeOver :: Tracker -> Tracker -> Tracker
takeOver from fresh = case fresh of
    Old t -> Old (Old.takingOver (trackerCentre from) (map oldRequest (trackerPending from)) t)
    New t -> New (New.takingOver (trackerCentre from) (trackerPending from) t)

-- | What has been asked for and not yet carried out, oldest first.
trackerPending :: Tracker -> [Request]
trackerPending (Old t) = map newRequest (Old.trackerPending t)
trackerPending (New t) = New.trackerPending t

-- | What the tracker has learnt so far.
trackerCalibration :: Tracker -> Calibration
trackerCalibration (Old t) = Old.trackerCalibration t
trackerCalibration (New t) = New.trackerCalibration t

-- | The clip tracked.
trackerModel :: Tracker -> Model
trackerModel (Old t) = Old.trackerModel t
trackerModel (New t) = New.trackerModel t

-- | What has been learnt of the clip's shape.
trackerShape :: Tracker -> ShapeLearner
trackerShape (Old t) = Old.trackerShape t
trackerShape (New t) = New.trackerShape t

-- | The clip's pose at the last recenter.
trackerCentre :: Tracker -> Maybe Pose
trackerCentre (Old t) = Old.trackerCentre t
trackerCentre (New t) = New.trackerCentre t

-- | The clip's pose in the camera's axes, as last tracked.
trackerPose :: Tracker -> Maybe Pose
trackerPose (Old t) = Old.trackerPose t
trackerPose (New t) = New.trackerPose t

-- | The last frame's blobs, heaviest first, and what each was taken for.
trackerBlobs :: Tracker -> [(Blob, BlobRole)]
trackerBlobs (Old t) = [(b, newRole role) | (b, role) <- Old.trackerBlobs t]
trackerBlobs (New t) = New.trackerBlobs t

-- | The still lights learnt, each as the centre and radius, in pixels, of
-- the area masked around it.
trackerLights :: Tracker -> [((Double, Double), Double)]
trackerLights (Old t) = Old.trackerLights t
trackerLights (New t) = New.trackerLights t

-- | The windows of turning weighed so far by what learns from them: for
-- the neck pivot, the clip's shape, and telling the mirror image, how
-- many were taken and how many left out, by what rejected them.
trackerWindows :: Tracker -> [(String, Tally)]
trackerWindows (Old t) = Old.trackerWindows t
trackerWindows (New t) = New.trackerWindows t

-- | Learn and mask still lights, or not, from the next frame; turned off,
-- what was learnt is forgotten.
setMasking :: Masking -> Tracker -> Tracker
setMasking masking (Old t) = Old (Old.setMasking masking t)
setMasking masking (New t) = New (New.setMasking masking t)

-- | Ask for something, after anything already asked for.
request :: Request -> Tracker -> Tracker
request r (Old t) = Old (Old.request (oldRequest r) t)
request r (New t) = New (New.request r t)

-- | Track a differently shaped clip from the next frame, recentering then.
setModel :: Model -> Tracker -> Tracker
setModel m (Old t) = Old (Old.setModel m t)
setModel m (New t) = New (New.setModel m t)

-- | Handle one frame's stripes, seen at the given time. Pending requests
-- wait for a frame where the clip is tracked.
step :: Instant -> [Stripe] -> Tracker -> (Tracker, Outcome, [Event])
step now stripes (Old t) = let (t', o, events) = Old.step now stripes t in (Old t', oldOutcome o, map newEvent events)
step now stripes (New t) = let (t', o, events) = New.step now stripes t in (New t', newOutcome o, events)

-- | What a frame showed.
data Outcome
    = Tracked
        { outPose :: HeadPose
        , outDistance :: Double
        -- ^ mm from the camera to the clip
        , outFit :: Double
        -- ^ RMS reprojection error, pixels
        , outSeen :: Seen
        , outHow :: How
        -- ^ worked out only if looked at
        }
    | -- | The clip couldn't be made out.
      Lost
        { outBlobs :: Int
        -- ^ blobs there were, besides those masked
        , outMasked :: Int
        -- ^ blobs ignored as part of a still light
        , outWhy :: Why
        -- ^ worked out only if looked at
        }
    deriving (Show)

-- | How a tracked frame's pose was come by, as its tracker tells it: what
-- each term that judged the pose found, and which of the ways tried, or
-- of the hypotheses scored, gave it.
data How
    = OldHow [Finding] Old.Chosen
    | NewHow [Finding] New.Chosen
    deriving (Show)

-- | Why the clip wasn't made out in a frame, as its tracker tells it: the
-- old one with any pose it passed over, the new one with every hypothesis
-- about the frame, best scored first.
data Why
    = OldWhy Old.Why (Maybe Old.Held)
    | NewWhy New.Why [New.Rival]
    deriving (Show)

-- | What a tracked frame's pose was judged by, and how it was come by, for
-- a developer.
explainHow :: How -> String
explainHow (OldHow judged chosen) = renderFindings judged ++ maybe "" ("   " ++) (Old.explainChosen chosen)
explainHow (NewHow judged chosen) = New.explainChosen chosen ++ ": " ++ renderFindings judged

-- | How a tracked frame's pose was come by, for a developer, if otherwise
-- than tracking mostly goes: by the old tracker, otherwise than by
-- following the last pose; by the new, otherwise than by continuing the
-- last pose where it predicted the clip's reflectors.
explainUnusual :: How -> Maybe String
explainUnusual (OldHow _ chosen@(Old.Chosen way _))
    | way /= Old.Followed = Old.explainChosen chosen
explainUnusual (NewHow _ chosen@(New.Chosen way source _ _))
    | way /= New.Continued || source /= New.ByPrediction = Just (New.explainChosen chosen)
explainUnusual _ = Nothing

-- | Whether two frames' reasons are the same but for their figures.
sameWhy :: Why -> Why -> Bool
sameWhy (OldWhy a _) (OldWhy b _) = Old.sameWhy a b
sameWhy (NewWhy a _) (NewWhy b _) = New.sameWhy a b
sameWhy _ _ = False

-- | Why the clip is out of sight, in a user's words: a clause to follow
-- "out of sight: ".
describeWhy :: Why -> String
describeWhy (OldWhy why _) = Old.describeWhy why
describeWhy (NewWhy why _) = New.describeWhy why

-- | Why the clip wasn't made out, for a developer, with what else the
-- tracker made of the frame.
explainWhy :: Why -> String
explainWhy (OldWhy why held) = Old.explainWhy why ++ maybe "" (("; " ++) . Old.explainHeld) held
explainWhy (NewWhy why rivals) = New.explainWhy why ++ New.explainRivals (take 3 rivals)

-- | Since when the clip has been out of sight in every frame, if it is,
-- and when a user was last told why, with the reason given.
data Absence = Absence !(Maybe Instant) !(Maybe (Instant, Why))

-- | The clip in sight, and nothing told.
noAbsence :: Absence
noAbsence = Absence Nothing Nothing

-- | How long the clip is out of sight, with lights enough in view to be
-- it, before a user is told why; and the least between two tellings.
absenceSeconds, absenceQuiet :: Seconds
absenceSeconds = seconds 1
absenceQuiet = seconds 10

-- | Take in a frame's outcome at the given time: a line for a user's log,
-- once the clip has been out of sight for 'absenceSeconds' with three
-- lights or more in view, saying why they aren't taken for it. It is told
-- once each time the clip goes out of sight, and again if the reason
-- changes, but never within 'absenceQuiet' of the last time. (As each
-- tracker's own @absence@ does of its own outcomes.)
absence :: Instant -> Outcome -> Absence -> (Absence, Maybe String)
absence _ Tracked{} (Absence _ told) = (Absence Nothing told, Nothing)
absence now Lost{outBlobs = n, outWhy = why} (Absence since told)
    | n >= 3 && now .-. began > absenceSeconds && due =
        ( began `seq` Absence (Just began) (Just (now, why))
        , Just ("The clip is out of sight, though " ++ show n ++ " lights are in view: " ++ describeWhy why ++ ".")
        )
    | otherwise = (began `seq` Absence (Just began) told, Nothing)
  where
    began = fromMaybe now since
    due = case told of
        Nothing -> True
        Just (at, was) -> now .-. at >= absenceQuiet && (at < began || not (sameWhy was why))

-- * The old tracker's, as the new one's

oldOutcome :: Old.Outcome -> Outcome
oldOutcome (Old.Tracked hp distance fit seen judged chosen) = Tracked hp distance fit (newSeen seen) (OldHow judged chosen)
oldOutcome (Old.Lost n masked why held) = Lost n masked (OldWhy why held)

newOutcome :: New.Outcome -> Outcome
newOutcome (New.Tracked hp distance fit seen judged chosen) = Tracked hp distance fit seen (NewHow judged chosen)
newOutcome (New.Lost n masked why rivals) = Lost n masked (NewWhy why rivals)

newSeen :: Old.Seen -> Seen
newSeen (Old.Seen missing merged strays masked) = Seen missing merged strays masked

newEvent :: Old.Event -> Event
newEvent event = case event of
    Old.Recentered -> Recentered
    Old.CalibrationStarted -> CalibrationStarted
    Old.Calibrated c -> Calibrated c
    Old.CalibrationFailed -> CalibrationFailed
    Old.Refined refinement c -> Refined refinement c
    Old.ShapeEstimated estimate -> ShapeEstimated estimate
    Old.Corrected evidence -> Corrected evidence
    Old.LightIgnored place learning -> LightIgnored place learning
    Old.LightForgotten place -> LightForgotten place
    Old.LightMoved place moving -> LightMoved place moving

newRole :: Old.BlobRole -> BlobRole
newRole Old.ReflectorBlob = ReflectorBlob
newRole Old.StrayBlob = StrayBlob
newRole Old.MaskedBlob = MaskedBlob

newRequest :: Old.Request -> Request
newRequest Old.Recenter = Recenter
newRequest Old.Calibrate = Calibrate

oldRequest :: Request -> Old.Request
oldRequest Recenter = Old.Recenter
oldRequest Calibrate = Old.Calibrate
