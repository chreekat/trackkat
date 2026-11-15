-- | Turning camera frames into head poses, frame by frame.
module TrackKat.Tracker (
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
import Data.Maybe (fromMaybe, isJust)

import TrackKat.Branch
import TrackKat.Calibration
import TrackKat.Frame (Blob, Stripe)
import TrackKat.Lights hiding (News (..))
import qualified TrackKat.Lights as Lights
import TrackKat.Linear (M33, mulMM, norm, transpose)
import TrackKat.Match
import TrackKat.Pose
import TrackKat.Shape
import TrackKat.Term
import TrackKat.Tracker.Absence
import TrackKat.Tracker.Blobs hiding (roles)
import qualified TrackKat.Tracker.Blobs as Blobs
import TrackKat.Tracker.Choose
import TrackKat.Tracker.Judge
import TrackKat.Tracker.Layout (findAfresh)
import TrackKat.Tracker.Outcome
import TrackKat.Tracker.Sighting
import TrackKat.Tracker.Sizes
import TrackKat.Tracker.Trail

-- | Something the user asked for, carried out on the next tracked frame.
data Request = Recenter | Calibrate
    deriving (Eq, Show)

-- | A requested calibration of level in progress.
data Sweep
    = Sweep
        -- | deadline, in seconds on the clock given to 'step'
        Double
        -- | rotations relative to the reference, newest first
        [M33]

data Tracker = Tracker
    { trIntrinsics :: !Intrinsics
    , trModel :: !Model
    , trRecent :: !(Maybe Recent)
    -- ^ predicts the next frame, and is the solver's starting point
    , trSizes :: !(Three Size)
    , trReference :: !(Maybe Pose)
    , trLearner :: !Learner
    , trShape :: !ShapeLearner
    , trBranch :: !Branch
    -- ^ whether the poses tracked and recentered on are the head's
    , trSweep :: !(Maybe Sweep)
    , trPending :: ![Request]
    -- ^ oldest first
    , trMasking :: !Masking
    , trLights :: !Lights
    , trLeft :: !(Maybe LeftBehind)
    -- ^ a pose left for one found afresh, while it still predicts (see
    -- Note [Following or finding afresh])
    , trBlobs :: ![(Blob, BlobRole)]
    -- ^ the last frame's, all evaluated, so as not to hold on to the frames
    -- before
    }

-- | A tracker that starts from the given calibration and recenters on the
-- first frame it tracks.
newTracker :: Intrinsics -> Model -> Masking -> Calibration -> Tracker
newTracker k m masking c = Tracker k m Nothing (pure noSize) Nothing (newLearner m c) (newShapeLearner m) (newBranch m) Nothing [Recenter] masking noLights Nothing []

-- | What the tracker has learnt so far.
trackerCalibration :: Tracker -> Calibration
trackerCalibration = learnerCalibration . trLearner

-- | The clip tracked.
trackerModel :: Tracker -> Model
trackerModel = trModel

-- | What has been learnt of the clip's shape.
trackerShape :: Tracker -> ShapeLearner
trackerShape = trShape

-- | The clip's pose at the last recenter.
trackerCentre :: Tracker -> Maybe Pose
trackerCentre = trReference

-- | The clip's pose in the camera's axes, as last tracked.
trackerPose :: Tracker -> Maybe Pose
trackerPose = fmap recentPose . trRecent

-- | The last frame's blobs, heaviest first, and what each was taken for.
trackerBlobs :: Tracker -> [(Blob, BlobRole)]
trackerBlobs = trBlobs

-- | The still lights learnt, each as the centre and radius, in pixels, of
-- the area masked around it.
trackerLights :: Tracker -> [((Double, Double), Double)]
trackerLights = lightAreas . trLights

-- | The windows of turning weighed so far by what learns from them: for
-- the neck pivot, the clip's shape, and telling the mirror image, how
-- many were taken and how many left out, by what rejected them.
trackerWindows :: Tracker -> [(String, Tally)]
trackerWindows t = [("pivot", learnerWindows (trLearner t)), ("clip shape", shapeWindows (trShape t)), ("mirror image", branchWindows (trBranch t))]

-- | Learn and mask still lights, or not, from the next frame; turned off,
-- what was learnt is forgotten.
setMasking :: Masking -> Tracker -> Tracker
setMasking masking t
    | masking == trMasking t = t
    | otherwise = t{trMasking = masking, trLights = noLights}

request :: Request -> Tracker -> Tracker
request r t = t{trPending = trPending t ++ [r]}

-- | Track a differently shaped clip from the next frame, recentering then,
-- since the model's axes move with its shape.
setModel :: Model -> Tracker -> Tracker
setModel m t = request Recenter t{trModel = m, trRecent = Nothing, trLeft = Nothing, trLearner = changeClip m (trLearner t), trShape = newShapeLearner m, trBranch = newBranch m}

-- | Seconds of head turning to calibrate level from.
calibrationSeconds :: Double
calibrationSeconds = 5

-- | For how long, in seconds, the clip must have been tracked in every
-- frame for still lights to be learnt clear of it.
confirmSeconds :: Double
confirmSeconds = 0.5

-- | Handle one frame's stripes, seen at the given time in seconds. Pending
-- requests wait for a frame where the clip is tracked.
step :: Double -> [Stripe] -> Tracker -> (Tracker, Outcome, [Event])
step now stripes t =
    case sighting of
        Left (why, held) ->
            let (learntLights, learntNews) = learnt unseen
                -- Lights just learnt that the clip would be found in are
                -- the clip, held still: they are forgotten unannounced.
                newly = [p | LightIgnored p _ <- learntNews]
                learntMask = activeMask [] learntLights
                (lights, unlearnt)
                    | null newly = (learntLights, [])
                    | otherwise = case findAfresh known (sortBlobs learntMask blobs) of
                        Right (_, restored) -> unlearn newly (map at restored) learntLights
                        Left _ -> (learntLights, [])
                news = [e | e <- learntNews, not (isUnlearnt e)]
                isUnlearnt (LightIgnored p _) = p `elem` unlearnt
                isUnlearnt _ = False
                -- Something tracked a moment ago in a light just learnt
                -- was the light, and isn't followed.
                untracked = case trRecent t of
                    Just r
                        | not (holding [p | LightIgnored p _ <- news] (toList (seenAt k m (recentPose r) <$> reflectors)) lights) ->
                            Just r{recentSince = now}
                    _ -> Nothing
             in ( t{trLearner = lostSight (trLearner t), trShape = shapeLostSight (trShape t), trLights = lights, trRecent = untracked, trLeft = left, trBlobs = roles []}
                , Lost (length visible) (length masked) why held
                , news
                )
        Right (chosen@(Chosen way _), Sighting seenPose fit sights unused hidden judged) ->
            let allSeen = all isJust sights
                -- A pose that continues the last carries on what goes by
                -- continuity; one found afresh starts it over (see Note
                -- [Following or finding afresh]).
                (continued, since) = case (way, left, trRecent t) of
                    (FoundAfresh, _, _) -> (Nothing, now)
                    (Resumed, Just (LeftBehind was believed), _) -> (Just believed, recentSince was)
                    (_, _, recent') -> (Just (trBranch t), maybe now recentSince recent')
                -- A pose still predicting that is left for one found
                -- afresh is kept while it predicts, unless one left
                -- before it still does. What was believed of it was
                -- believed of the centre too, so it isn't kept past a
                -- correction or a recentering.
                leaving = case (way, left, following) of
                    (Resumed, _, _) -> Nothing
                    (FoundAfresh, Nothing, Just r) -> Just (LeftBehind r (trBranch t))
                    _ -> left
                -- Lights are learnt clear of the clip once it has been
                -- tracked a while: something tracked for a moment can be
                -- lights that look like the clip. If it was, and the light
                -- is learnt, it is no longer followed.
                confirmed = now - since >= confirmSeconds
                (lights, news)
                    | confirmed = learnt (View (toList sights) (reflectorDiscs seenPose sights) (usualAt sizes))
                    | otherwise = learnt (View (toList sights) [] Nothing)
                wasLight = not confirmed && holding [p | LightIgnored p _ <- news] (toList (seenAt k m seenPose <$> reflectors)) lights
                -- See Note [Which pose is the head's] in TrackKat.Branch.
                others = if allSeen then Just (alternatives k m seenPose) else Nothing
                -- Each reflector a blob of its own, with no other blob,
                -- masked or not, within its usual diameter, where a piece
                -- of it could be.
                clarity = case sequenceA sights of
                    Just three | and (alone <$> trSizes t <*> three) -> Clear
                    _ -> Doubtful
                alone size s =
                    let across = 2 * sqrt (max (sizeUsual size / (metres seenPose ^ (2 :: Int))) (sightPixels s) / pi)
                     in sightKind s == Whole && length [() | b <- blobs, distance2 (sightAt s) (at b) <= across ^ (2 :: Int)] <= 1
                watched = watch now pivot (trReference t) clarity seenPose others (fromMaybe (foundAfresh (trBranch t)) continued)
                (pose, fix) = case (correction watched, followedMirror watched) of
                    (c, Just other) | c `elem` [MirrorTracked, MirrorBoth] -> (other, c)
                    (MirrorReference, _) -> (seenPose, MirrorReference)
                    _ -> (seenPose, MirrorNeither)
                reference
                    | fix `elem` [MirrorReference, MirrorBoth] = (\old -> fromMaybe old (mirrorPose k m old)) <$> trReference t
                    | otherwise = trReference t
                correcting = fix /= MirrorNeither
                evidence = branchEvidence watched
                recent = Recent pose now (if showing (trSizes t) (metres seenPose) sights == AllShowing then now else maybe now recentAllSeen (trRecent t)) since
                sizes = resize (metres pose) <$> trSizes t <*> sights
                -- Learning in progress from before a correction was of
                -- the mirror image, and so was a requested calibration's
                -- turning.
                restarted =
                    t
                        { trRecent = Just recent
                        , trSizes = sizes
                        , trPending = []
                        , trReference = reference
                        , trBranch = corrected fix seenPose watched
                        , trLearner = if correcting then lostSight (trLearner t) else trLearner t
                        , trShape = if correcting then shapeLostSight (trShape t) else trShape t
                        , trSweep = if correcting then fmap (\(Sweep deadline _) -> Sweep deadline []) (trSweep t) else trSweep t
                        , trLights = lights
                        , trLeft = if correcting then Nothing else leaving
                        , trBlobs = roles [s | Just s <- toList sights]
                        }
                (t1, requested) = foldl (handle pose) (restarted, []) (trPending t)
                ref = maybe pose id (trReference t1)
                -- Only a pose surely the head's, its reflectors seen
                -- clearly, is learnt from.
                ((learner, refinements), (shapes, shapeNews)) = case clarity of
                    Clear | settled (trBranch t1) -> (learn now pose (trLearner t1), watchShape k now pivot pose (trShape t1))
                    _ -> ((lostSight (trLearner t1), []), (shapeLostSight (trShape t1), Nothing))
                (t2, calibrated) = calibrate pose ref t1{trLearner = learner, trShape = shapes}
                applied = trLearner t2
                seen =
                    Seen
                        [which | (which, Nothing) <- toList ((,) <$> reflectors <*> sights)]
                        (length [() | Just s <- toList sights, sightKind s == Merged])
                        unused
                        hidden
             in ( if wasLight then t2{trRecent = Nothing} else t2
                , Tracked (relativePose (appliedLevel applied) (appliedPivot applied) ref pose) (norm (poseT pose)) fit seen judged chosen
                , [Corrected evidence | correcting]
                    ++ reverse requested
                    ++ [Refined r (learnerCalibration learner) | r <- refinements]
                    ++ [ShapeEstimated est | Just est <- [shapeNews]]
                    ++ calibrated
                    ++ news
                )
  where
    k = trIntrinsics t
    m = trModel t
    blobs = findBlobs stripes
    Trail following left = stillPredicting now (Trail (trRecent t) (trLeft t))
    -- Blobs in a still light's area are ignored, but for any where the
    -- clip is predicted to be.
    mask = case trMasking t of
        NoMasking -> noMask
        MaskLights -> activeMask (maybe [] (\r -> toList (seenAt k m (recentPose r) <$> reflectors)) following) (trLights t)
    shown = sortBlobs mask blobs
    masked = shownMasked shown
    visible = shownVisible shown
    -- Learning still lights, given what the frame showed of the clip: the
    -- lights learnt, and the events saying what changed.
    learnt view = case trMasking t of
        NoMasking -> (trLights t, [])
        MaskLights -> map toEvent <$> learnLights now view blobs (trLights t)
    roles seen = Blobs.roles mask seen blobs
    toEvent (Lights.Learnt p why) = LightIgnored p why
    toEvent (Lights.Forgotten p) = LightForgotten p
    toEvent (Lights.Moved p how) = LightMoved p how
    -- Where each reflector is, or would be, in the image at a pose, out to
    -- a margin that takes in the pieces of the biggest seen, such as a
    -- reflector broken in two.
    reflectorDiscs pose sights =
        let biggestSeen = maximum (0 : [sightPixels s | Just s <- toList sights])
         in [(seenAt k m pose which, 15 + 2 * sqrt (biggestSeen / pi)) | which <- toList reflectors]
    pivot = appliedPivot (trLearner t)
    known = Known k m (Reach pivot (trReference t)) (trSizes t) (recentPose <$> trRecent t)
    sighting = makeOut known now (Trail following left) shown
    usualAt sizes = biggestAt sizes (trReference t)
    resize = resized (maybe 0 (\r -> now - recentTime r) (trRecent t))
    handle pose (acc, evs) Recenter = (acc{trReference = Just pose, trBranch = recentered (trBranch acc), trLeft = Nothing}, Recentered : evs)
    handle _ (acc, evs) Calibrate =
        (acc{trSweep = Just (Sweep (now + calibrationSeconds) [])}, CalibrationStarted : evs)
    calibrate pose ref acc = case trSweep acc of
        Nothing -> (acc, [])
        Just (Sweep deadline rots)
            | now < deadline ->
                (acc{trSweep = Just (Sweep deadline (poseR pose `mulMM` transpose (poseR ref) : rots))}, [])
            | otherwise -> case levelFromYaws rots of
                Nothing -> (acc{trSweep = Nothing}, [CalibrationFailed])
                Just l ->
                    let acc' = acc{trSweep = Nothing, trLearner = calibrateLevel l (trLearner acc)}
                     in (acc', [Calibrated (trackerCalibration acc')])
