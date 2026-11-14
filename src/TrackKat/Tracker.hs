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

import Data.Either (isRight, lefts, rights)
import Data.Foldable (toList)
import Data.List (intercalate, partition, sortOn)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Text.Printf (printf)

import TrackKat.Branch
import TrackKat.Calibration
import TrackKat.Frame (Blob (..), Stripe, stripesToBlobs)
import TrackKat.Lights hiding (News (..))
import qualified TrackKat.Lights as Lights
import TrackKat.Linear (M33, dot, mulMM, mulMV, norm, normalize, transpose, (*^), (^+^), (^-^))
import TrackKat.Match
import TrackKat.Pose
import TrackKat.Shape
import TrackKat.Term
import TrackKat.World (Seconds, World (..), during, inDegrees, inMillimetres, inPerSecond, inPixels, seconds, times, world)

-- | Something the user asked for, carried out on the next tracked frame.
data Request = Recenter | Calibrate
    deriving (Eq, Show)

-- | Something that happened while handling a frame.
data Event
    = Recentered
    | CalibrationStarted
    | -- | The level was calibrated on request and applied at once; the
      -- calibration with it.
      Calibrated Calibration
    | CalibrationFailed
    | -- | Everyday movement changed the calibration noticeably; what is
      -- applied follows it slowly.
      Refined Refinement Calibration
    | -- | The clip's shape was estimated from how the head moves, surely, or
      -- differently from when it was last (see "TrackKat.Shape").
      ShapeEstimated ShapeEstimate
    | -- | The pose tracked, or the one recentered on, was found to be the
      -- mirror image of the head's (see "TrackKat.Branch") and was
      -- switched for the head's, on this evidence.
      Corrected Evidence
    | -- | A bright light that stays put, at this place in the image, is
      -- ignored from now on (see "TrackKat.Lights"), and why it was taken
      -- for one.
      LightIgnored (Double, Double) Learning
    | -- | The light ignored at this place hasn't been seen for a while,
      -- and isn't ignored any more.
      LightForgotten (Double, Double)
    | -- | The light ignored at this place moved, off with the clip or
      -- from its place as the clip came into view: it was one of the
      -- reflectors, and isn't ignored any more.
      LightMoved (Double, Double) Moving
    deriving (Show)

-- | What a frame showed.
data Outcome
    = Tracked
        { outPose :: HeadPose
        , outDistance :: Double
        -- ^ mm from the camera to the clip
        , outFit :: Double
        -- ^ RMS reprojection error, pixels
        , outSeen :: Seen
        , outJudged :: [Finding]
        -- ^ what each term that accepted the pose found (see Note
        -- [Explaining]); worked out only if looked at
        , outChosen :: Chosen
        -- ^ how the pose was come by, of the ways tried (see Note
        -- [Following or finding afresh]); worked out only if looked at
        }
    | -- | The clip couldn't be made out.
      Lost
        { outBlobs :: Int
        -- ^ blobs there were, besides those masked
        , outMasked :: Int
        -- ^ blobs ignored as part of a still light
        , outWhy :: Why
        -- ^ why not (see Note [Explaining]); worked out only if looked at
        , outHeld :: Maybe Held
        -- ^ a pose found by layout that wasn't taken, the clip being
        -- likelier where it was (see Note [Following or finding afresh]);
        -- worked out only if looked at
        }
    deriving (Show)

-- Note [Explaining]
-- ~~~~~~~~~~~~~~~~~
-- Whether blobs are the clip is decided by terms (see "TrackKat.Term"):
-- 'canBeThere' for whether a pose is somewhere a head can be, with the fit
-- and the jump from the last frame when following the clip ('followable'),
-- and with the blobs' sizes when finding it afresh ('clipLike'). Before
-- them come rules about which blobs to try at all: how many big blobs
-- there are when finding the clip afresh ('threeBig'), and for how long
-- the clip may be followed with a reflector out of sight ('stillShowing').
--
-- A frame where the clip isn't made out says why ('Why'): what was being
-- tried, and which terms rejected the best candidate, with the figure each
-- judged and its limit. A limit that vetoes good blobs, as the reach from
-- the centre has, then shows as itself and not as three strays. A frame
-- where it is made out says what each term found, so that how close a
-- limit came can be watched ('outJudged').
--
-- Both are lazy: tracking pays for a judgement's findings only when
-- something looks at them, and an outcome whose findings nobody looks at
-- costs one unevaluated value. Nothing of them is kept in the tracker, so
-- a tracker holds on to no frame through them.
--
-- The best candidate is the first tried: when following, the best match
-- (see Note [Matching blobs] in "TrackKat.Match"); when finding afresh,
-- the three big blobs, if there are just three. Others tried after it,
-- and rejected too, aren't told.
--
-- With a pose to go on, following and finding by layout are both tried
-- when the first is rejected or weak (see Note [Following or finding
-- afresh]). A tracked frame says which gave its pose and what became of
-- the other ('outChosen'); a lost one gives following's reason, and any
-- pose found by layout that was passed over ('outHeld').
--
-- What learns from tracked poses says what it did likewise: a light
-- learnt says why it was taken for one, a correction says on what
-- evidence, and windows of turning are counted by what took or rejected
-- them ('trackerWindows').

-- Note [Following or finding afresh]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- With a pose from a moment ago, a frame's blobs can be taken for the clip
-- in two ways: by following, each reflector matched to a blob near where
-- that pose predicts it ('follow'); or by layout alone, the big blobs in
-- view taken for the clip as when there is no pose to go on ('findHere').
-- Following used to be the only way tried for as long as the pose
-- predicted ('predictionSeconds'), so that a jump following wouldn't
-- make, as after a frame followed to a wrong pose, cost up to half a
-- second out of sight before the clip was looked for afresh; and a pose
-- followed onto the wrong blobs stayed there while its gates let it. Now
-- they are hypotheses about the same frame, scored alike, and the best is
-- taken.
--
-- What they are scored by, in nats (see "TrackKat.Term"):
--
--  * the blobs' own evidence ('blobsSay'): how far each blob taken for a
--    reflector is from the size its reflector has usually been
--    ('usualSize', as a match's terms for sizes score a size), a reflector
--    unseen (what a match's term for it costs), and how far the reflectors
--    are from where the pose puts them ('misfit', under the world's
--    'centroidNoise');
--
--  * where the pose is, given the last. For a pose that continues the
--    last, how likely a head turned and moved so far in the time
--    ('continuing': gaussians of the spreads of a head's speed, the
--    world's 'headTurnSpeed' and 'headMoveSpeed', over that time). For a
--    pose that doesn't, how likely a head is there with nothing known of
--    where it was ('anywhere': anywhere a head can be, turned from the
--    centre by the world's 'headTurnSpread', within 'headReachAside' to
--    either side and in the camera's range).
--
-- These are commensurable because each is the log of a probability density
-- over the same things: the blobs' places and sizes, given the pose, and
-- the pose's six freedoms, given the last pose or given nothing. A density
-- that says the pose is within a few degrees and mm is far higher at its
-- peak than one that says only that it is somewhere a head can be, by the
-- ratio of the volumes they spread over; 'afreshCost' is the log of that
-- ratio, some 22 nats a frame after the last pose and less the longer
-- after, and it is all the advantage following has. There is no bonus for
-- following besides: the two are taken as equally likely beforehand. A
-- match's term for the reflectors' places is left out of the score, being
-- the same continuity measured in the image before the pose is solved.
--
-- So following wins whenever both take the same blobs, and a pose found
-- by layout wins when following has to take a blob of the wrong size, or
-- leave a reflector unseen, or jump, to an extent that costs more than
-- the pose found does. The hard gates still veto either: 'followable' for
-- a followed pose, 'clipLike' for one found by layout.
--
-- A pose found by layout that is within the jump following allows
-- ('jumpable') continues the last all the same: it is the clip refound
-- ('Refound'), scored by the better of 'continuing' and 'anywhere', and
-- everything that goes by continuity carries on as if it had been
-- followed: the belief about the mirror image and its window of turning
-- (see "TrackKat.Branch"), the time tracked that lets lights be learnt,
-- and what learns from poses. Only one that isn't ('FoundAfresh') starts
-- them over, as finding the clip afresh always has: either pose may then
-- be the mirror image.
--
-- When following is rejected, a pose found by layout that continues the
-- last is taken. One that doesn't has a third hypothesis to beat
-- ('stillThere'): that the clip is still where the last pose predicts it,
-- and the frame doesn't show it rightly. That scores what a match's term
-- for a reflector unseen costs, for each reflector with no blob near where
-- it is predicted. So blobs like the clip somewhere else altogether, none
-- where the reflectors were, are taken at once, at 22 nats against 54;
-- but two reflectors still in their places and a stray, as a reflector
-- fades for a moment, are not: the clip is likelier where it was, one
-- reflector out of sight, at 18 nats. This is the wait that following
-- alone amounted to, as a hypothesis with a score: it holds while the
-- pose predicts and the clip may show less ('showingSeconds'), it gives
-- way as finding afresh gets cheaper with the time since the pose, a few
-- frames for a head that has really jumped, and the frame is lost
-- meanwhile, as it was, with what was passed over told ('Held').
--
-- A pose left for one found afresh elsewhere isn't forgotten at once
-- ('LeftBehind'). Blobs that look like the clip somewhere else for a
-- frame or two used to be frames out of sight, with the clip followed
-- again from where it was when they passed. Now, if none of the
-- reflectors is still in its place, they are tracked; but if the clip is
-- then found by layout where the pose left behind still predicts it
-- ('Resumed', scored as continuing that pose), that pose's continuity is
-- taken up again as if those frames had been out of sight: a brief wrong
-- sight doesn't cost what was known of the mirror image.
--
-- What the wait protected is kept otherwise:
--
--  * A pose still predicts for 'predictionSeconds' out of sight, and a
--    frame where no hypothesis makes out the clip is lost with following's
--    reason as before, so a brief occlusion or merge is followed through
--    when the reflectors show again.
--
--  * The clip showing two reflectors, or one behind another
--    ('showingSeconds'), is followed for as long as it was. A reflector
--    unseen costs 18 nats, less than finding afresh a frame on, so such a
--    pose is outscored only once it also jumps; and what would be found
--    instead, a stray taken for the third reflector, pays for its size.
--
--  * Blobs masked as still lights are never taken for the clip while a
--    pose predicts: only blobs in view are tried by layout, exactly three
--    big ones or the best scored three of a few. Masked ones are tried,
--    as before, only with no pose to go on.
--
-- Finding by layout costs a solve or several, so it isn't tried on every
-- frame: only when following is rejected, or scores below 'weakScore';
-- and then not of the very three blobs following took, which is the same
-- hypothesis, nor of three whose sizes alone score no better than
-- following did.
--
-- What the scores can't tell: a stray the size of a reflector, taken for
-- it. A blob's size is its only evidence of what it is, the sizes learnt
-- follow what is tracked within seconds ('sizeMemory', and the latest at
-- once), and each frame is scored by itself, with no memory of how well
-- the frames before fitted. So a pose followed onto such a stray is left
-- only when a gate rejects it, as before.

-- | How a tracked frame's pose was come by (see Note [Following or
-- finding afresh]).
data Way
    = -- | by following the last pose, its reflectors matched to blobs near
      -- where it predicts them
      Followed
    | -- | by the layout of the big blobs, at a pose that continues the last
      Refound
    | -- | by the layout of the big blobs, at a pose that doesn't continue
      -- the last but one from a moment before, left for a pose found
      -- afresh
      Resumed
    | -- | by the layout of the big blobs, at a pose that doesn't continue
      -- the last, or with none to continue
      FoundAfresh
    deriving (Eq, Show)

-- | Which way gave a tracked frame's pose, and what became of the other,
-- if both were tried.
data Chosen = Chosen Way (Maybe Tried)
    deriving (Eq, Show)

-- | The way not taken: what became of it, and what the jump to the pose
-- found by layout was judged to be, against what following allows, if a
-- pose was found so. The jump is from the last pose, or, of a pose
-- resumed, from the one it continues.
data Tried = Tried Fate [Finding]
    deriving (Eq, Show)

-- | What became of a way of making out the clip that wasn't taken.
data Fate
    = -- | it was rejected, for this
      Failed Why
    | -- | it scored less: what the way taken scored, and what it did
      Outscored Score Score
    deriving (Eq, Show)

-- | What was decided between following the clip and finding it afresh in
-- a tracked frame, for a developer; nothing if only one was tried.
explainChosen :: Chosen -> Maybe String
explainChosen (Chosen _ Nothing) = Nothing
explainChosen (Chosen way (Just (Tried fate jump))) = Just $ case way of
    Followed -> "followed: " ++ against "finding the clip afresh"
    Refound -> "refound: " ++ against "following the clip" ++ "; found by layout where predicted (" ++ renderFindings jump ++ ")"
    Resumed -> "resumed: " ++ against "following the clip" ++ "; found by layout where a pose left a moment ago predicted (" ++ renderFindings jump ++ ")"
    FoundAfresh -> "found afresh: " ++ against "following the clip" ++ "; found by layout away from where predicted (" ++ renderFindings jump ++ ")"
  where
    against other = case fate of
        Failed why -> explainWhy why
        Outscored won lost -> other ++ " scored " ++ renderScore lost ++ " against " ++ renderScore won

-- | A pose found by layout away from where the last pose predicts the
-- clip, and not taken: what it scored, how many reflectors have no blob
-- near where they are predicted, and what the clip being there all the
-- same, those out of sight, scores.
data Held = Held Score Int Score
    deriving (Eq, Show)

-- | A pose passed over for the clip being where it was, for a developer.
explainHeld :: Held -> String
explainHeld (Held found missing held) =
    "found by layout elsewhere, scoring "
        ++ renderScore found
        ++ ", but likelier where it was with "
        ++ show missing
        ++ (if missing == 1 then " reflector" else " reflectors")
        ++ " out of sight ("
        ++ renderScore held
        ++ ")"

-- | Why the clip wasn't made out in a frame: what was being tried, and
-- what came of it.
data Why = Why Looking Reason
    deriving (Eq, Show)

-- | How the clip was looked for.
data Looking
    = -- | by the layout of the big blobs, with no recent pose to go on
      Afresh
    | -- | near where a recent pose predicts its reflectors
      Following
    deriving (Eq, Show)

-- | What came of looking.
data Reason
    = -- | a candidate was judged, and these terms rejected it
      Unfit [Finding]
    | -- | no pose puts the clip's reflectors where the candidate's blobs
      -- are
      NoPoseFits
    | -- | no blobs are where the reflectors were predicted
      NothingMatches
    deriving (Eq, Show)

-- | The brief names of the terms that rejected the best candidate.
whyRejectedBy :: Why -> [String]
whyRejectedBy (Why _ (Unfit failed)) = map (brief . findingName) failed
whyRejectedBy _ = []

-- | Whether two frames' reasons are the same but for their figures.
sameWhy :: Why -> Why -> Bool
sameWhy a@(Why la ra) b@(Why lb rb) = la == lb && whyRejectedBy a == whyRejectedBy b && kind ra == kind rb
  where
    kind :: Reason -> Int
    kind Unfit{} = 0
    kind NoPoseFits = 1
    kind NothingMatches = 2

-- | Why the clip is out of sight, in a user's words: a clause to follow
-- "out of sight: ".
describeWhy :: Why -> String
describeWhy (Why looking reason) = case reason of
    Unfit failed -> intercalate ", and " (map (plain . findingName) failed)
    NoPoseFits -> "the lights in view aren't laid out like the clip"
    NothingMatches -> case looking of
        Following -> "the clip's reflectors aren't where they were a moment ago"
        Afresh -> "nothing in view looks like the clip"

-- | Why the clip wasn't made out, for a developer: the terms that
-- rejected, each with its figure and limit.
explainWhy :: Why -> String
explainWhy (Why looking reason) =
    ( case looking of
        Afresh -> "finding the clip afresh, "
        Following -> "following the clip, "
    )
        ++ case reason of
            Unfit failed -> "rejected: " ++ renderFindings failed
            NoPoseFits -> "no pose puts the clip where the blobs are"
            NothingMatches -> "no blobs are where its reflectors were predicted"

-- | Since when the clip has been out of sight in every frame, if it is,
-- and when a user was last told why, with the reason given.
data Absence = Absence !(Maybe Double) !(Maybe (Double, Why))

-- | The clip in sight, and nothing told.
noAbsence :: Absence
noAbsence = Absence Nothing Nothing

-- | Seconds the clip is out of sight, with lights enough in view to be
-- it, before a user is told why; and the least between two tellings.
absenceSeconds, absenceQuiet :: Double
absenceSeconds = 1
absenceQuiet = 10

-- | Take in a frame's outcome at the given time: a line for a user's log,
-- once the clip has been out of sight for 'absenceSeconds' with three
-- lights or more in view, saying why they aren't taken for it. It is told
-- once each time the clip goes out of sight, and again if the reason
-- changes, but never within 'absenceQuiet' of the last time.
absence :: Double -> Outcome -> Absence -> (Absence, Maybe String)
absence _ Tracked{} (Absence _ told) = (Absence Nothing told, Nothing)
absence now Lost{outBlobs = n, outWhy = why} (Absence since told)
    | n >= 3 && now - began > absenceSeconds && due =
        ( began `seq` Absence (Just began) (Just (now, why))
        , Just ("The clip is out of sight, though " ++ show n ++ " lights are in view: " ++ describeWhy why ++ ".")
        )
    | otherwise = (began `seq` Absence (Just began) told, Nothing)
  where
    began = fromMaybe now since
    due = case told of
        Nothing -> True
        Just (at, was) -> now - at >= absenceQuiet && (at < began || not (sameWhy was why))

-- | How the reflectors appeared in a tracked frame.
data Seen = Seen
    { seenMissing :: [Reflector]
    -- ^ reflectors out of sight, held where recent frames had them
    , seenMerged :: Int
    -- ^ reflectors sharing a blob with another
    , seenStrays :: Int
    -- ^ blobs that weren't reflectors
    , seenMasked :: Int
    -- ^ blobs ignored as part of a still light
    }
    deriving (Eq, Show)

-- | A few words to end a status line with on anything unusual about how
-- the reflectors appeared, like "   2 of 3 reflectors, +1 stray"; empty if
-- they were three blobs and nothing else.
describeSeen :: Seen -> String
describeSeen (Seen missing merged strays masked) = case notes of
    [] -> ""
    _ -> "   " ++ intercalate ", " notes
  where
    notes =
        [show (3 - length missing) ++ " of 3 reflectors" | not (null missing)]
            ++ ["merged" | merged > 0]
            ++ ["+" ++ show strays ++ (if strays == 1 then " stray" else " strays") | strays > 0]
            ++ [describeMasked masked | masked > 0]

-- | Blobs ignored as part of a still light, as notes on the status say.
describeMasked :: Int -> String
describeMasked n = "+" ++ show n ++ " masked"

-- | Lines for the log on a still light being ignored, forgotten, and
-- found to have moved, at a place in the image.
describeLightForgotten :: (Double, Double) -> String
describeLightForgotten p = "No longer ignoring the light at " ++ describePlace p ++ ": it has been gone a while."

-- | A line on a light being ignored, with why it was taken for one.
describeLightIgnored :: (Double, Double) -> Learning -> String
describeLightIgnored p (Learning still size notClip) =
    printf "Ignoring a bright light at %s: it stayed put for %.0f seconds, %s." (describePlace p) still $ case notClip of
        NoReflectorSeen -> "before the clip was seen" :: String
        BiggerThanReflectors biggest -> printf "and at %.0f pixels is bigger than the clip's reflectors (%.0f)" size biggest
        ClipElsewhere -> "while the clip was tracked elsewhere"
        StillLongerThanHeads -> "longer than a head is held still"

-- | A line on a light found to have moved, with how it showed that.
describeLightMoved :: (Double, Double) -> Moving -> String
describeLightMoved p how =
    "No longer ignoring the light at " ++ describePlace p ++ case how of
        CarriedOff -> ": it moved off with the clip."
        LeftWhenFound -> ": it went as the clip came into view."

describePlace :: (Double, Double) -> String
describePlace (u, v) = "(" ++ show (round u :: Int) ++ ", " ++ show (round v :: Int) ++ ")"

-- | A requested calibration of level in progress.
data Sweep
    = Sweep
        -- | deadline, in seconds on the clock given to 'step'
        Double
        -- | rotations relative to the reference, newest first
        [M33]

-- | The last pose tracked.
data Recent = Recent
    { recentPose :: !Pose
    , recentTime :: !Double
    -- ^ in seconds on the clock given to 'step'
    , recentAllSeen :: !Double
    -- ^ when all three reflectors last showed (see 'showing')
    , recentSince :: !Double
    -- ^ since when the clip has been tracked in every frame
    }

-- | A pose that was left for one found afresh elsewhere, and what was
-- believed of it being the head's or the mirror image.
data LeftBehind = LeftBehind !Recent !Branch

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

-- | What a blob was taken for.
data BlobRole
    = -- | one of the clip's reflectors, or part of one
      ReflectorBlob
    | -- | something else, ignored
      StrayBlob
    | -- | part of a still light, masked
      MaskedBlob
    deriving (Eq, Show)

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

-- | How a match shows the reflectors: all three; one perhaps behind
-- another, sharing a blob no bigger than that one alone (see
-- 'bigEnoughForTwo'), or perhaps out of sight; or one out of sight.
data Showing = AllShowing | OneBehind | OneOut
    deriving (Eq)

-- | How the given sights show the reflectors, given their sizes and the
-- distance in metres.
showing :: Three Size -> Double -> Three (Maybe Sight) -> Showing
showing sizes z sights = case sequenceA sights of
    Nothing -> OneOut
    Just three -> case [(sightPixels s, sizeLatest size / (z * z)) | (size, s) <- toList ((,) <$> sizes <*> three), sightKind s == Merged] of
        [(a, ea), (b, eb)] | not (bigEnoughForTwo (a + b) ea eb) -> OneBehind
        _ -> AllShowing

-- | For how long, in seconds since all three last showed, the clip is
-- tracked showing less: two reflectors leave the pose free to turn about
-- the line through them, and one taken to be behind another, as when the
-- head rolls far, may be out of sight instead.
showingSeconds :: Showing -> Double
showingSeconds AllShowing = 1 / 0
showingSeconds OneBehind = 2.5
showingSeconds OneOut = 1

-- | Blobs smaller or larger than this many pixels are not reflectors, when
-- finding the clip afresh. A reflector lighting 250 pixels at 950 mm
-- lights about 3600 as near as 'nearest'.
minBlobPixels, maxBlobPixels :: Int
minBlobPixels = 15
maxBlobPixels = 5000

-- | The smallest blob taken for a reflector predicted nearby: one fading
-- from view, or part of one.
minFaintPixels :: Int
minFaintPixels = 6

-- | The blobs in a frame that could be reflectors, heaviest first.
findBlobs :: [Stripe] -> [Blob]
findBlobs = stripesToBlobs minFaintPixels maxBlobPixels

-- | For how long, in seconds, a pose predicts where the reflectors are. After
-- longer out of sight, the clip is found only afresh, with nothing said of
-- where it was.
predictionSeconds :: Double
predictionSeconds = 0.5

-- | For how long, in seconds, the clip must have been tracked in every
-- frame for still lights to be learnt clear of it.
confirmSeconds :: Double
confirmSeconds = 0.5

-- | The most a head turns (degrees) and moves (mm, at its pivot) between
-- frames the given number of seconds apart: from one frame to the next,
-- the world's 'headStepTurn' and 'headStepMove', which is what the shaping
-- for games allows, and more the longer apart, by its 'headTurnRate' and
-- 'headMoveRate'. A bigger jump means the blobs were matched to the wrong
-- reflectors.
maxTurn, maxMove :: Double -> Double
maxTurn dt = inDegrees (headStepTurn world) + inPerSecond (headTurnRate world) * dt
maxMove dt = inMillimetres (headStepMove world) + inPerSecond (headMoveRate world) * dt

-- | The spreads of how far a head turns (degrees) and its pivot moves (mm)
-- in the given seconds: the world's 'headTurnSpeed' and 'headMoveSpeed'
-- for that long, or for a frame if that is longer.
turnSpread, moveSpread :: Double -> Double
turnSpread dt = inDegrees (headTurnSpeed world `during` sinceSeen dt)
moveSpread dt = inMillimetres (headMoveSpeed world `during` sinceSeen dt)

sinceSeen :: Double -> Seconds
sinceSeen dt = seconds (max dt (1 / inPerSecond (frameRate world)))

-- | How much likelier, in nats, a pose is where a pose so many seconds
-- before says it most likely is than where a head most likely is with
-- nothing known of where it was: what finding the clip afresh gives up
-- (see Note [Following or finding afresh]).
--
-- It is the log of how much more room a head has anywhere than a head
-- followed has: in turning, the world's 'headTurnSpread' against
-- 'turnSpread', each of three ways; in place, the box 'canBeThere' allows
-- ('maxReachMove' to either side, and up or down, and from 'nearest' to
-- 'farthest') against a gaussian of 'moveSpread' each way. Neither is
-- less than nothing: after long enough, a pose says no more than that the
-- head is somewhere.
afreshCost :: Double -> Double
afreshCost dt = 3 * log (max 1 (inDegrees (headTurnSpread world) / turnSpread dt)) + log (max 1 (box / (moveSpread dt * sqrt (2 * pi)) ^ (3 :: Int)))
  where
    box = (2 * maxReachMove) ^ (2 :: Int) * (farthest - nearest)

-- | The score below which a followed pose is doubted, and the clip looked
-- for by layout in the same frame too (see Note [Following or finding
-- afresh]): what a head turning and moving at twice the spreads of its
-- speed scores, beyond which, as spreads are stated, little happens.
weakScore :: Score
weakScore = Score (negate (2 * (twice * twice / 2)))
  where
    twice = 2 :: Double

-- | The farthest a head turns (degrees) and moves (mm, at its pivot) from
-- where it was recentered: three of the world's 'headTurnSpread', and its
-- 'headReachAside', the move to either side, or up or down, as the camera
-- sees it. Toward or away from the camera it goes as near as 'nearest' or
-- as far as 'farthest': people lean in, and sit back after a break. Beyond
-- that the blobs aren't the clip, but something like a phone's light.
maxReachTurn, maxReachMove :: Double
maxReachTurn = inDegrees (times 3 (headTurnSpread world))
maxReachMove = inMillimetres (headReachAside world)

-- | The nearest and farthest, in mm, the clip can be from the camera: the
-- world's 'cameraNearest' and 'cameraFarthest'.
nearest, farthest :: Double
nearest = inMillimetres (cameraNearest world)
farthest = inMillimetres (cameraFarthest world)

-- | The worst RMS reprojection error, in pixels, of a pose matched from a
-- prediction: twice the world's 'centroidNoise'.
maxFit :: Double
maxFit = inPixels (times 2 (centroidNoise world))

-- | A blob as big as a reflector, when finding the clip afresh, is at
-- least this fraction (so many parts in so many) of the biggest blob in
-- view: other reflections are often tiny beside the reflectors.
besideBiggest :: (Int, Int)
besideBiggest = (3, 25)

-- | When finding the clip afresh, none of its blobs lights less than one
-- in this many of the pixels its reflector has lately, at the pose's
-- distance.
smallestShare :: Double
smallestShare = 8

-- | Up to how many of the heaviest big blobs, when there are more than
-- three, are searched for the only three laid out like the clip; and up
-- to how many of the heaviest blobs in masked areas are tried in place of
-- big blobs missing.
clipSearch, maskedSearch :: Int
clipSearch = 5
maskedSearch = 4

-- | Up to how many matches as good as the best are tried when the best
-- is an impossible jump.
jumpRetries :: Int
jumpRetries = 5

-- | Seconds over which a reflector's usual size is forgotten.
sizeMemory :: Double
sizeMemory = 10

-- | What a frame showed, when the clip was made out.
data Sighting
    = Sighting
        Chosen
        Pose
        -- | RMS reprojection error, pixels
        Double
        (Three (Maybe Sight))
        -- | blobs that weren't reflectors
        Int
        -- | blobs ignored as part of a still light
        Int
        -- | what each term that accepted the pose found
        [Finding]

-- | The clip found by the layout of the big blobs, with a pose still
-- predicting: what it scores, how its pose stands to that pose, the
-- sighting it makes given what became of following, and what its jump was
-- judged to be against what following allows.
data Layout = Layout
    { layoutScore :: Score
    , layoutWay :: Way
    , layoutSighting :: Fate -> Sighting
    , layoutJump :: [Finding]
    }

-- | A pose following the last: the pose, its RMS reprojection error in
-- pixels, and the degrees it turned and mm its pivot moved from the last.
data Moved = Moved Pose Double Double Double

-- | Three blobs taken for the clip, when finding it afresh: the pose, and
-- the blobs taken for the left, right and top reflectors.
data Found = Found Pose (Three Blob)

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
                    | otherwise = case findAfresh (learntMask, partition (inMask learntMask) blobs) of
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
        Right (Sighting chosen@(Chosen way _) seenPose fit sights unused hidden judged) ->
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
                -- before it still does.
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
                        , trLeft = leaving
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
    -- Blobs in a still light's area are ignored, but for any where the
    -- clip is predicted to be.
    following = case trRecent t of
        Just r | now - recentTime r <= predictionSeconds -> Just r
        _ -> Nothing
    left = case trLeft t of
        Just l@(LeftBehind r _) | now - recentTime r <= predictionSeconds -> Just l
        _ -> Nothing
    mask = case trMasking t of
        NoMasking -> noMask
        MaskLights -> activeMask (maybe [] (\r -> toList (seenAt k m (recentPose r) <$> reflectors)) following) (trLights t)
    (masked, visible) = partition (inMask mask) blobs
    -- Learning still lights, given what the frame showed of the clip: the
    -- lights learnt, and the events saying what changed.
    learnt view = case trMasking t of
        NoMasking -> (trLights t, [])
        MaskLights -> map toEvent <$> learnLights now view blobs (trLights t)
    -- Each blob's role, given where the reflectors were seen: a blob near
    -- one, for its size, is one, or a piece of one.
    roles seen = foldr (\(_, r) rest -> r `seq` rest) assigned assigned
      where
        assigned = [(b, role b) | b <- blobs]
        role b
            | any (\s -> distance2 (sightAt s) (at b) <= reflectorReach b ^ (2 :: Int)) seen = ReflectorBlob
            | inMask mask b = MaskedBlob
            | otherwise = StrayBlob
    -- How near a reflector's place, in pixels, a blob is its own: three,
    -- and the blob's diameter.
    reflectorReach b = 3 + 2 * sqrt (fromIntegral (blobPoints b) / pi)
    distance2 (u, v) (x, y) = (u - x) ^ (2 :: Int) + (v - y) ^ (2 :: Int)
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
    metres pose = norm (poseT pose) / 1000
    sighting = maybe (either (\why -> Left (why, Nothing)) (Right . fst) (findAfresh (mask, (masked, visible)))) followOrFind following
    -- The better of following the given pose and finding the clip by
    -- layout, the second tried only when the first is rejected or weak
    -- (see Note [Following or finding afresh]). A tie is following's.
    followOrFind r = case follow r of
        Right (followScore, followed@(Sighting _ pose fit sights unused hidden judged))
            | followScore >= weakScore -> Right followed
            | otherwise ->
                let keeping fate jump = Right (Sighting (Chosen Followed (Just (Tried fate jump))) pose fit sights unused hidden judged)
                 in case findHere r (Just (followScore, sights)) of
                        Nothing -> Right followed
                        Just (Left reason) -> keeping (Failed (Why Afresh reason)) []
                        Just (Right found)
                            | layoutScore found > followScore -> Right (layoutSighting found (Outscored (layoutScore found) followScore))
                            | otherwise -> keeping (Outscored followScore (layoutScore found)) (layoutJump found)
        Left why -> case findHere r Nothing of
            Just (Right found)
                | layoutWay found == Refound || layoutScore found > held -> Right (layoutSighting found (Failed why))
                | otherwise -> Left (why, Just (Held (layoutScore found) missing held))
              where
                (missing, held) = stillThere r
            _ -> Left (why, Nothing)
    -- The clip still where the given pose predicts it, though the frame
    -- doesn't show it so (see Note [Following or finding afresh]): how
    -- many of its reflectors have no blob near where they are predicted,
    -- and what that scores, each of them out of sight; impossible once
    -- the clip has shown less than all three for longer than it may.
    stillThere r
        | now - recentAllSeen r > showingSeconds OneOut = (missing, impossible)
        | otherwise = (missing, Score (fromIntegral missing * nats unseenScore))
      where
        -- Reflectors with a blob near their place, but no more of them
        -- than there are such blobs: one blob isn't two reflectors seen.
        places = toList (seenAt k m (recentPose r) <$> reflectors)
        near q b = distance2 q (at b) <= reflectorReach b ^ (2 :: Int)
        missing = 3 - min (length [() | q <- places, any (near q) visible]) (length [() | b <- visible, any (`near` b) places])
    -- The clip by the layout of the big blobs in view, with the given
    -- pose still predicting, or why the blobs aren't the clip. Nothing if
    -- the blobs don't allow trying: there aren't three big ones, or none
    -- is worth trying against the score and sights given, which are
    -- following's. Masked blobs aren't tried.
    --
    -- Of more than three big blobs, each three of the heaviest few that
    -- would be taken for the clip is a hypothesis, and the best scored is
    -- the one told: with a pose to go on, the sizes the reflectors had and
    -- where the clip was say which three, where finding afresh without
    -- one wants just one such three ('onlyClipLike').
    findHere r taken = case (lefts tried, sortOn (negate . nats . layoutScore) (rights tried)) of
        (_, best : _) -> Just (Right best)
        (reason : _, []) -> Just (Left reason)
        ([], []) -> Nothing
      where
        big = bigAmong visible
        threes = case compare (length big) 3 of
            EQ -> [big]
            GT | any ((> 0) . sizeUsual) (trSizes t) -> choose 3 (take clipSearch big)
            _ -> []
        tried = [foundHere r three | three <- threes, maybe True (worthTrying three) taken]
        -- Not the three blobs following took, which is the same
        -- hypothesis; nor three whose sizes alone, at the distance the
        -- clip was, score no better than following did, since where they
        -- put the clip can only cost them more.
        worthTrying three (followed, sights) =
            not (sightsOf three sights) && case identify m at three of
                Just (l, rt, top) -> score usualSize (recentPose r, whole <$> Three l rt top) > followed
                Nothing -> False
    -- Three blobs as the clip, with the given pose still predicting, as
    -- 'findHere' tells it.
    foundHere r three = do
        (pose, fit, (l, rt, top), judged) <- asClip three
        let dt = now - recentTime r
            sights = whole <$> Three l rt top
            -- The pose as moved from another, so many seconds before: if
            -- that isn't an impossible jump, as continuing it or as
            -- anywhere, whichever makes it likelier.
            from was since =
                let (turn, move) = poseChange pivot (recentPose was) pose
                    moved = Moved pose fit turn move
                    likelier = if score (continuing since) moved >= score (anywhere dt) moved then continuing since else anywhere dt
                 in (accepts (jumpable since) moved, about (const moved) likelier, findings (jumpable since) moved)
            (way, prior, jump) = case (from r dt, (\(LeftBehind was _) -> from was (now - recentTime was)) <$> left) of
                ((True, continued, jumped), _) -> (Refound, continued, jumped)
                (_, Just (True, continued, jumped)) -> (Resumed, continued, jumped)
                ((False, _, jumped), _) -> (FoundAfresh, anywhere dt, jumped)
            placed = Moved pose fit 0 0
            likely = blobsSay pose sights <> prior
        Right
            Layout
                { layoutScore = score likely placed
                , layoutWay = way
                , layoutSighting = \fate -> Sighting (Chosen way (Just (Tried fate jump))) pose fit sights (length visible - 3) (length masked) (judged ++ findings likely placed)
                , layoutJump = jump
                }
    -- Whether the blobs are just those the sights take for the reflectors,
    -- each one of its own.
    sightsOf :: [Blob] -> Three (Maybe Sight) -> Bool
    sightsOf three sights = and [or [sightKind s == Whole && sightAt s == at b | Just s <- toList sights] | b <- three]
    whole b = Just (Sight (at b) (fromIntegral (blobPoints b)) Whole)
    -- The pixels the biggest reflector, of the given sizes, lights where
    -- the head was recentered.
    usualAt sizes = case (maximum (sizeUsual <$> sizes), trReference t) of
        (biggest, Just ref) | biggest > 0 -> Just (biggest / metres ref ^ (2 :: Int))
        _ -> Nothing
    -- Of more than three big blobs, the only three of the heaviest few
    -- that would be taken for the clip, if just three would: the clip
    -- beside something else. Not before the clip has been seen: nothing
    -- then tells it from a fire and its flames laid out like it.
    onlyClipLike bs
        | any ((> 0) . sizeUsual) (trSizes t)
        , length (bigAmong bs) > 3 = case filter (isRight . asClip) (choose 3 (take clipSearch (bigAmong bs))) of
            [three] -> Just three
            _ -> Nothing
        | otherwise = Nothing
    -- Big enough to be a reflector, of the given blobs: of a fair size, and
    -- not tiny beside the biggest blob, as other reflections often are.
    bigAmong bs = let biggest = maximum (0 : map blobPoints bs) in filter (bigBeside biggest) bs
    bigBeside biggest b = blobPoints b >= minBlobPixels && blobPoints b * snd besideBiggest >= biggest * fst besideBiggest
    -- The pose of the clip, if the given blobs are its three reflectors,
    -- laid out as the clip is, at a pose a head can be at, with what the
    -- terms that say so found; or why they aren't.
    asClip three = case identify m at three of
        Nothing -> Left NoPoseFits
        Just (l, r, top) -> case solvePose k m (recentPose <$> trRecent t) (at l, at r, at top) of
            Nothing -> Left NoPoseFits
            Just (pose, fit) ->
                let found = Found pose (Three l r top)
                 in if accepts clipLike found
                        then Right (pose, fit, (l, r, top), findings clipLike found)
                        else Left (Unfit (rejections clipLike found))
    -- Three blobs at a pose are the clip: the pose is somewhere a head can
    -- be, and the blobs are big enough.
    clipLike = about (\(Found pose _) -> pose) canBeThere <> bigEnough
    -- None of the blobs far smaller than its reflector has lately been,
    -- at the pose's distance: lights much nearer the camera than the
    -- head, laid out like the clip, light far fewer pixels than its
    -- reflectors would there. Its figure is that of the blob smallest
    -- for its reflector: its pixels, against the least its reflector
    -- would light.
    bigEnough =
        gate
            (Name "size" "the lights in view are much smaller than the clip's reflectors have been")
            (\(Found pose three) -> and (liftA2 (\size b -> fromIntegral (blobPoints b) * smallestShare * metres pose ^ (2 :: Int) >= sizeUsual size) (trSizes t) three))
            ( \(Found pose three) ->
                let least size = sizeUsual size / (smallestShare * metres pose ^ (2 :: Int))
                    sized = toList (liftA2 (\size b -> (fromIntegral (blobPoints b), least size)) (trSizes t) three)
                    (lit, wanted) = foldr1 (\a b -> if fst a - snd a <= fst b - snd b then a else b) sized
                 in Measure lit AtLeast wanted "px"
            )
    -- With nothing to predict from, three big blobs by their layout,
    -- solved from where the clip was last seen if anywhere, given a mask
    -- and the blobs it masks and doesn't; and the masked blobs taken as
    -- reflectors. With fewer than three big blobs unmasked, the heaviest
    -- in each of a few masked areas are tried too, those that make the
    -- three most alike in size first: a clip held still can be learnt as
    -- lights (see Note [Still lights] in TrackKat.Lights). With more, the
    -- only three that would be taken for the clip, if just three would.
    --
    -- When none is found, the reason is the three big blobs' own, if
    -- there were just three; or else that there weren't.
    findAfresh (areas, (hidden, shown)) = case rights attempts of
        found : _ -> Right found
        [] -> Left . Why Afresh $ case (rejections threeBig (length bigShown), lefts attempts) of
            ([], reason : _) -> reason
            (count, _) -> Unfit count
      where
        biggest = maximum (0 : map blobPoints shown)
        bigShown = bigAmong shown
        -- Three blobs to try, and those of them masked.
        choices = case compare (length bigShown) 3 of
            EQ -> [(bigShown, [])]
            LT -> [(bigShown ++ extra, extra) | extra <- sortOn alike (choose (3 - length bigShown) (take maskedSearch (filter (bigBeside biggest) (heaviestIn areas hidden))))]
            GT -> [(three, []) | Just three <- [onlyClipLike shown]]
        alike extra = let ps = map blobPoints (bigShown ++ extra) in fromIntegral (maximum ps) / fromIntegral (max 1 (minimum ps)) :: Double
        attempts = map attempt choices
        attempt (three, extra) = do
            (pose, fit, (l, r, top), judged) <- asClip three
            Right (Sighting (Chosen FoundAfresh Nothing) pose fit (Three (whole l) (whole r) (whole top)) (length shown + length extra - 3) (length hidden - length extra) judged, extra)
    -- Finding the clip afresh wants just three big blobs: with fewer, or
    -- more, it is found only as 'findAfresh' says.
    threeBig :: Term Int
    threeBig =
        within
            ( Name "big blobs" "fewer than three lights big enough to be reflectors are in view"
            , Name "big blobs" "more than three lights are in view, and no three of them stand out as the clip"
            )
            ""
            (3, 3)
            fromIntegral
    -- The best match to the prediction, unless its pose is an impossible
    -- jump; then, of the next few that see as many reflectors, the one whose
    -- pose is nearest. (Fewer reflectors can always be fitted by a pose
    -- near the last, so they would hide a real jump.)
    --
    -- When none is followed, the reason is the best match's; or, if there
    -- is no match to try, that the best there would be shows too little of
    -- the clip for too long, if there is one.
    follow r =
        let dt = now - recentTime r
            allMatches = matches k m (recentPose r) (trSizes t) visible
            candidates = filter (accepts stillShowing) allMatches
            -- The clip is followed showing less than all three reflectors
            -- only for so long since all three last showed.
            stillShowing =
                gate
                    (Name "partly seen" "a reflector has been out of sight for too long")
                    (\c -> now - recentAllSeen r <= allowed c)
                    (\c -> Measure (now - recentAllSeen r) AtMost (allowed c) "s")
            allowed c = showingSeconds (showing (trSizes t) (metres (recentPose r)) (matchSights c))
            attempt c = case solveNear k m pivot (recentPose r) (observations (metres (recentPose r)) c) of
                Nothing -> Left NoPoseFits
                Just (pose, fit) ->
                    let (turn, move) = poseChange pivot (recentPose r) pose
                        jump = max (turn / maxTurn dt) (move / maxMove dt)
                        moved = Moved pose fit turn move
                        likely = blobsSay pose (matchSights c)
                     in if accepts (followable dt) moved
                            then
                                Right
                                    ( jump
                                    ,
                                        ( Score (fromIntegral (3 - seenCount c) * nats unseenScore + nats (score (likely <> continuing dt) moved))
                                        , Sighting (Chosen Followed Nothing) pose fit (matchSights c) (matchUnused c) (length masked) (matchJudged c ++ findings (followable dt <> likely <> continuing dt) moved)
                                        )
                                    )
                            else Left (Unfit (rejections (followable dt) moved))
         in case candidates of
                [] -> Left . Why Following $ case allMatches of
                    c : _ -> Unfit (rejections stillShowing c)
                    [] -> NothingMatches
                c : cs -> case attempt c of
                    Right (_, s) -> Right s
                    Left reason ->
                        let alike = [a | a <- cs, seenCount a == seenCount c]
                         in case sortOn fst (rights (map attempt (take jumpRetries alike))) of
                                (_, s) : _ -> Right s
                                [] -> Left (Why Following reason)
    -- A pose follows the last, so many seconds before: it fits what was
    -- seen, it isn't an impossible jump, and it is somewhere a head can be.
    followable dt =
        atMost (Name "fit" "the lights in view don't fit the clip's shape") "px" maxFit (\(Moved _ fit _ _) -> fit)
            <> jumpable dt
            <> about (\(Moved pose _ _ _) -> pose) canBeThere
    -- A pose isn't an impossible jump from the last, so many seconds
    -- before.
    jumpable dt =
        gate
            (Name "turn" "the head would have turned faster than heads turn")
            (\(Moved _ _ turn _) -> turn / maxTurn dt <= 1)
            (\(Moved _ _ turn _) -> Measure turn AtMost (maxTurn dt) "degrees")
            <> gate
                (Name "move" "the head would have moved faster than heads move")
                (\(Moved _ _ _ move) -> move / maxMove dt <= 1)
                (\(Moved _ _ _ move) -> Measure move AtMost (maxMove dt) "mm")
    -- The soft terms that hypotheses about a frame are compared by (see
    -- Note [Following or finding afresh]).
    --
    -- How likely the reflectors are seen so far from where a pose puts
    -- them: each a centre out by the fit, under the world's
    -- 'centroidNoise'.
    misfit = gaussian (Name "misfit" "the lights in view don't quite fit the clip's shape") "px" (inPixels (centroidNoise world) / sqrt 3) (\(Moved _ fit _ _) -> fit)
    -- How likely a head turned and moved as far as a pose is from the
    -- last, so many seconds before.
    continuing dt =
        gaussian (Name "turning" "the head would have turned faster than heads mostly turn") "degrees" (turnSpread dt) (\(Moved _ _ turn _) -> turn)
            <> gaussian (Name "moving" "the head would have moved faster than heads mostly move") "mm" (moveSpread dt) (\(Moved _ _ _ move) -> move)
    -- How likely a head is at a pose with nothing known of where it was,
    -- beside how likely one followed for so many seconds is where it was:
    -- anywhere it can be, and turned from the centre as heads are.
    anywhere dt =
        cost (Name "afresh" "nothing says where the clip would be") (const (afreshCost dt))
            <> maybe mempty (\ref -> gaussian (Name "reach" "turned further than heads mostly turn") "degrees" (inDegrees (headTurnSpread world)) (\(Moved pose _ _ _) -> fst (poseChange pivot ref pose))) (trReference t)
    -- What the blobs taken for the reflectors at a pose say of it: how
    -- well they fit it, and how likely each that is a reflector's own is
    -- the size its reflector has been lately, at the pose's distance, as
    -- a match's terms for sizes score a size. (Against the usual size, not
    -- the latest as matching goes by: the latest is whatever the last
    -- frame took for the reflector, which is what is in question.)
    blobsSay pose sights = about (const (pose, sights)) usualSize <> misfit
    usualSize :: Term (Pose, Three (Maybe Sight))
    usualSize =
        likelihood (Name "usual size" "the lights in view aren't the size the clip's reflectors have been") $ \(pose, sights) ->
            sum [nats (sizeScore (sizeUsual size / metres pose ^ (2 :: Int)) (sightPixels sight)) | (size, Just sight) <- toList ((,) <$> trSizes t <*> sights), sightKind sight /= Merged]
    -- Somewhere a head can be: at a sane distance from the camera, and not
    -- too far to the side of where it was recentered, though as near or as
    -- far from the camera as it likes.
    canBeThere :: Term Pose
    canBeThere =
        within
            ( Name "near" "too close to the camera"
            , Name "far" "too far from the camera"
            )
            "mm"
            (nearest, farthest)
            (norm . poseT)
            <> maybe mempty reach (trReference t)
      where
        pivotAt (Pose r p) = mulMV r pivot ^+^ p
        reach ref =
            atMost (Name "turned" "turned further from where you recentered than a head turns") "degrees" maxReachTurn (\pose -> fst (poseChange pivot ref pose))
                <> atMost (Name "aside" "too far to the side of where you recentered") "mm" maxReachMove (aside ref)
        aside ref pose =
            let fromCamera = normalize (pivotAt ref)
                moved = pivotAt pose ^-^ pivotAt ref
                back = dot moved fromCamera
             in norm (moved ^-^ back *^ fromCamera)
    seenCount c = length (catMaybes (toList (matchSights c)))
    observations z c = catMaybes (toList (observe z <$> reflectors <*> trSizes t <*> matchSights c))
    -- A dim reflector, smaller than usual, counts for less: part of it may
    -- be too dim to see, moving its centroid. One sharing a blob counts for
    -- half, its place being an estimate.
    observe z which size = fmap $ \s ->
        let usual = sizeUsual size / (z * z)
            dim = if usual > 0 then min 1 (sightPixels s / usual) else 1
            shared = if sightKind s == Merged then 0.5 else 1
         in Observation which (dim * shared) (sightAt s)
    -- Reflectors' sizes, at 1 m, after a frame at the given distance. One
    -- sharing a blob with another has its share guessed, not seen.
    resize z size seen =
        let since = maybe 0 (\r -> now - recentTime r) (trRecent t)
            usual = sizeUsual size * exp (negate since / sizeMemory)
         in case seen of
                Just s | sightKind s /= Merged -> let atOneMetre = sightPixels s * z * z in Size atOneMetre (max atOneMetre usual)
                _ -> size{sizeUsual = usual}
    at b = (blobX b, blobY b)
    handle pose (acc, evs) Recenter = (acc{trReference = Just pose, trBranch = recentered (trBranch acc)}, Recentered : evs)
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

-- | The ways to choose so many of a list, in order.
choose :: Int -> [a] -> [[a]]
choose 0 _ = [[]]
choose _ [] = []
choose n (x : xs) = map (x :) (choose (n - 1) xs) ++ choose n xs
