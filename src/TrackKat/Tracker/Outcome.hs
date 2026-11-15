-- | What the tracker says of a frame: whether the clip was made out, how,
-- and if not why not; what else happened; and the words for telling it.
module TrackKat.Tracker.Outcome (
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
    describeSeen,
    describeMasked,
    describeLightIgnored,
    describeLightForgotten,
    describeLightMoved,
) where

import Data.List (intercalate)
import Text.Printf (printf)

import TrackKat.Branch (Evidence)
import TrackKat.Calibration (Calibration, Refinement)
import TrackKat.Lights (Learning (..), Moving (..), NotTheClip (..))
import TrackKat.Pose (HeadPose, Reflector)
import TrackKat.Shape (ShapeEstimate)
import TrackKat.Term

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
