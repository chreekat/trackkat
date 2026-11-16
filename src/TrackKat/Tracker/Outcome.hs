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
    Picking (..),
    Guess (..),
    Chosen (..),
    Rival (..),
    explainChosen,
    explainRivals,
    decisive,
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
import TrackKat.Tracker.Account (Guess (..), Picking (..), Way (..))

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
        -- ^ what each term of the pose's score found (see Note
        -- [Explaining]); worked out only if looked at
        , outChosen :: Chosen
        -- ^ which hypothesis gave the pose, and what the others scored;
        -- worked out only if looked at
        }
    | -- | The clip couldn't be made out.
      Lost
        { outBlobs :: Int
        -- ^ blobs there were, besides those masked
        , outMasked :: Int
        -- ^ blobs ignored as part of a still light
        , outWhy :: Why
        -- ^ why not (see Note [Explaining]); worked out only if looked at
        , outRivals :: [Rival]
        -- ^ every hypothesis about the frame, best scored first; worked
        -- out only if looked at
        }
    deriving (Show)

-- Note [Explaining]
-- ~~~~~~~~~~~~~~~~~
-- What to make of a frame is chosen among hypotheses by their scores, each
-- a sum of named terms (see Note [One account] in
-- "TrackKat.Tracker.Account"). So whatever is made of a frame can say
-- why: which hypothesis won ('outChosen'), what each of its terms found
-- ('outJudged'), and what every other hypothesis scored ('Rival').
--
-- A frame where the clip isn't made out says why ('Why'): what the
-- likeliest way of seeing the clip in it was, and which term counted most
-- against that ('decisive'), with its figure; or which gate made it
-- impossible; or that there was nothing to try. A limit that vetoes good
-- blobs then shows as itself and not as three strays.
--
-- All of it is lazy: tracking pays for a judgement's findings only when
-- something looks at them, and an outcome whose findings nobody looks at
-- costs one unevaluated value. Nothing of them is kept in the tracker, so
-- a tracker holds on to no frame through them.
--
-- What learns from tracked poses says what it did likewise: a light
-- learnt says why it was taken for one, a correction says on what
-- evidence, and windows of turning are counted by what took or rejected
-- them ('trackerWindows').

-- | A hypothesis about a frame, and what it scored.
data Rival = Rival Guess Score
    deriving (Eq, Show)

-- | Which hypothesis gave a tracked frame's pose: how the clip came to be
-- there, how its blobs were picked out, and what it scored; and every
-- other hypothesis about the frame, best scored first.
data Chosen = Chosen Way Picking Score [Rival]
    deriving (Eq, Show)

-- | A hypothesis in a developer's words.
explainGuess :: Guess -> String
explainGuess OutOfView = "away"
explainGuess (InView way source) =
    ( case way of
        Continued -> "continued"
        Resumed -> "resumed"
        Appeared -> "afresh"
    )
        ++ case source of
            ByPrediction -> " as predicted"
            ByLayout -> " by layout"

-- | How a tracked frame's pose was come by, for a developer: the
-- hypothesis taken with its score, and the next best.
explainChosen :: Chosen -> String
explainChosen (Chosen way source won rivals) = explainGuess (InView way source) ++ " " ++ renderScore won ++ explainRivals (take 2 rivals)

-- | Hypotheses passed over, for a developer: empty if there are none.
explainRivals :: [Rival] -> String
explainRivals [] = ""
explainRivals rivals = "; over " ++ intercalate ", " [explainGuess guess ++ " " ++ renderScore scored | Rival guess scored <- rivals]

-- | Of what a hypothesis's terms found, what counted against it: the
-- terms that make it impossible, if any do; or else the one term that
-- scored least, of those that say how the frame is unlike what was
-- expected. (How much room the hypothesis had, and whether the clip was
-- in view or where it was followed a moment before, are what they are,
-- whatever the frame shows, and aren't told as what is wrong with it.)
decisive :: [Finding] -> [Finding]
decisive found = case [f | f <- found, isImpossible (findingScore f)] of
    [] -> case [f | f <- found, brief (findingName f) `notElem` ["room", "in view", "left"]] of
        [] -> []
        f : rest -> [foldl (\least g -> if findingScore g < findingScore least then g else least) f rest]
    failed -> failed

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
    = -- | the likeliest way of seeing the clip was less likely than its
      -- being away, or was not to be told, and these terms counted against
      -- it (see 'decisive')
      Unfit [Finding]
    | -- | no pose puts the clip's reflectors where the candidate's blobs
      -- are
      NoPoseFits
    | -- | no blobs are where the reflectors were predicted
      NothingMatches
    deriving (Eq, Show)

-- | The brief names of the terms that counted against the best candidate.
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

-- | Why the clip wasn't made out, for a developer: the terms that counted
-- against it, each with its figure and limit.
explainWhy :: Why -> String
explainWhy (Why looking reason) =
    ( case looking of
        Afresh -> "finding the clip afresh, "
        Following -> "following the clip, "
    )
        ++ case reason of
            Unfit failed -> "unlikely: " ++ renderFindings failed
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
