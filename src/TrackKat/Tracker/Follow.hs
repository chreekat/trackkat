-- | The ways of making out the clip near where a pose predicts its
-- reflectors ('followed'), and for how long a pose is told that the frame
-- doesn't fix ('partlySeen').
module TrackKat.Tracker.Follow (
    followed,
    followTries,
    rankWindow,

    -- * How the reflectors show
    Showing (..),
    showing,
    showingSeconds,
    partlySeen,
) where

import Data.Foldable (toList)
import Data.Maybe (catMaybes)

import TrackKat.Linear (norm)
import TrackKat.Match
import TrackKat.Pose
import TrackKat.Term
import TrackKat.Tracker.Account
import TrackKat.Tracker.Blobs
import TrackKat.Tracker.Reflectors
import TrackKat.Tracker.Sizes
import TrackKat.Tracker.Trail (Recent (..))
import TrackKat.World (Instant, Seconds, inSeconds, seconds, (.-.))

-- | Up to how many matches to a prediction are solved for a pose: a bound
-- on a frame's work. Each is a solve.
followTries :: Int
followTries = 6

-- | How far below the best ranked match, in nats, a match may rank and
-- still be solved for a pose: one ranked further below isn't likelier once
-- solved, as far as has been seen, a pose making up only a few noises'
-- worth that a shift of the picture can't.
rankWindow :: Double
rankWindow = 30

-- | The ways of making out the clip near where a pose predicts it at the
-- given time: each of the best ranked matches of the blobs in view to the
-- prediction (see Note [Matching blobs] in "TrackKat.Match"), best ranked
-- first, no more than 'followTries' of them and none ranked more than
-- 'rankWindow' below the best. Each is how it sees the reflectors, the
-- most the frame could say for that ('atBestAnywhere'), and the clip as it makes
-- it out, at the pose that fits it, held toward the pose predicting:
-- solved only if looked at, and nothing if no pose fits.
followed :: Known -> Instant -> Recent -> Shown -> [(Three (Maybe Sight), Score, Maybe Candidate)]
followed known now r shown = [(matchSights c, atBestAnywhere expect (matchSights c), solved c) | c <- tried]
  where
    k = knownCamera known
    m = knownClip known
    pivot = reachPivot (knownReach known)
    from = recentPose r
    dt = now .-. recentTime r
    expect = expecting known now from
    z = expectMetres expect
    -- How far the clip's whole picture shifts as the head moves and
    -- turns about its pivot at its usual speeds, in pixels.
    shiftSpread = focal k / (1000 * z) * sqrt (moveSpread dt ^ (2 :: Int) + (turnSpread dt * pi / 180 * norm pivot) ^ (2 :: Int))
    tried = case matches expect shiftSpread (shownVisible shown) of
        best : rest -> best : take (followTries - 1) (takeWhile (\c -> nats (matchRank c) >= nats (matchRank best) - rankWindow) rest)
        [] -> []
    solved c = do
        (pose, fit) <- solveNear k m pivot from (catMaybes (toList (observe <$> reflectors <*> expectSizes expect <*> matchSights c)))
        Just (Candidate pose fit (matchSights c) (matchUnused c) (length (shownMasked shown)) ByPrediction)
    observe which size = fmap (\s -> Observation which (sightWeight (sizeAt z size) s) (sightAt s))

-- | How a match shows the reflectors: all three; one perhaps behind
-- another, sharing a blob no bigger than that one alone (see
-- 'bigEnoughForTwo'), or perhaps out of sight; or one out of sight.
data Showing = AllShowing | OneBehind | OneOut
    deriving (Eq, Show)

-- | How the given sights show the reflectors, given what is expected of
-- them.
showing :: Expect -> Three (Maybe Sight) -> Showing
showing expect sights = case sequenceA sights of
    Nothing -> OneOut
    Just three -> case [(sightPixels s, sizeAt (expectMetres expect) size) | (size, s) <- toList ((,) <$> expectSizes expect <*> three), sightKind s == Merged] of
        [(a, ea), (b, eb)] | not (bigEnoughForTwo (a + b) ea eb) -> OneBehind
        _ -> AllShowing

-- | For how long since all three last showed a pose is told of the clip
-- showing less: two reflectors leave the pose free to turn about
-- the line through them, and one taken to be behind another, as when the
-- head rolls far, may be out of sight instead. This isn't how likely the
-- clip is there, which showing less for longer doesn't lessen, but how
-- long a pose it doesn't fix is worth telling.
showingSeconds :: Showing -> Seconds
showingSeconds AllShowing = seconds (1 / 0)
showingSeconds OneBehind = seconds 2.5
showingSeconds OneOut = seconds 1

-- | A pose is told of the clip showing less than all three reflectors
-- only for so long since all three last showed (see 'showingSeconds'),
-- given what is expected of the reflectors, the time, and when they last
-- all showed; never, if they haven't.
partlySeen :: Expect -> Instant -> Maybe Instant -> Term (Three (Maybe Sight))
partlySeen expect now allSeen =
    gate
        (Name "partly seen" "a reflector has been out of sight for too long")
        (\sights -> since <= inSeconds (allowed sights))
        (\sights -> Measure since AtMost (inSeconds (allowed sights)) "s")
  where
    since = maybe (1 / 0) (\seen -> inSeconds (now .-. seen)) allSeen
    allowed = showingSeconds . showing expect
