-- | The ways of making out the clip by the layout of a frame's blobs,
-- wherever a pose predicts it or none does ('laidOut').
module TrackKat.Tracker.Layout (
    asClip,
    Masked (..),
    Laid (..),
    laidOut,
    threeBlobs,
    clipSearch,
    maskedSearch,
) where

import Data.Foldable (toList)
import Data.Maybe (catMaybes)

import TrackKat.Frame (Blob (..))
import TrackKat.Lights (heaviestIn)
import TrackKat.Linear (V3 (..), identity)
import TrackKat.Match
import TrackKat.Pose
import TrackKat.Term
import TrackKat.Tracker.Account
import TrackKat.Tracker.Blobs
import TrackKat.Tracker.Reflectors (Expect (..), atBest, sightWeight, suiting)
import TrackKat.Tracker.Sizes (sizeAt)
import TrackKat.World (Instant)

-- | Up to how many of the heaviest blobs in view are searched for three
-- laid out like the clip, and up to how many of the heaviest blobs in
-- masked areas are tried in place of blobs missing: bounds on a frame's
-- work. Five give ten threes, each a solve.
clipSearch, maskedSearch :: Int
clipSearch = 5
maskedSearch = 4

-- | The pose that puts the clip's three reflectors where the given blobs
-- are, each taken for the reflector its place among them says, and its
-- RMS reprojection error in pixels; nothing if there aren't three, or no
-- pose does. The pose is solved from where the clip was last tracked, if
-- anywhere.
asClip :: Known -> [Blob] -> Maybe (Pose, Double, Three Blob)
asClip known three = do
    (l, r, top) <- identify (knownClip known) at three
    (pose, fit) <- solvePose (knownCamera known) (knownClip known) (knownLast known) (at l, at r, at top)
    Just (pose, fit, Three l r top)

-- | Whether blobs masked as still lights may be taken for reflectors.
data Masked
    = -- | never: a pose predicts where the clip is
      LeaveMasked
    | -- | in place of blobs missing in view: a clip held still can be
      -- learnt as lights (see Note [Still lights] in "TrackKat.Lights")
      TryMasked
    deriving (Eq, Show)

-- | A way of making out the clip by layout.
data Laid = Laid
    { laidSights :: Three (Maybe Sight)
    -- ^ the blob taken for each reflector
    , laidAtBest :: Score
    -- ^ the most the frame could say for it, however far away the clip
    -- ('atBestAnywhere')
    , laidAfresh :: Score
    -- ^ the most that and the room it has anywhere could come to together
    -- ('roomAtMost')
    , laidClip :: Maybe Candidate
    -- ^ the clip as it makes it out, at the pose that puts the reflectors
    -- there, solved only if looked at; nothing if no pose does
    , laidMasked :: [Blob]
    -- ^ the masked blobs it takes for reflectors
    }

-- | The ways of making out the clip by layout at the given time: each
-- three of the heaviest few blobs in view, each blob taken for the
-- reflector its place among them says. With fewer than three in view, and
-- masked blobs allowed, the heaviest in each of a few masked areas make
-- up the three.
laidOut :: Known -> Instant -> Masked -> Shown -> [Laid]
laidOut known now masked (Shown areas hidden visible) = catMaybes (map found choices)
  where
    lacking = 3 - length visible
    choices
        | lacking <= 0 = [(three, []) | three <- picks 3 (take clipSearch visible)]
        | masked == TryMasked = [(visible ++ extra, extra) | extra <- picks lacking (take maskedSearch (heaviestIn areas hidden))]
        | otherwise = []
    found (three, extra) = do
        (l, r, top) <- identify (knownClip known) at three
        let blobs = Three l r top
            sights = whole <$> blobs
            solved = do
                (pose, fit, _) <- asClip known three
                Just (Candidate pose fit sights (length visible + length extra - 3) (length hidden - length extra) ByLayout)
            bests = [(said, nats said + roomAtMost (knownCamera known) (knownClip known) anywhereRoom z (weights expect sights)) | z <- suiting somewhere sights, let expect = somewhere{expectMetres = z}; said = atBest expect sights]
        Just (Laid sights (maximum (map fst bests)) (Score (maximum (map snd bests))) solved extra)
    weights expect sights = [sightWeight (sizeAt (expectMetres expect) size) s | (size, Just s) <- toList ((,) <$> expectSizes expect <*> sights)]
    -- What is expected of the reflectors, nothing said of where.
    somewhere = expecting known now (Pose identity (V3 0 0 1000))

-- | Making out the clip by layout wants three blobs: its figure is how
-- many there are to try.
threeBlobs :: Term Int
threeBlobs = atLeast (Name "blobs" "fewer than three lights are in view") "" 3 fromIntegral
