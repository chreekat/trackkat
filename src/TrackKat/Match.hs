{-# LANGUAGE DeriveTraversable #-}

-- | Which blobs are which reflectors, when a recent pose says where each
-- reflector should be in the image.
--
-- Usually each reflector is one blob. But a reflector can fade from view
-- (they are tape, dim when seen at a slant), two can run together
-- into one blob, one can break into two pieces, and other bright things
-- can show up as extra blobs. See Note [Matching blobs].
module TrackKat.Match (
    Three (..),
    reflectors,
    Size (..),
    noSize,
    Kind (..),
    Sight (..),
    Match (..),
    matchScore,
    matches,
    splitMerged,
    bigEnoughForTwo,
) where

import Data.Foldable (toList)
import Data.List (sortOn)
import Data.Maybe (catMaybes)

import TrackKat.Frame (Blob (..), blobAxes)
import TrackKat.Linear (norm)
import TrackKat.Pose
import TrackKat.Term
import TrackKat.World (World (..), inPixels, world)

-- | One of something for each reflector: left, right, top.
data Three a = Three a a a
    deriving (Show, Functor, Foldable, Traversable)

instance Applicative Three where
    pure a = Three a a a
    Three f g h <*> Three a b c = Three (f a) (g b) (h c)

reflectors :: Three Reflector
reflectors = Three LeftReflector RightReflector TopReflector

-- | How big a reflector's blob has been lately, in the pixels it would have
-- at 1 m (a blob's area goes with the inverse square of distance).
data Size = Size
    { sizeLatest :: !Double
    -- ^ in the last frame it was seen in
    , sizeUsual :: !Double
    -- ^ the most it has been lately
    }
    deriving (Show)

-- | Not seen yet.
noSize :: Size
noSize = Size 0 0

-- | How a reflector appeared.
data Kind
    = -- | as a blob of its own
      Whole
    | -- | as two blobs close together, taken as one
      Rejoined
    | -- | sharing one blob with another reflector
      Merged
    deriving (Eq, Show)

-- | Where a reflector was seen, and how many pixels it lit.
data Sight = Sight
    { sightAt :: !(Double, Double)
    , sightPixels :: !Double
    , sightKind :: !Kind
    }
    deriving (Show)

-- | A way to see the reflectors in the blobs: each reflector's sight, or
-- Nothing for one not seen; how many blobs it leaves unused; how unlikely
-- it is, from how far it puts the reflectors from where they were
-- predicted and how their sizes changed, in squared pixels; and what each
-- term of that found (see Note [Matching blobs]).
data Match = Match
    { matchSights :: Three (Maybe Sight)
    , matchUnused :: Int
    , matchCost :: Double
    , matchJudged :: [Finding]
    }
    deriving (Show)

-- | How likely a match is, as its terms score it.
matchScore :: Match -> Score
matchScore m = Score (negate (matchCost m / squaredPixelsPerNat))

-- Note [Matching blobs]
-- ~~~~~~~~~~~~~~~~~~~~~
-- A recent pose predicts where each reflector appears and, from the sizes
-- each has had lately, roughly how big. Each reflector's candidates are
-- the blobs, and pairs of blobs close together taken as one (a reflector
-- that broke in two); a reflector can also go unseen, at a fixed cost, but
-- only one of them. Two reflectors predicted close together can also share
-- one blob, split along its length by 'splitMerged'; whether it is big
-- enough to be both says whether they are both seen (see
-- 'bigEnoughForTwo').
--
-- A way of matching costs the squared distances, in pixels, between where
-- it sees the reflectors and where they were predicted, after taking out
-- the shift they share (fast movement moves them all together), plus a
-- cost for each blob that is far from the size predicted. A reflector is
-- not taken to be unseen, nor two to share a blob, while a blob of half
-- the usual size of one of them or more is left over: that would be it.
-- Matches come best first; the caller solves for the pose from the first,
-- and checks it.
--
-- The costs are terms (see "TrackKat.Term"): the reflectors' places, their
-- sizes, and any unseen, for reflectors each seen apart ('apartTerm'); the
-- places, the shared blob's size and the third reflector's, for two
-- sharing a blob ('sharingTerm'). A squared distance of d pixels is the
-- log-likelihood of a centre out by d under noise of 'centroidNoise',
-- d squared over twice the noise squared, so the costs as reckoned, in
-- squared pixels, are 'squaredPixelsPerNat' times the terms' nats. Which
-- matches there are to cost is still decided by rules outside the terms:
-- at least two reflectors seen, no blob used twice, none left over that
-- would be a reflector taken as unseen ('missingPlausible'), and two
-- sharing a blob only when predicted within 'reach' of each other.

-- | The ways to match blobs to reflectors, best first, given a recent pose
-- and the reflectors' recent sizes. At least two reflectors are seen in
-- each.
matches :: Intrinsics -> Model -> Pose -> Three Size -> [Blob] -> [Match]
matches k m pose sizes blobs = sortOn matchCost (separate ++ merged)
  where
    indexed = zip [0 :: Int ..] (take maxBlobs blobs)
    metres = norm (poseT pose) / 1000
    expected = fmap (\s -> sizeLatest s / (metres * metres)) sizes
    predicted = fmap (seenAt k m pose) reflectors
    singles = [([i], Sight (blobX b, blobY b) (pixels b) Whole) | (i, b) <- indexed]
    pieces = [([i, j], rejoin a b) | (i, a) <- indexed, (j, b) <- indexed, i < j, together a b]
    units = singles ++ pieces
    -- Each reflector's few best candidates, by distance and size.
    options = (\e q -> take 4 (sortOn (\(_, s) -> distance2 q (sightAt s) + sizeCost e (sightPixels s)) units)) <$> expected <*> predicted
    unused used = length blobs - length used
    usual = fmap (\s -> sizeUsual s / (metres * metres)) sizes
    -- A reflector can go unseen, or share a blob, only if no blob left over
    -- is half its usual size or more: that would be the reflector, seen far
    -- from where it was predicted.
    missingPlausible used size Nothing = size <= 0 || and [wholeFactor * pixels b < size | (i, b) <- indexed, i `notElem` used]
    missingPlausible _ _ (Just _) = True
    separate =
        [ judged apartTerm (Apart (shiftedCost placed) (sum sized) (missingCost * fromIntegral (3 - length seen))) (fmap (fmap snd) chosen) (unused used)
        | chosen <- traverse (Nothing :) (fmap (map Just) options)
        , let seen = catMaybes (toList chosen)
        , length seen >= 2
        , let used = concatMap fst seen
        , distinct used
        , and (missingPlausible used <$> usual <*> chosen)
        , let placed = catMaybes (toList ((\q -> fmap (\(_, s) -> (q, sightAt s))) <$> predicted <*> chosen))
              sized = catMaybes (toList ((\e -> fmap (\(_, s) -> sizeCost e (sightPixels s))) <$> expected <*> chosen))
        ]
    merged =
        [ judged sharingTerm sharing (place (Just si) (Just sj) (Just third)) (unused (i : thirdUsed))
        | (qi, qj, qk, ei, ej, ek, ui, uj, otherOptions, place) <- pairs
        , distance2 qi qj <= reach ei ej ^ (2 :: Int)
        , (i, b) <- take 2 (sortOn (\(_, b) -> distance2 (between qi qj ei ej) (blobX b, blobY b)) indexed)
        , let (si, sj) = splitMerged b qi qj ei ej
        , (thirdUsed, third) <- take 1 [o | o@(us, _) <- otherOptions, i `notElem` us]
        , missingPlausible (i : thirdUsed) ui Nothing && missingPlausible (i : thirdUsed) uj Nothing
        , let sharing =
                Sharing
                    (shiftedCost [(qi, sightAt si), (qj, sightAt sj), (qk, sightAt third)])
                    (sizeCost (ei + ej) (pixels b))
                    (sizeCost ek (sightPixels third))
        ]
    Three qL qR qT = predicted
    Three eL eR eT = expected
    Three oL oR oT = options
    Three uL uR uT = usual
    pairs =
        [ (qL, qR, qT, eL, eR, eT, uL, uR, oT, \a b c -> Three a b c)
        , (qL, qT, qR, eL, eT, eR, uL, uT, oR, \a b c -> Three a c b)
        , (qR, qT, qL, eR, eT, eL, uR, uT, oL, \a b c -> Three c a b)
        ]
    between (ui, vi) (uj, vj) ei ej =
        let f = share ei ej in (f * ui + (1 - f) * uj, f * vi + (1 - f) * vj)

-- | The costs, in squared pixels, of a match with each reflector seen
-- apart or not at all: of the places, of the sizes, and of any unseen.
data Apart = Apart Double Double Double

-- | The costs, in squared pixels, of a match with two reflectors sharing a
-- blob: of the places, of the shared blob's size, and of the third
-- reflector's size.
data Sharing = Sharing Double Double Double

-- | Squared pixels of cost to a nat of log-likelihood: twice the square
-- of the world's 'centroidNoise'.
squaredPixelsPerNat :: Double
squaredPixelsPerNat = 2 * inPixels (centroidNoise world) ^ (2 :: Int)

-- | The terms of a match's cost. They add in the order the costs always
-- have, so that matches come in the same order to the last bit.
apartTerm :: Term Apart
apartTerm =
    ( squaredPixels (Name "places" "the reflectors aren't where they were expected") (\(Apart places _ _) -> places)
        <> squaredPixels (Name "sizes" "the reflectors aren't the size they were") (\(Apart _ sizes _) -> sizes)
    )
        <> squaredPixels (Name "unseen" "a reflector is out of sight") (\(Apart _ _ unseen) -> unseen)

sharingTerm :: Term Sharing
sharingTerm =
    ( squaredPixels (Name "places" "the reflectors aren't where they were expected") (\(Sharing places _ _) -> places)
        <> squaredPixels (Name "shared size" "two reflectors run together aren't the size of both") (\(Sharing _ shared _) -> shared)
    )
        <> squaredPixels (Name "third size" "the third reflector isn't the size it was") (\(Sharing _ _ third) -> third)

-- | A cost in squared pixels, as a term.
squaredPixels :: Name -> (h -> Double) -> Term h
squaredPixels name f = cost name (\h -> f h / squaredPixelsPerNat)

-- | A match as its terms judge it: its cost is what they come to, in
-- squared pixels.
judged :: Term h -> h -> Three (Maybe Sight) -> Int -> Match
judged term h sights left = Match sights left (negate (nats (score term h)) * squaredPixelsPerNat) (findings term h)

-- | Up to how many blobs, heaviest first, are considered.
maxBlobs :: Int
maxBlobs = 8

-- | The cost of a reflector going unseen: as if it were six of the world's
-- 'centroidNoise', 6 pixels, from where it was predicted.
missingCost :: Double
missingCost = (6 * inPixels (centroidNoise world)) ^ (2 :: Int)

-- | How many times bigger than a blob left over a reflector's usual size
-- must be for the reflector to be unseen, or sharing a blob, and not that
-- blob: a blob half its usual size or more would be it.
wholeFactor :: Double
wholeFactor = 2

-- | The cost of a blob being bigger or smaller than predicted: as if it
-- were this many pixels away for each factor of e.
sizeScale :: Double
sizeScale = 10

sizeCost :: Double -> Double -> Double
sizeCost expected actual
    | expected <= 0 || actual <= 0 = 0
    | otherwise = (sizeScale * log (actual / expected)) ^ (2 :: Int)

-- | Squared distances between predicted and seen places, after taking out
-- their mean offset, plus a tenth of that offset's square.
shiftedCost :: [((Double, Double), (Double, Double))] -> Double
shiftedCost pts = sum [distance2 (u - du, v - dv) (pu, pv) | ((pu, pv), (u, v)) <- pts] + 0.1 * (du * du + dv * dv)
  where
    n = fromIntegral (max 1 (length pts))
    du = sum [u - pu | ((pu, _), (u, _)) <- pts] / n
    dv = sum [v - pv | ((_, pv), (_, v)) <- pts] / n

distinct :: [Int] -> Bool
distinct (x : xs) = x `notElem` xs && distinct xs
distinct [] = True

distance2 :: (Double, Double) -> (Double, Double) -> Double
distance2 (a, b) (c, d) = (a - c) ^ (2 :: Int) + (b - d) ^ (2 :: Int)

pixels :: Blob -> Double
pixels = fromIntegral . blobPoints

-- | Two blobs close enough to be pieces of one reflector: nearer than the
-- diameter of a disc of their combined area.
together :: Blob -> Blob -> Bool
together a b = distance2 (blobX a, blobY a) (blobX b, blobY b) <= 4 * (pixels a + pixels b) / pi

-- | Two blobs taken as one: their brightness-weighted centroid.
rejoin :: Blob -> Blob -> Sight
rejoin a b = Sight ((wa * blobX a + wb * blobX b) / w, (wa * blobY a + wb * blobY b) / w) (pixels a + pixels b) Rejoined
  where
    weightOf x = if blobWeight x > 0 then fromIntegral (blobWeight x) else pixels x
    wa = weightOf a
    wb = weightOf b
    w = wa + wb

-- | How close together two reflectors must be predicted, in pixels, to be
-- seen as one blob: they touch when as close as the sum of their radii
-- (from their expected sizes, or 10 pixels when not known), and half as
-- much again allows for movement.
reach :: Double -> Double -> Double
reach ei ej = 1.5 * (radius ei + radius ej)
  where
    radius e = if e > 0 then sqrt (e / pi) else 10

-- | Whether a blob is big enough to be two reflectors of the given expected
-- sizes (0 for unknown) together: nearer, on the log scale that sizes are
-- compared on, to the two side by side than to the bigger alone. One no
-- bigger could as well be either alone, the other out of sight.
bigEnoughForTwo :: Double -> Double -> Double -> Bool
bigEnoughForTwo area ei ej = accepts bothInOne (area, ei, ej)

-- | A blob's pixels, and two reflectors' expected sizes: whether the blob
-- is big enough to be both (see 'bigEnoughForTwo'). Its figure is the
-- blob's size against the least that would be both, the geometric mean of
-- the bigger alone and the two together.
bothInOne :: Term (Double, Double, Double)
bothInOne =
    gate
        (Name "both in one" "a blob two reflectors would share is no bigger than one")
        (\(area, ei, ej) -> ei <= 0 || ej <= 0 || area * area > max ei ej * (ei + ej))
        (\(area, ei, ej) -> Measure area AtLeast (sqrt (max ei ej * (ei + ej))) "px")

-- | The first reflector's share of a blob the two make together, from their
-- expected sizes.
share :: Double -> Double -> Double
share ei ej
    | ei > 0 && ej > 0 = ei / (ei + ej)
    | otherwise = 0.5

-- | Split a blob two reflectors share, given where each was predicted and
-- the size each was expected to be (0 for unknown).
--
-- Each reflector is taken to be a disc of its expected size (or, not
-- knowing, of half the blob). The two are placed on the blob's longest
-- axis, around its centroid in proportion to their sizes, as far apart as
-- accounts for the blob's spread along that axis beyond that of the discs
-- themselves: not apart at all if the blob is round. Which goes which way
-- along the axis follows the prediction.
splitMerged :: Blob -> (Double, Double) -> (Double, Double) -> Double -> Double -> (Sight, Sight)
splitMerged b (ui, vi) (uj, vj) ei ej = (Sight (at (1 - f)) (f * area) Merged, Sight (at (-f)) ((1 - f) * area) Merged)
  where
    area = pixels b
    known = ei > 0 && ej > 0
    f = share ei ej
    (ai, aj) = if known then (ei, ej) else (area / 2, area / 2)
    disc a = a / (4 * pi) + 1 / 12
    (major, _, angle) = blobAxes b
    own = f * disc ai + (1 - f) * disc aj
    apart = sqrt (max 0 (major - own) / (f * (1 - f)))
    (ax, ay) = (cos angle, sin angle)
    direction = if (ui - uj) * ax + (vi - vj) * ay < 0 then -1 else 1
    at t = (blobX b + direction * t * apart * ax, blobY b + direction * t * apart * ay)
