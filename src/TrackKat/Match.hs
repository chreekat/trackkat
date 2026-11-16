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
    Kind (..),
    Sight (..),
    Match (..),
    matches,
    bigEnoughForTwo,
) where

import Data.Foldable (toList)
import Data.List (sortOn)
import Data.Maybe (catMaybes)

import TrackKat.Frame (Blob (..), blobAxes)
import TrackKat.Sight
import TrackKat.Term
import TrackKat.Tracker.Reflectors
import TrackKat.Tracker.Sizes (readSays, reading, sizeAt)
import TrackKat.World (World (..), inPixels, world)

-- | A way to see the reflectors in the blobs: each reflector's sight, or
-- Nothing for one not seen; how many blobs it leaves unused; and how it
-- ranks among the ways there are, with what each term of that found (see
-- Note [Matching blobs]).
data Match = Match
    { matchSights :: Three (Maybe Sight)
    , matchUnused :: Int
    , matchRank :: Score
    , matchJudged :: [Finding]
    }
    deriving (Eq, Show)

-- Note [Matching blobs]
-- ~~~~~~~~~~~~~~~~~~~~~
-- A recent pose predicts where each reflector appears and, from what is
-- learnt of each one's size, roughly how big. Each reflector's candidates
-- are the blobs, and pairs of blobs close together taken as one (a
-- reflector that broke in two); a reflector can also go unseen, but only
-- one of them. Two reflectors predicted close together can also share one
-- blob, split along its length by 'splitMerged'.
--
-- The ways of matching are many, and working out the pose each gives
-- costs a solve, so they are ranked first, by what the frame would say of
-- each if the clip were just where it was predicted but for a shift of
-- the whole picture of it (fast movement moves the reflectors all
-- together): the very terms a hypothesis about the frame is scored by in
-- the end ('whatShows', the shift taken out of what is seen), and one of
-- matching's own for the shift, which stands in for how the head got
-- there until a pose says. The rank orders the ways, best first, and
-- says which are worth a solve; it decides nothing more. What a match
-- comes to is what its pose scores.
--
-- Which ways there are is by rules where they are what can be and by the
-- terms otherwise. At least two reflectors are seen, since fewer fix no
-- pose; no blob is two things at once; and two reflectors share a blob
-- only when predicted within 'reach' of each other, since blobs that
-- don't touch don't merge. A reflector may be unseen whatever blobs are
-- left over: that a blob left over would be it is for its terms to say.

-- | The ways to match blobs to reflectors, best ranked first, given where
-- and how big the reflectors are expected, and the spread, in pixels, of
-- how far the clip's whole picture shifts from where it was expected. At
-- least two reflectors are seen in each.
matches :: Expect -> Double -> [Blob] -> [Match]
matches expect shiftSpread blobs = sortOn (negate . nats . matchRank) (separate ++ merged)
  where
    indexed = zip [0 :: Int ..] (take maxBlobs blobs)
    z = expectMetres expect
    expected = fmap (sizeAt z) (expectSizes expect)
    predicted = expectAt expect
    noise = inPixels (centroidNoise world)
    singles = [([i], Sight (blobX b, blobY b) (pixels b) Whole) | (i, b) <- indexed]
    pieces = [([i, j], rejoin a b) | (i, a) <- indexed, (j, b) <- indexed, i < j, together a b]
    units = singles ++ pieces
    -- Each reflector's few best candidates, by distance and size.
    options = (\size q -> take 4 (sortOn (\(_, s) -> distance2 q (sightAt s) / (2 * noise * noise) - readSays (reading z size (sightPixels s))) units)) <$> expectSizes expect <*> predicted
    unused used = length blobs - length used
    separate =
        [ ranked (fmap (fmap snd) chosen) (unused used)
        | chosen <- traverse (Nothing :) (fmap (map Just) options)
        , let seen = catMaybes (toList chosen)
        , length seen >= 2
        , let used = concatMap fst seen
        , distinct used
        ]
    merged =
        [ ranked (place (Just si) (Just sj) (Just third)) (unused (i : thirdUsed))
        | (qi, qj, ei, ej, otherOptions, place) <- pairs
        , distance2 qi qj <= reach ei ej ^ (2 :: Int)
        , (i, b) <- take 2 (sortOn (\(_, b) -> distance2 (between qi qj ei ej) (blobX b, blobY b)) indexed)
        , let (si, sj) = splitMerged b qi qj ei ej
        , (thirdUsed, third) <- take 1 [o | o@(us, _) <- otherOptions, i `notElem` us]
        ]
    Three qL qR qT = predicted
    Three eL eR eT = expected
    Three oL oR oT = options
    pairs =
        [ (qL, qR, eL, eR, oT, \a b c -> Three a b c)
        , (qL, qT, eL, eT, oR, \a b c -> Three a c b)
        , (qR, qT, eR, eT, oL, \a b c -> Three c a b)
        ]
    between (ui, vi) (uj, vj) ei ej =
        let f = share ei ej in (f * ui + (1 - f) * uj, f * vi + (1 - f) * vj)
    -- A way of seeing the reflectors as it ranks: what the frame says of
    -- it with the places expected all shifted as far as, on average,
    -- the reflectors seen are from them, and what the shift costs.
    ranked sights left =
        let offsets = [(u - pu, v - pv) | ((pu, pv), Just s) <- toList ((,) <$> predicted <*> sights), let (u, v) = sightAt s]
            n = fromIntegral (max 1 (length offsets))
            shift@(du, dv) = (sum (map fst offsets) / n, sum (map snd offsets) / n)
            back = fmap (fmap (\s -> let (u, v) = sightAt s in s{sightAt = (u - du, v - dv)})) sights
         in Match sights left (score term (back, shift)) (findings term (back, shift))
    term = about fst (whatShows expect) <> about snd (shifting shiftSpread)

-- | How likely the clip's whole picture is shifted so far, across and
-- down, from where it was expected, of the given spread: matching's own
-- term (see Note [Matching blobs]).
shifting :: Double -> Term (Double, Double)
shifting spread = gaussian (Name "shift" "the clip isn't where it was expected") "px" spread (\(du, dv) -> sqrt (du * du + dv * dv))

-- | Up to how many blobs, heaviest first, are considered.
maxBlobs :: Int
maxBlobs = 8

distinct :: [Int] -> Bool
distinct (x : xs) = x `notElem` xs && distinct xs
distinct [] = True

distance2 :: (Double, Double) -> (Double, Double) -> Double
distance2 (a, b) (c, d) = (a - c) ^ (2 :: Int) + (b - d) ^ (2 :: Int)

pixels :: Blob -> Double
pixels = fromIntegral . blobPoints

-- | Two blobs that may be pieces of one reflector: each one's centre
-- inside the reflector they would make, a disc of their combined area
-- about the centre of both.
together :: Blob -> Blob -> Bool
together a b = all inside [a, b]
  where
    inside piece = distance2 (blobX piece, blobY piece) (sightAt (rejoin a b)) <= (pixels a + pixels b) / pi

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
