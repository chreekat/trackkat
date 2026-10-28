-- | Bright lights that stay put, such as a lamp or a fire behind the user:
-- learnt from the blobs that can't be the clip, and masked, so that they
-- don't stop the clip being found. See Note [Still lights].
module TrackKat.Lights (
    Masking (..),
    Lights,
    noLights,
    News (..),
    learnLights,
    unlearn,
    holding,
    Mask,
    noMask,
    activeMask,
    inMask,
    heaviestIn,
) where

import Data.List (minimumBy, partition)
import Data.Ord (comparing)

import TrackKat.Frame (Blob (..))

-- | Whether still lights are learnt and masked.
data Masking = MaskLights | NoMasking
    deriving (Eq, Show)

-- Note [Still lights]
-- ~~~~~~~~~~~~~~~~~~~
-- A lit fireplace shows as a big blob that barely moves, with flickering
-- fragments of flame coming and going around it. While the clip is
-- tracked, matching ignores it. But finding the clip afresh takes the big
-- blobs by their layout and needs exactly three, so a fire's blob stops
-- that altogether.
--
-- A blob whose centroid stays within 'stillPixels' of where it has been on
-- average, seen again at least every 'gapSeconds', is learnt as a light
-- after 'learnSeconds'. Smaller blobs within 'reachPixels', under half its
-- size (the fire's fragments), widen the area learnt, then and later. The
-- light's area is a disc around it, out to its farthest centroid seen and
-- a margin. Blobs inside are masked: ignored, except where the clip is
-- predicted to be (a head can pass in front of a light). A light unseen for
-- 'forgetSeconds' is forgotten.
--
-- Blobs that can be the clip are not learnt from: those near where the
-- reflectors are, or were last seen in the past 'clipMemorySeconds', once
-- the clip has been tracked for a moment (lights can look like the clip
-- for a frame or two). So a still head, its reflectors matched, is never
-- masked. A light learnt where something was tracked for just a moment
-- shows that something was the light, and it is no longer followed.
--
-- A clip that can't be found afresh, as when the fire is first lit, and is
-- held still is learnt along with the fire. So when fewer than three big
-- blobs are left unmasked, finding the clip afresh also tries the heaviest
-- blob in each of a few masked areas. Lights just learnt in which the clip
-- is found this way are forgotten at once, unannounced.

-- | How far, in pixels, a still light's centroid strays from its average.
-- A fire's core moves less than 2 pixels from frame to frame.
stillPixels :: Double
stillPixels = 4

-- | How far from a light, in pixels, its fragments show: a fire's flicker
-- within about 20 pixels of its core.
reachPixels :: Double
reachPixels = 25

-- | The margin, in pixels, of a light's area beyond the farthest centroid
-- seen.
marginPixels :: Double
marginPixels = 5

-- | Seconds a light must stay put to be learnt, how long it may vanish
-- meanwhile, and after how long unseen it is forgotten.
learnSeconds, gapSeconds, forgetSeconds :: Double
learnSeconds = 3
gapSeconds = 0.25
forgetSeconds = 45

-- | For how long, in seconds, where the reflectors were last seen is kept
-- clear of learning.
clipMemorySeconds :: Double
clipMemorySeconds = 10

-- | A blob being watched: perhaps a light.
data Candidate = Candidate
    { cSumX :: !Double
    , cSumY :: !Double
    , cCount :: !Double
    -- ^ sightings of its core, whose centroids sum to the above
    , cPixels :: !Double
    -- ^ their pixels, summed
    , cFirst :: !Double
    , cLast :: !Double
    -- ^ when its core was first and last seen
    , cSpread :: !Double
    -- ^ the farthest, in pixels, a core's or fragment's centroid has been
    -- from its average
    }

cAt :: Candidate -> (Double, Double)
cAt c = (cSumX c / cCount c, cSumY c / cCount c)

-- | A light learnt: the area it and its fragments light.
data Region = Region
    { rAt :: !(Double, Double)
    , rRadius :: !Double
    , rPixels :: !Double
    -- ^ its core's usual size
    , rLast :: !Double
    -- ^ when something was last seen in it
    }

-- | Its lists are evaluated whole, so as not to hold on to earlier frames
-- through frames that don't look at them.
data Lights = Lights
    { lCandidates :: ![Candidate]
    , lRegions :: ![Region]
    , lClip :: !(Maybe (Double, [((Double, Double), Double)]))
    -- ^ when and where the reflectors were last seen
    }

noLights :: Lights
noLights = Lights [] [] Nothing

-- | A light learnt, or forgotten, at its place in the image.
data News = Learnt (Double, Double) | Forgotten (Double, Double)
    deriving (Eq, Show)

-- | Learn from a frame's blobs, seen at the given time in seconds, given
-- discs (centre, radius in pixels) around where the reflectors are in it,
-- none if the clip wasn't seen.
learnLights :: Double -> [((Double, Double), Double)] -> [Blob] -> Lights -> (Lights, [News])
learnLights now reflectors blobs ls =
    ( Lights (whole (filter (\c -> not (any (covers (cAt c)) (learnt ++ kept))) waiting)) (whole (learnt ++ kept)) clip
    , map (Learnt . rAt) learnt ++ map (Forgotten . rAt) gone
    )
  where
    whole xs = foldr seq () xs `seq` xs
    clip = if null reflectors then lClip ls else Just (now, reflectors)
    area = case clip of
        Just (t, discs) | now - t <= clipMemorySeconds -> discs
        _ -> []
    eligible = [b | b <- blobs, not (any (\(p, r) -> distance (at b) p <= r) area)]
    (regions, rest) = foldl' refresh (lRegions ls, []) eligible
    (kept, gone) = partition (\r -> now - rLast r <= forgetSeconds) regions
    watched =
        [ c
        | c <- foldl' watch (lCandidates ls) (reverse rest)
        , now - cLast c <= gapSeconds
        , not (any (\(p, r) -> distance (cAt c) p <= r) area)
        ]
    (ready, waiting) = partition (\c -> now - cFirst c >= learnSeconds) watched
    learnt = [Region (cAt c) (min (reachPixels + marginPixels) (max stillPixels (cSpread c) + marginPixels)) (cPixels c / cCount c) now | c <- ready]
    covers p r = distance p (rAt r) <= rRadius r
    -- A blob in a light's area, or a fragment near it, shows it is still
    -- there; a fragment widens the area. Other blobs are left over.
    refresh (rs, left) b = case break (\r -> covers (at b) r || fragment r) rs of
        (before, r : after) ->
            let d = distance (at b) (rAt r)
                r' = r{rLast = now, rRadius = max (rRadius r) (min (reachPixels + marginPixels) (d + marginPixels))}
             in (before ++ r' : after, left)
        (_, []) -> (rs, b : left)
      where
        fragment r = distance (at b) (rAt r) <= reachPixels && pixels b * 2 <= rPixels r
    -- A blob is the next sighting of the nearest candidate not yet seen in
    -- this frame within 'stillPixels', or else a new candidate; it is a
    -- fragment of any bigger candidate within reach.
    watch cs b =
        let near = [c | c <- cs, cLast c < now, distance (at b) (cAt c) <= stillPixels]
            sighted c = c{cSumX = cSumX c + blobX b, cSumY = cSumY c + blobY b, cCount = cCount c + 1, cPixels = cPixels c + pixels b, cLast = now}
            core = case near of
                [] -> Nothing
                _ -> Just (cAt (minimumBy (comparing (distance (at b) . cAt)) near))
            update c
                | Just p <- core, cAt c == p = let c' = sighted c in c'{cSpread = max (cSpread c') (distance (at b) (cAt c'))}
                | distance (at b) (cAt c) <= reachPixels && pixels b * 2 <= cPixels c / cCount c =
                    c{cSpread = max (cSpread c) (distance (at b) (cAt c))}
                | otherwise = c
            updated = map update cs
         in case core of
                Just _ -> updated
                Nothing -> Candidate (blobX b) (blobY b) 1 (pixels b) now now 0 : updated

-- | Forget the lights centred at the given places whose areas hold any of
-- the other places given; the places of those forgotten.
unlearn :: [(Double, Double)] -> [(Double, Double)] -> Lights -> (Lights, [(Double, Double)])
unlearn centres places ls = (ls{lRegions = kept}, map rAt gone)
  where
    (gone, kept) = partition (holds centres places) (lRegions ls)

-- | Whether any light centred at the given places holds any of the other
-- places given.
holding :: [(Double, Double)] -> [(Double, Double)] -> Lights -> Bool
holding centres places = any (holds centres places) . lRegions

holds :: [(Double, Double)] -> [(Double, Double)] -> Region -> Bool
holds centres places r = rAt r `elem` centres && any (\p -> distance p (rAt r) <= rRadius r) places

-- | The areas of lights masked in a frame.
newtype Mask = Mask [Region]

noMask :: Mask
noMask = Mask []

-- | The lights' areas, but for any where a reflector is predicted to show,
-- or near enough that its blob could be there.
activeMask :: [(Double, Double)] -> Lights -> Mask
activeMask predicted ls = Mask [r | r <- lRegions ls, not (any (\p -> distance p (rAt r) <= rRadius r + nearPixels) predicted)]
  where
    nearPixels = 20

-- | Whether a blob's centroid is in a masked area.
inMask :: Mask -> Blob -> Bool
inMask (Mask rs) b = any (\r -> distance (at b) (rAt r) <= rRadius r) rs

-- | Of the given blobs, heaviest first, the heaviest in each masked area,
-- in the same order.
heaviestIn :: Mask -> [Blob] -> [Blob]
heaviestIn (Mask rs) bs = filter (`elem` heaviest) bs
  where
    heaviest = [b | r <- rs, b : _ <- [filter (\x -> distance (at x) (rAt r) <= rRadius r) bs]]

at :: Blob -> (Double, Double)
at b = (blobX b, blobY b)

pixels :: Blob -> Double
pixels = fromIntegral . blobPoints

distance :: (Double, Double) -> (Double, Double) -> Double
distance (a, b) (c, d) = sqrt ((a - c) ^ (2 :: Int) + (b - d) ^ (2 :: Int))
