-- | Bright lights that stay put, such as a lamp or a fire behind the user:
-- learnt from the blobs that can't be the clip, and masked, so that they
-- don't stop the clip being found. See Note [Still lights].
module TrackKat.Lights (
    Masking (..),
    Lights,
    noLights,
    News (..),
    View (..),
    unseen,
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
import Data.Maybe (catMaybes, listToMaybe)
import Data.Ord (comparing)

import TrackKat.Frame (Blob (..))
import TrackKat.Match (Kind (..), Sight (..))

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
-- But a head can be held still for longer than that with the clip out of
-- sight, or with only one or two reflectors in view, as when looking down
-- at a phone: then nothing is tracked to say where the reflectors are. So
-- a blob no more than 'reflectorLikeFactor' times the size of the biggest
-- reflector seen lately (see 'sizeMemorySeconds') looks like a reflector,
-- and is learnt only once it has stayed put for 'stillHeadSeconds', longer
-- than a head is held still, unless the clip is tracked elsewhere with all
-- three reflectors in view. A fire's core is about twice a reflector's
-- size, so it is learnt as quickly as ever. Before any reflector has been
-- seen, nothing looks like one.
--
-- A clip that can't be found afresh, as when the fire is first lit, and is
-- held still is learnt along with the fire. So when fewer than three big
-- blobs are left unmasked, finding the clip afresh also tries the heaviest
-- blob in each of a few masked areas. Lights just learnt in which the clip
-- is found this way are forgotten at once, unannounced.
--
-- And a light that was a reflector after all shows it by moving with the
-- clip. A reflector tracked in a light's area, of about the light's size,
-- that didn't come into the area from outside it in the frame before, is
-- taken to be the light; once that reflector is tracked outside the area,
-- the light is forgotten at once, as having moved. A reflector passing in
-- front of a light comes into its area from outside, and a fire's core is
-- bigger than a reflector, so a head passing in front of the fire doesn't
-- make it forgotten.

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

-- | How much bigger than the biggest reflector seen lately a blob can be,
-- and still look like a reflector: a reflector grows as the head comes
-- nearer the camera.
reflectorLikeFactor :: Double
reflectorLikeFactor = 1.5

-- | Seconds over which the size of the biggest reflector seen fades.
sizeMemorySeconds :: Double
sizeMemorySeconds = 600

-- | Seconds a blob that looks like a reflector must stay put to be learnt
-- as a light, while the clip isn't tracked elsewhere.
stillHeadSeconds :: Double
stillHeadSeconds = 300

-- | How much bigger or smaller than a light a reflector tracked in its
-- area can be, and be taken for it.
sameSizeFactor :: Double
sameSizeFactor = 1.5

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
    , rHeld :: !(Maybe Int)
    -- ^ which reflector, numbered as in 'viewSights', was taken for it
    }

-- | Its lists are evaluated whole, so as not to hold on to earlier frames
-- through frames that don't look at them.
data Lights = Lights
    { lCandidates :: ![Candidate]
    , lRegions :: ![Region]
    , lClip :: !(Maybe (Double, [((Double, Double), Double)]))
    -- ^ when and where the reflectors were last seen
    , lBiggest :: !(Maybe (Double, Double))
    -- ^ when the biggest reflector lately was seen, and its pixels
    , lSights :: !(Double, [Maybe (Double, Double)])
    -- ^ when the last frame was, and where each reflector was tracked in
    -- it
    }

noLights :: Lights
noLights = Lights [] [] Nothing Nothing (0, [])

-- | A light learnt, or forgotten, at its place in the image; forgotten as
-- having gone, or as having moved.
data News = Learnt (Double, Double) | Forgotten (Double, Double) | Moved (Double, Double)
    deriving (Eq, Show)

-- | What a frame showed of the clip.
data View = View
    { viewSights :: [Maybe Sight]
    -- ^ where each reflector was seen, if the clip was tracked; none if
    -- it wasn't
    , viewDiscs :: [((Double, Double), Double)]
    -- ^ discs (centre, radius in pixels) around where the reflectors are,
    -- once the clip has been tracked long enough for lights to be learnt
    -- clear of it; none before
    }

-- | A frame where the clip wasn't tracked.
unseen :: View
unseen = View [] []

-- | Learn from a frame's blobs, seen at the given time in seconds, given
-- what it showed of the clip.
learnLights :: Double -> View -> [Blob] -> Lights -> (Lights, [News])
learnLights now (View sights reflectors) blobs ls =
    ( Lights (whole (filter (\c -> not (any (covers (cAt c)) (learnt ++ stayed))) waiting)) (whole (learnt ++ stayed)) clip biggest (now, whole (map (fmap sightAt) sights))
    , map (Learnt . rAt) learnt ++ map (Forgotten . rAt) gone ++ map (Moved . rAt) moved
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
    (stayed, moved) = partition (not . carriedOff) (map holder kept)
    watched =
        [ c
        | c <- foldl' watch (lCandidates ls) (reverse rest)
        , now - cLast c <= gapSeconds
        , not (any (\(p, r) -> distance (cAt c) p <= r) area)
        ]
    (ready, waiting) = partition (\c -> now - cFirst c >= learnSeconds && (not (reflectorLike c) || now - cFirst c >= stillHeadSeconds || allTracked)) watched
    learnt = [Region (cAt c) (min (reachPixels + marginPixels) (max stillPixels (cSpread c) + marginPixels)) (cPixels c / cCount c) now Nothing | c <- ready]
    covers p r = distance p (rAt r) <= rRadius r
    -- The clip tracked long enough to learn clear of, all three reflectors
    -- in view.
    allTracked = not (null reflectors) && length (catMaybes sights) == 3
    -- The biggest reflector seen lately, its size fading with time, and
    -- the blobs that look like one.
    fading = [b * exp (negate (now - t) / sizeMemorySeconds) | Just (t, b) <- [lBiggest ls]]
    biggest = case [sightPixels s | not (null reflectors), Just s <- sights, sightKind s /= Merged] of
        [] -> lBiggest ls
        seen -> let b = maximum (seen ++ fading) in b `seq` Just (now, b)
    reflectorLike c = any (\b -> cPixels c / cCount c <= reflectorLikeFactor * b) fading
    -- A light is taken for a reflector tracked in its area, of about its
    -- size, that didn't come in from outside it; and is carried off by
    -- that reflector tracked outside it.
    previously = case lSights ls of
        (t, ps) | now - t <= gapSeconds -> ps
        _ -> []
    holder r = case rHeld r of
        Just _ -> r
        Nothing -> case listToMaybe [i | (i, Just s) <- zip [0 ..] sights, takenFor r i s] of
            Just i -> i `seq` r{rHeld = Just i}
            Nothing -> r
    takenFor r i s =
        covers (sightAt s) r
            && sightPixels s <= sameSizeFactor * rPixels r
            && sightPixels s * sameSizeFactor >= rPixels r
            && all (`covers` r) (seenAs i previously)
    carriedOff r = case rHeld r of
        Just i | Just s <- seenAs i sights -> not (covers (sightAt s) r)
        _ -> False
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

-- | Of what each reflector was seen as, that of the one numbered so.
seenAs :: Int -> [Maybe a] -> Maybe a
seenAs i xs = case drop i xs of
    Just x : _ -> Just x
    _ -> Nothing

at :: Blob -> (Double, Double)
at b = (blobX b, blobY b)

pixels :: Blob -> Double
pixels = fromIntegral . blobPoints

distance :: (Double, Double) -> (Double, Double) -> Double
distance (a, b) (c, d) = sqrt ((a - c) ^ (2 :: Int) + (b - d) ^ (2 :: Int))
