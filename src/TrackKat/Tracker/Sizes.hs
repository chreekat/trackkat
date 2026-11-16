-- | What is learnt of how big each reflector's blob is, and what a blob's
-- size then says of its being a reflector's.
--
-- See Note [A reflector's size].
module TrackKat.Tracker.Sizes (
    -- * What is learnt
    Size (..),
    unlearnt,
    sizeMean,
    sizeVariance,
    faded,
    learntFrom,
    sizeAt,
    sizeKnown,
    biggestAt,

    -- * What a blob's size says
    Reading (..),
    reading,
    sharedReading,
    shared,
    alikeness,
    strayDensity,
    pixelNoise,
) where

import TrackKat.Linear (norm)
import TrackKat.Pose (Pose (..))
import TrackKat.Sight (Three)
import TrackKat.Tracker.Blobs (maxBlobPixels, minFaintPixels)
import TrackKat.World (Seconds, World (..), inSeconds, world)

-- Note [A reflector's size]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
-- A blob's size is the only thing that says what it is, apart from where
-- it is. What is learnt of a reflector's size is one distribution, used
-- wherever a size is judged: a gaussian over the log of the pixels its
-- blob would light at 1 m (a blob's area goes with the inverse square of
-- distance).
--
-- It is kept as evidence ('Size'): the mean of the blobs taken for the
-- reflector's own, each weighed by how well it says the size
-- ('pixelNoise'), and the sum of those weights. A blob is judged against
-- it to within the world's 'sizeNoise'. The evidence fades with time, its weight falling by e in the
-- world's 'sizeMemory' ('faded'), so that what is learnt is of the last
-- so many seconds, seen or not. What a size is taken to be ('sizeMean',
-- 'sizeVariance') is the evidence combined with what was known before
-- any: the world's 'reflectorPixels', to within its
-- 'reflectorPixelsFactor'. With no evidence, or all of it faded, that is
-- all there is ('unlearnt'); a blob or two and the evidence outweighs it.
--
-- A blob of a reflector isn't always all of it: tape dims at a slant, and
-- a hand hides half. So the likelihood of a blob's pixels, given its
-- reflector, is a mixture ('reading'): mostly the gaussian above, and
-- with the world's 'partShowing' chance any fraction of the size expected,
-- each as likely as another. A blob much smaller than its reflector is
-- then unlikely by that chance and its fraction, not by a gaussian's
-- tail; a bigger one has only the tail. And what a blob teaches is
-- weighed by how likely it is all of the reflector, so that a reflector
-- dimming doesn't teach that it is small: what is learnt stays near the
-- most it has been lately, as long as it is remembered.
--
-- What a size says is always beside what it would say of a stray, a blob
-- that is none of the clip's: of any size the camera's blobs come in, as
-- likely one as another on the log scale ('strayDensity'). The log of the
-- ratio is the evidence ('readSays'): for the reflector where the size is
-- as learnt, against it where it is far off.
--
-- Before any is learnt, the three reflectors' sizes aren't known
-- separately so much as together: one clip's reflectors are alike, to
-- within the world's 'sameSizeFactor', however bright the clip and the
-- camera make them. Each reflector's distribution is judged by itself all
-- the same, and what their being alike adds or takes away is reckoned
-- once for the three ('shared', 'alikeness'): of each one's variance, what
-- is beyond the spread between a clip's reflectors is taken as common to
-- all, up to the least of the three. Once any is learnt well that is
-- nothing, and they are judged apart.

-- | What is learnt of how big a reflector's blob is: the evidence of the
-- blobs taken for its own, as the weighted mean of the logs of the pixels
-- each would light at 1 m, and the weight, which is one over the
-- variance the evidence alone leaves.
data Size = Size
    { sizeEvidence :: !Double
    , sizeWeight :: !Double
    }
    deriving (Eq, Show)

-- | A reflector not seen yet, or not for long: no evidence.
unlearnt :: Size
unlearnt = Size 0 0

-- | What is known of a reflector's size before any evidence: the log of
-- the world's 'reflectorPixels', and the variance of its
-- 'reflectorPixelsFactor'.
beforehand :: (Double, Double)
beforehand = (log (reflectorPixels world), log (reflectorPixelsFactor world) ^ (2 :: Int))

-- | The mean and the variance of the log of the pixels a reflector lights
-- at 1 m: its evidence, and what was known beforehand.
sizeMean, sizeVariance :: Size -> Double
sizeMean (Size m w) = let (m0, v0) = beforehand in (m0 / v0 + w * m) / (1 / v0 + w)
sizeVariance (Size _ w) = let (_, v0) = beforehand in 1 / (1 / v0 + w)

-- | What is learnt of a size, so long after: its evidence faded, weighing
-- less by e in the world's 'sizeMemory'.
faded :: Seconds -> Size -> Size
faded since (Size m w) = Size m (w * exp (negate (max 0 (inSeconds since)) / inSeconds (sizeMemory world)))

-- | The pixels a reflector of a size is expected to light, so many metres
-- away.
sizeAt :: Double -> Size -> Double
sizeAt z size = exp (sizeMean size) / (z * z)

-- | Whether a reflector's size is known to within the world's
-- 'sameSizeFactor': seen, and not long ago.
sizeKnown :: Size -> Bool
sizeKnown size = sizeVariance size < log (sameSizeFactor world) ^ (2 :: Int)

-- | The pixels the biggest reflector, of the given sizes, lights at a
-- pose, if there is one and any reflector's size is known.
biggestAt :: Three Size -> Maybe Pose -> Maybe Double
biggestAt sizes at = case (filter sizeKnown (foldr (:) [] sizes), at) of
    (known@(_ : _), Just pose) -> Just (maximum (map (sizeAt (norm (poseT pose) / 1000)) known))
    _ -> Nothing

-- | How well a blob of so many pixels says its reflector's size, as the
-- variance of the log of them: the world's 'sizeNoise', and the pixels on
-- the blob's edge, each as likely lit as not, about the square root of how
-- many there are of them. A small blob says less.
pixelNoise :: Double -> Double
pixelNoise pixels = sizeNoise world ^ (2 :: Int) + 2 * sqrt (pi * area) / (area * area)
  where
    area = max 1 pixels

-- | How likely a stray is any one size, on the log scale: as likely one as
-- another, of the sizes blobs are kept at.
strayDensity :: Double
strayDensity = 1 / log (fromIntegral maxBlobPixels / fromIntegral minFaintPixels)

-- | What a blob's size says of its being a reflector's own.
data Reading = Reading
    { readOff :: !Double
    -- ^ the log of its pixels over those expected
    , readVariance :: !Double
    -- ^ the variance that is judged by: the size's, and the world's
    -- 'sizeNoise'
    , readWhole :: !Double
    -- ^ the chance, were it the reflector's, that it is all of it
    , readSays :: !Double
    -- ^ the log of how much likelier its pixels are of the reflector than
    -- of a stray
    }
    deriving (Eq, Show)

-- | What a blob of so many pixels says of its being a reflector's own, of
-- the size given, so many metres away (see Note [A reflector's size]).
reading :: Double -> Size -> Double -> Reading
reading z size pixels = Reading off variance (exp (whole - both)) (both - log strayDensity)
  where
    off = log (max 1 pixels) + 2 * log z - sizeMean size
    variance = sizeVariance size + sizeNoise world ^ (2 :: Int)
    chance = partShowing world
    -- The logs of the two likelihoods, and of their sum: far off, either
    -- is too small to tell from nothing, and its log is not.
    whole = log (1 - chance) - off * off / (2 * variance) - log (2 * pi * variance) / 2
    -- Part of a reflector is any fraction of it, and bigger than all of
    -- it only by the noise.
    part = log chance - log (1 + sqrt (pi * noise / 2)) + (if off <= 0 then off else negate (off * off) / (2 * noise))
    noise = sizeNoise world ^ (2 :: Int)
    both = max whole part + log (1 + exp (negate (abs (whole - part))))

-- | What a blob of so many pixels says of its being two reflectors' at
-- once, of the sizes given, so many metres away. Two reflectors that run
-- together overlap by any amount, one as likely as another: the blob is
-- anything from the bigger alone to both side by side, and beyond those
-- only by its noise and theirs.
sharedReading :: Double -> Size -> Size -> Double -> Reading
sharedReading z a b pixels = Reading off variance 1 (density - log strayDensity)
  where
    together = exp (sizeMean a) + exp (sizeMean b)
    bigger = max (exp (sizeMean a)) (exp (sizeMean b))
    off = log (max 1 pixels) + 2 * log z - log together
    least = log (bigger / together)
    variance = max (sizeVariance a) (sizeVariance b) + sizeNoise world ^ (2 :: Int)
    between = 1 - bigger / together
    beyond by = negate (by * by) / (2 * variance)
    density
        | off > 0 = beyond off - log between
        | off >= least = off - log between
        | otherwise = least + beyond (off - least) - log between

-- | A size after a blob of so many pixels was taken for the reflector's
-- own, so many metres away: the blob added to its evidence, weighing as
-- its own noise allows and as it is likely all of the reflector.
learntFrom :: Double -> Double -> Size -> Size
learntFrom z pixels size@(Size m w)
    | weight <= 0 = size
    | otherwise = Size ((w * m + weight * seen) / (w + weight)) (w + weight)
  where
    weight = readWhole (reading z size pixels) / pixelNoise pixels
    seen = log (max 1 pixels) + 2 * log z

-- | How much of the variance of each of three sizes is common to them
-- (see Note [A reflector's size]): what the least known to the camera has
-- beyond the spread of one of a clip's reflectors beside another.
shared :: Three Size -> Double
shared sizes = max 0 (minimum (fmap sizeVariance sizes) - log (sameSizeFactor world) ^ (2 :: Int))

-- | What the sizes of a clip's reflectors being alike, or not, adds to
-- what each one's size says by itself: the log of how much likelier the
-- readings are together, so much of their variance being common to them,
-- than each apart.
alikeness :: Double -> [Reading] -> Double
alikeness common readings
    | common <= 0 = 0
    | otherwise = negate (sum (map narrowed counted) + log (1 + common * a) - common * b * b / (1 + common * a)) / 2
  where
    -- A reading counts as it is likely all of its reflector: one that
    -- isn't is taken as that much less sure of its size.
    counted = [(readOff r, max (sizeNoise world ^ (2 :: Int)) (readVariance r - common) / readWhole r) | r <- readings, readWhole r > 1e-6]
    a = sum [1 / own | (_, own) <- counted]
    b = sum [off / own | (off, own) <- counted]
    narrowed (off, own) = log (own / (own + common)) + off * off * (1 / own - 1 / (own + common))
