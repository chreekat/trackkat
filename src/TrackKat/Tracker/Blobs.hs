-- | A frame's blobs: which there are, which are big enough to be
-- reflectors, which a mask hides, and what each was taken for.
module TrackKat.Tracker.Blobs (
    -- * The blobs in a frame
    findBlobs,
    minFaintPixels,
    maxBlobPixels,
    at,
    whole,

    -- * Those in view
    Shown (..),
    sortBlobs,

    -- * Choosing among them
    picks,

    -- * Those near a reflector
    reflectorReach,
    nearTo,
    distance2,
    BlobRole (..),
    roles,
) where

import Data.List (partition)

import TrackKat.Frame (Blob (..), Stripe, stripesToBlobs)
import TrackKat.Lights (Mask, inMask)
import TrackKat.Sight (Kind (..), Sight (..))

-- | The smallest and largest blobs kept at all, in pixels: smaller is
-- noise, and larger nothing a reflector lights, which at 250 pixels at
-- 950 mm is about 3600 as near as the camera sees.
minFaintPixels, maxBlobPixels :: Int
minFaintPixels = 6
maxBlobPixels = 5000

-- | The blobs in a frame that could be reflectors, heaviest first.
findBlobs :: [Stripe] -> [Blob]
findBlobs = stripesToBlobs minFaintPixels maxBlobPixels

-- | Where a blob is in the image.
at :: Blob -> (Double, Double)
at b = (blobX b, blobY b)

-- | A blob as one reflector's own.
whole :: Blob -> Maybe Sight
whole b = Just (Sight (at b) (fromIntegral (blobPoints b)) Whole)

-- | A frame's blobs as a mask sorts them, each kind in the order they
-- came.
data Shown = Shown
    { shownMask :: Mask
    , shownMasked :: [Blob]
    -- ^ those in the mask's areas
    , shownVisible :: [Blob]
    -- ^ the rest
    }

-- | Sort blobs by whether a mask hides them.
sortBlobs :: Mask -> [Blob] -> Shown
sortBlobs mask blobs = Shown mask masked visible
  where
    (masked, visible) = partition (inMask mask) blobs

-- | The ways to pick so many of a list, in order.
picks :: Int -> [a] -> [[a]]
picks 0 _ = [[]]
picks _ [] = []
picks n (x : xs) = map (x :) (picks (n - 1) xs) ++ picks n xs

-- | How near a reflector's place, in pixels, a blob is its own: three,
-- and the blob's diameter.
reflectorReach :: Blob -> Double
reflectorReach b = 3 + 2 * sqrt (fromIntegral (blobPoints b) / pi)

-- | Whether a place in the image is near enough a blob for the blob to be
-- a reflector there (see 'reflectorReach').
nearTo :: (Double, Double) -> Blob -> Bool
nearTo place b = distance2 place (at b) <= reflectorReach b ^ (2 :: Int)

-- | The square of how far apart two places in the image are, in pixels.
distance2 :: (Double, Double) -> (Double, Double) -> Double
distance2 (u, v) (x, y) = (u - x) ^ (2 :: Int) + (v - y) ^ (2 :: Int)

-- | What a blob was taken for.
data BlobRole
    = -- | one of the clip's reflectors, or part of one
      ReflectorBlob
    | -- | something else, ignored
      StrayBlob
    | -- | part of a still light, masked
      MaskedBlob
    deriving (Eq, Show)

-- | Each blob's role, given a mask and where the reflectors were seen: a
-- blob near a reflector, for its size, is one, or a piece of one. All
-- evaluated, so that keeping them holds on to nothing else.
roles :: Mask -> [Sight] -> [Blob] -> [(Blob, BlobRole)]
roles mask seen blobs = foldr (\(_, r) rest -> r `seq` rest) assigned assigned
  where
    assigned = [(b, role b) | b <- blobs]
    role b
        | any (\s -> nearTo (sightAt s) b) seen = ReflectorBlob
        | inMask mask b = MaskedBlob
        | otherwise = StrayBlob
