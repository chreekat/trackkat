-- | Which of a frame's blobs are looked at, and what each is taken for
-- (see "TrackKat.Tracker.Blobs").
module Test.Tracker.Blobs (tests) where

import Data.List (isSubsequenceOf, sortOn)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Test.Gen
import TrackKat.Frame (Blob (..))
import TrackKat.Lights (noMask)
import TrackKat.Match (Kind (..), Sight (..))
import TrackKat.Tracker.Blobs

-- | Sorting blobs neither makes any up nor changes their order, and says
-- the same of them in whatever order they come.
tests :: TestTree
tests =
    testGroup
        "Blobs"
        [ testProperty "the big blobs are some of the blobs, in the order they came, each of a fair size and not tiny beside the biggest" $
            forAllShrink genBlobs shrinkBlobs $ \blobs ->
                let big = bigAmong blobs
                    biggest = maximum (0 : map blobPoints blobs)
                 in conjoin
                        [ property (big `isSubsequenceOf` blobs)
                        , property (all (\b -> blobPoints b >= minBlobPixels && 25 * blobPoints b >= 3 * biggest) big)
                        , property (all (\b -> b `elem` big || blobPoints b < minBlobPixels || 25 * blobPoints b < 3 * biggest) blobs)
                        ]
        , testProperty "the big among the big blobs are all of them" $
            forAllShrink genBlobs shrinkBlobs $
                \blobs -> bigAmong (bigAmong blobs) === bigAmong blobs
        , testProperty "which blobs are big doesn't go by the order they come in" $
            forAllShrink genBlobs shrinkBlobs $ \blobs -> forAll (shuffle blobs) $ \shuffled ->
                inOrder (bigAmong shuffled) === inOrder (bigAmong blobs)
        , testProperty "another blob no bigger than the biggest leaves the big blobs big, and a bigger one makes none big that wasn't" $
            forAllShrink genBlobs shrinkBlobs $ \blobs -> forAllShrink (genBlob 5000) shrinkBlob $ \another ->
                let before = bigAmong blobs
                    after = filter (/= another) (bigAmong (another : blobs))
                 in if blobPoints another <= maximum (0 : map blobPoints blobs)
                        then after === filter (/= another) before
                        else property (after `isSubsequenceOf` before)
        , testProperty "with nothing masked, every blob is in view, in the order they came" $
            forAllShrink genBlobs shrinkBlobs $ \blobs ->
                let shown = sortBlobs noMask blobs
                 in shownVisible shown === blobs .&&. shownMasked shown === []
        , testProperty "the ways to pick so many of a list are each so many of it in order, all different, the first ones first" $
            forAll (choose (0, 4)) $ \n -> forAll (choose (0, 7)) $ \size ->
                let from = [1 .. size] :: [Int]
                    ways = picks n from
                 in conjoin
                        [ length ways === product [size - n + 1 .. size] `div` product [1 .. n]
                        , property (all (\way -> length way == n && way `isSubsequenceOf` from) ways)
                        , property (and [a /= b | (i, a) <- zip [0 :: Int ..] ways, (j, b) <- zip [0 ..] ways, i < j])
                        , take 1 ways === [take n from | n <= size]
                        ]
        , testProperty "a blob is near its own place, and as near any place within three pixels and its diameter" $
            forAllShrink (genBlob 5000) shrinkBlob $ \b -> forAll (choose (0, 2)) $ \off -> forAll (choose (0, 2 * pi)) $ \way ->
                let far = off * reflectorReach b
                    place = (blobX b + far * cos way, blobY b + far * sin way)
                 in property (nearTo (at b) b) .&&. (abs (off - 1) > 1e-9 ==> nearTo place b === (off < 1))
        , testProperty "each blob is taken for one thing, in the order they came: a reflector just where one was seen near it, and with nothing masked a stray otherwise" $
            forAllShrink genBlobs shrinkBlobs $ \blobs -> forAllShrink (listOf (genSight 5000)) (shrinkList shrinkSight) $ \seen ->
                let taken = roles noMask seen blobs
                 in map fst taken === blobs
                        .&&. conjoin [role === (if any (\s -> nearTo (sightAt s) b) seen then ReflectorBlob else StrayBlob) | (b, role) <- taken]
        , testProperty "a blob seen as a reflector's own is taken for a reflector" $
            forAllShrink genBlobs shrinkBlobs $ \blobs ->
                conjoin [lookup b (roles noMask [s | Just s <- [whole b]] blobs) === Just ReflectorBlob | b <- blobs]
        , testProperty "a blob as a reflector's own is the blob's place and pixels, whole" $
            forAllShrink (genBlob 5000) shrinkBlob $ \b -> case whole b of
                Just (Sight place pixels Whole) -> place === at b .&&. pixels === fromIntegral (blobPoints b)
                other -> counterexample (show other) False
        ]

-- | Blobs in an order that doesn't go by the order they came in.
inOrder :: [Blob] -> [Blob]
inOrder = sortOn (\b -> (blobX b, blobY b, blobPoints b))

-- | A few blobs of sizes from a fragment's to a lamp's.
genBlobs :: Gen [Blob]
genBlobs = do
    n <- choose (0, 8)
    vectorOf n (oneof [genBlob 40, genBlob 400, genBlob 5000])

shrinkBlobs :: [Blob] -> [[Blob]]
shrinkBlobs = shrinkList shrinkBlob
