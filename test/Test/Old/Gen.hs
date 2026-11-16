-- | What is made up for properties of the old tracker alone, beside what
-- "Test.Gen" makes up for both.
module Test.Old.Gen (
    genSizes,
    shrinkSizes,
    Sizes (..),
    shrinks,
) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Test.Gen (genThree)
import Test.Old.Instances ()
import Test.Shrink (shrinksWell)
import TrackKat.Old.Match (Size (..), Three (..), noSize)
import TrackKat.Old.Tracker.Outcome (Way (..))

-- | The sizes the reflectors have had, in pixels at 1 m: none seen yet, or
-- each seen at up to the given size, its usual size from twice its latest
-- down to a twentieth of it, as when it has faded out of sight.
genSizes :: Double -> Gen (Three Size)
genSizes most = oneof [pure (pure noSize), genThree size]
  where
    size = do
        latest <- choose (1, most)
        Size latest . (latest *) <$> choose (0.05, 2)

-- | None seen yet, and each with its usual size its latest.
shrinkSizes :: Three Size -> [Three Size]
shrinkSizes sizes =
    [pure noSize | any ((> 0) . sizeUsual) sizes]
        ++ [settled | let settled = (\s -> s{sizeUsual = sizeLatest s}) <$> sizes, or ((/=) <$> (sizeUsual <$> sizes) <*> (sizeUsual <$> settled))]

-- | The sizes the reflectors have had, each seen at up to 3000 pixels at
-- 1 m (see 'genSizes').
newtype Sizes = Sizes (Three Size)
    deriving (Eq, Show)

instance Arbitrary Sizes where
    arbitrary = Sizes <$> genSizes 3000
    shrink (Sizes sizes) = Sizes <$> shrinkSizes sizes

-- | That these are made simpler as Note [Simplest first] in "Test.Gen"
-- says.
shrinks :: TestTree
shrinks =
    testGroup
        "Shrink"
        [ shrinksWell "the reflectors' sizes" (Sizes (pure noSize)) (\(Sizes sizes) -> sizes == pure noSize || all (\s -> sizeLatest s >= 1 && sizeLatest s <= 3000 && sizeUsual s >= 0.05 * sizeLatest s && sizeUsual s <= 2 * sizeLatest s) sizes)
        , shrinksWell "a way the clip was made out" Followed (const True)
        ]
