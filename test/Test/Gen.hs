-- | Generators, each with the shrinks that go with it, and comparisons to
-- within a tolerance, for properties of several subjects.
module Test.Gen (
    -- * Comparing
    close,
    closeV,
    closeM,

    -- * Vectors and turns
    genV3,
    shrinkV3,
    genDirection,
    genTurn,
    shrinkTurn,
    degreesOf,
) where

import Test.Tasty.QuickCheck

import TrackKat.Linear

-- | That two numbers are within a tolerance of each other.
close :: Double -> Double -> Double -> Property
close slack a b =
    counterexample (show a ++ " and " ++ show b ++ " are more than " ++ show slack ++ " apart") (abs (a - b) <= slack)

-- | That two vectors are within a tolerance of each other.
closeV :: Double -> V3 -> V3 -> Property
closeV slack a b =
    counterexample (show a ++ " and " ++ show b ++ " are more than " ++ show slack ++ " apart") (norm (a ^-^ b) <= slack)

-- | That each row of two matrices is within a tolerance of the other's.
closeM :: Double -> M33 -> M33 -> Property
closeM slack (M33 a b c) (M33 x y z) = conjoin [closeV slack a x, closeV slack b y, closeV slack c z]

-- | A vector with each component no bigger than the given size.
genV3 :: Double -> Gen V3
genV3 size = V3 <$> component <*> component <*> component
  where
    component = choose (negate size, size)

-- | Vectors with a component zeroed or rounded to a whole number.
shrinkV3 :: V3 -> [V3]
shrinkV3 (V3 x y z) = [V3 x' y z | x' <- simpler x] ++ [V3 x y' z | y' <- simpler y] ++ [V3 x y z' | z' <- simpler z]
  where
    simpler c = [0 | c /= 0] ++ [whole | let whole = fromIntegral (truncate c :: Int), whole /= c, whole /= 0]

-- | A unit vector.
genDirection :: Gen V3
genDirection = normalize <$> genV3 1 `suchThat` ((> 0.1) . norm)

-- | A turn, as the vector along its axis as long as its angle in radians
-- (see 'rodrigues'), of up to the given degrees.
genTurn :: Double -> Gen V3
genTurn most = (\axis angle -> (angle * pi / 180) *^ axis) <$> genDirection <*> choose (0, most)

-- | Turns about simpler axes, and by half as much and not at all.
shrinkTurn :: V3 -> [V3]
shrinkTurn w = [V3 0 0 0 | norm w > 0] ++ [0.5 *^ w | norm w > 1e-3] ++ [w' | w' <- shrinkV3 w, norm w' <= norm w]

-- | The angle of a turn, in degrees.
degreesOf :: V3 -> Double
degreesOf w = norm w * 180 / pi
