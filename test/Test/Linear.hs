-- | Vectors, turns and linear systems (see "TrackKat.Linear").
module Test.Linear (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Test.Gen
import TrackKat.Linear

-- Note [Tolerances of turns]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Products of turns and vectors of a size s are right to a few times the
-- unit roundoff, 1e-16, times s; properties of them allow 1e-12 s.
--
-- An angle taken from a turn ('rotationVector') is the arc cosine of a sum
-- of the matrix's diagonal, which loses half its digits near no turn at
-- all: an angle is right to about 1e-8 radians, and properties of them
-- allow 1e-7.
tests :: TestTree
tests =
    testGroup
        "Linear"
        [ testProperty "a turn's rows are unit vectors square to each other, right-handed" $
            forAllShrink (genTurn 180) shrinkTurn $ \w ->
                let r@(M33 a b c) = rodrigues w
                 in closeM 1e-12 (r `mulMM` transpose r) identity .&&. closeV 1e-12 (cross a b) c
        , testProperty "a turn's angle and axis are those it was made from, short of half a turn" $
            forAllShrink (genTurn 179) shrinkTurn $ \w ->
                closeV 1e-7 (rotationVector (rodrigues w)) w
        , testProperty "the turn back is the turn's transpose" $
            forAllShrink (genTurn 180) shrinkTurn $ \w ->
                rodrigues ((-1) *^ w) === transpose (rodrigues w)
        , testProperty "no turn is the identity" $
            rodrigues (V3 0 0 0) === identity .&&. rotationVector identity === V3 0 0 0
        , testProperty "turning keeps a vector's length" $
            forAllShrink (genTurn 180) shrinkTurn $ \w -> forAllShrink (genV3 1000) shrinkV3 $ \v ->
                close (1e-12 * 1000) (norm (mulMV (rodrigues w) v)) (norm v)
        , testProperty "a turn after another turns a vector as both do in order" $
            forAllShrink (genTurn 180) shrinkTurn $ \a -> forAllShrink (genTurn 180) shrinkTurn $ \b -> forAllShrink (genV3 1000) shrinkV3 $ \v ->
                closeV (1e-12 * 1000) (mulMV (rodrigues a `mulMM` rodrigues b) v) (mulMV (rodrigues a) (mulMV (rodrigues b) v))
        , testProperty "transposing twice changes nothing" $
            forAllShrink (genTurn 180) shrinkTurn $
                \w -> transpose (transpose (rodrigues w)) === rodrigues w
        , testProperty "a cross product is square to both vectors, and reversed by swapping them" $
            forAllShrink (genV3 1000) shrinkV3 $ \a -> forAllShrink (genV3 1000) shrinkV3 $ \b ->
                conjoin
                    [ close (1e-12 * 1000 ^ (3 :: Int)) (dot (cross a b) a) 0
                    , close (1e-12 * 1000 ^ (3 :: Int)) (dot (cross a b) b) 0
                    , cross a b === (-1) *^ cross b a
                    ]
        , testProperty "a normalized vector is a unit vector the same way, and the zero vector stays zero" $
            forAllShrink (genV3 1000) shrinkV3 $ \v ->
                if norm v == 0
                    then normalize v === v
                    else close 1e-12 (norm (normalize v)) 1 .&&. closeV (1e-12 * 1000) (norm v *^ normalize v) v
        , testProperty "the angle between vectors is the same either way round, and no more than half a turn" $
            forAllShrink (genV3 1000) shrinkV3 $ \a -> forAllShrink (genV3 1000) shrinkV3 $ \b ->
                angleBetween a b === angleBetween b a .&&. property (angleBetween a b >= 0 && angleBetween a b <= pi)
        , testProperty "a turn moves no vector by more than its angle" $
            forAllShrink (genTurn 180) shrinkTurn $ \w -> forAllShrink genDirection shrinkNothing $ \v ->
                property (angleBetween v (mulMV (rodrigues w) v) <= norm w + 1e-7)
        , testProperty "turning toward a direction goes all the way close the limit, and by the limit beyond it" $
            forAllShrink genApart shrinkNothing $ \(from, to) -> forAll (choose (0, pi)) $ \limit ->
                let turned = rotateToward limit from to
                    apart = angleBetween from to
                 in if apart <= limit
                        then turned === to
                        else close 1e-7 (angleBetween from turned) limit .&&. close 1e-7 (angleBetween turned to) (apart - limit)
        , testProperty "a linear system's solution is the vector its right-hand side was made from" $
            forAllShrink genSystem shrinkSystem $ \(a, x) ->
                let b = [sum (zipWith (*) row x) | row <- a]
                 in case solveLinear a b of
                        Nothing -> counterexample "no solution" False
                        Just solved -> conjoin (zipWith (close 1e-9) solved x)
        , testProperty "a system with two equations the same has no one solution" $
            forAllShrink genSystem shrinkSystem $ \(a, x) ->
                length a >= 2 ==> solveLinear (take 1 a ++ take 1 a ++ drop 2 a) x === Nothing
        ]

-- | Two unit vectors neither the same way nor opposite ways, where the way
-- round from one to the other is no way in particular.
genApart :: Gen (V3, V3)
genApart = do
    from <- genDirection
    axis <- (normalize . cross from <$> genDirection) `suchThat` ((> 0.5) . norm)
    angle <- choose (0.01, pi - 0.01)
    pure (from, mulMV (rodrigues (angle *^ axis)) from)

-- | A system of up to six equations whose matrix has, in each row, a
-- diagonal entry at least twice the sum of the others' sizes, so that its
-- solution is found to nearly full precision; and a solution for it.
genSystem :: Gen ([[Double]], [Double])
genSystem = do
    n <- chooseInt (1, 6)
    rows <- vectorOf n (vectorOf n (choose (-1, 1)))
    signs <- vectorOf n (elements [-1, 1])
    x <- vectorOf n (choose (-100, 100))
    pure ([[if i == j then s * (2 * sum (map abs row) + 1) else v | (j, v) <- zip [0 :: Int ..] row] | (i, row, s) <- zip3 [0 ..] rows signs], x)

-- | Systems of fewer equations: the last equation and unknown dropped,
-- which leaves the diagonal as heavy.
shrinkSystem :: ([[Double]], [Double]) -> [([[Double]], [Double])]
shrinkSystem (a, x) = [(map init (init a), init x) | length a > 1]
