-- | Vectors, turns and linear systems (see "TrackKat.Linear").
module Test.Linear (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Test.Gen
import Test.Instances ()
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
            \(Turn w) ->
                let r@(M33 a b c) = rodrigues w
                 in closeM 1e-12 (r `mulMM` transpose r) identity .&&. closeV 1e-12 (cross a b) c
        , testProperty "a turn's angle and axis are those it was made from, short of half a turn" $
            \(ShortOfHalf w) ->
                closeV 1e-7 (rotationVector (rodrigues w)) w
        , testProperty "the turn back is the turn's transpose" $
            \(Turn w) ->
                rodrigues ((-1) *^ w) === transpose (rodrigues w)
        , testProperty "no turn is the identity" $
            rodrigues (V3 0 0 0) === identity .&&. rotationVector identity === V3 0 0 0
        , testProperty "turning keeps a vector's length" $
            \(Turn w) v ->
                close (1e-12 * 1000) (norm (mulMV (rodrigues w) v)) (norm v)
        , testProperty "a turn after another turns a vector as both do in order" $
            \(Turn a) (Turn b) v ->
                closeV (1e-12 * 1000) (mulMV (rodrigues a `mulMM` rodrigues b) v) (mulMV (rodrigues a) (mulMV (rodrigues b) v))
        , testProperty "transposing twice changes nothing" $
            \(Turn w) ->
                transpose (transpose (rodrigues w)) === rodrigues w
        , testProperty "a cross product is square to both vectors, and reversed by swapping them" $
            \a b ->
                conjoin
                    [ close (1e-12 * 1000 ^ (3 :: Int)) (dot (cross a b) a) 0
                    , close (1e-12 * 1000 ^ (3 :: Int)) (dot (cross a b) b) 0
                    , cross a b === (-1) *^ cross b a
                    ]
        , testProperty "a normalized vector is a unit vector the same way, and the zero vector stays zero" $
            \v ->
                if norm v == 0
                    then normalize v === v
                    else close 1e-12 (norm (normalize v)) 1 .&&. closeV (1e-12 * 1000) (norm v *^ normalize v) v
        , testProperty "the angle between vectors is the same either way round, and no more than half a turn" $
            \a b ->
                angleBetween a b === angleBetween b a .&&. property (angleBetween a b >= 0 && angleBetween a b <= pi)
        , testProperty "a turn moves no vector by more than its angle" $
            \(Turn w) (Direction v) ->
                property (angleBetween v (mulMV (rodrigues w) v) <= norm w + 1e-7)
        , testProperty "turning toward a direction goes all the way close the limit, and by the limit beyond it" $
            \(TwoWays from to) (Limit limit) ->
                let turned = rotateToward limit from to
                    apart = angleBetween from to
                 in if apart <= limit
                        then turned === to
                        else close 1e-7 (angleBetween from turned) limit .&&. close 1e-7 (angleBetween turned to) (apart - limit)
        , testProperty "a linear system's solution is the vector its right-hand side was made from" $
            \(Dominant a x) ->
                let b = [sum (zipWith (*) row x) | row <- a]
                 in case solveLinear a b of
                        Nothing -> counterexample "no solution" False
                        Just solved -> conjoin (zipWith (close 1e-9) solved x)
        , testProperty "a system with two equations the same has no one solution" $
            \(Dominant a x) ->
                length a >= 2 ==> solveLinear (take 1 a ++ take 1 a ++ drop 2 a) x === Nothing
        ]

-- | A turn short of half a turn, where its axis can still be told.
newtype ShortOfHalf = ShortOfHalf V3
    deriving (Show)

instance Arbitrary ShortOfHalf where
    arbitrary = ShortOfHalf <$> genTurn 179
    shrink (ShortOfHalf w) = ShortOfHalf <$> shrinkTurn w

-- | A unit vector.
newtype Direction = Direction V3
    deriving (Show)

instance Arbitrary Direction where
    arbitrary = Direction <$> genDirection
    shrink (Direction v) = Direction <$> shrinkDirection v

-- | Two unit vectors neither the same way nor opposite ways, where the way
-- round from one to the other is no way in particular. Simplest the same
-- angle apart, along the first axis and in the plane of the first two.
data TwoWays = TwoWays V3 V3
    deriving (Show)

instance Arbitrary TwoWays where
    arbitrary = do
        from <- genDirection
        axis <- (normalize . cross from <$> genDirection) `suchThat` ((> 0.5) . norm)
        angle <- choose (0.01, pi - 0.01)
        pure (TwoWays from (mulMV (rodrigues (angle *^ axis)) from))
    shrink (TwoWays from to) = [TwoWays from' to' | (from', to') /= (from, to)]
      where
        angle = angleBetween from to
        (from', to') = (V3 1 0 0, V3 (cos angle) (sin angle) 0)

-- | The most to turn by, in radians, up to half a turn.
newtype Limit = Limit Double
    deriving (Show)

instance Arbitrary Limit where
    arbitrary = Limit <$> choose (0, pi)
    shrink (Limit angle) = Limit <$> shrinkWithin (0, pi) 0 angle

-- | A system of up to six equations whose matrix has, in each row, a
-- diagonal entry at least twice the sum of the others' sizes, so that its
-- solution is found to nearly full precision; and a solution for it.
-- Simpler with the last equation and unknown dropped, which leaves the
-- diagonal as heavy.
data Dominant = Dominant [[Double]] [Double]
    deriving (Show)

instance Arbitrary Dominant where
    arbitrary = do
        n <- chooseInt (1, 6)
        rows <- vectorOf n (vectorOf n (choose (-1, 1)))
        signs <- vectorOf n (elements [-1, 1])
        x <- vectorOf n (choose (-100, 100))
        pure (Dominant [[if i == j then s * (2 * sum (map abs row) + 1) else v | (j, v) <- zip [0 :: Int ..] row] | (i, row, s) <- zip3 [0 ..] rows signs] x)
    shrink (Dominant a x) = [Dominant (map init (init a)) (init x) | length a > 1]
