-- | The belief in which pose is the head's (see "TrackKat.Belief").
module Test.Belief (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Test.Gen (close, genWeights, shares, shrinkWithin)
import Test.Instances ()
import TrackKat.Belief

-- | A belief is four probabilities that add up to one, and everything that
-- happens to one keeps them so; sums are right to 1e-12.
tests :: TestTree
tests =
    testGroup
        "Belief"
        [ testProperty "knowing nothing, each pose is as likely the mirror image as not" $
            once $
                isBelief unknown .&&. trackedMirrorChance unknown === 0.5 .&&. referenceMirrorChance unknown === 0.5
        , testProperty "normalising makes a belief of any weights, none below the least chance's share of them" $
            \(Weights b) ->
                isBelief (normalise b) .&&. property (all (>= leastChance / (sum (chances b) + 4 * leastChance)) (chances (normalise b)))
        , testProperty "evidence against the mirror image leaves it no less likely than the least chance allows" $
            \b (Against logOdds) ->
                property (trackedMirrorChance (evidence logOdds b) >= 2 * leastChance / (1 + 4 * leastChance))
        , testProperty "normalising leaves a belief clear of the least chance as it is" $
            \(ClearBelief b) ->
                closeBelief (normalise b) b
        , testProperty "crossing keeps a belief a belief, and says nothing of the reference" $
            \b (Chance chance) ->
                isBelief (crossed chance b) .&&. close 1e-12 (referenceMirrorChance (crossed chance b)) (referenceMirrorChance b)
        , testProperty "crossing for certain switches the tracked pose, and with no chance does nothing" $
            \b ->
                crossed 1 b === swapTracked b .&&. crossed 0 b === b
        , testProperty "crossing as likely as not leaves the tracked pose as likely the mirror image as not" $
            \b ->
                close 1e-12 (trackedMirrorChance (crossed 0.5 b)) 0.5
        , testProperty "crossing twice is crossing once, with the chance of just one of the two" $
            \b (Chance p) (Chance q) ->
                closeBelief (crossed p (crossed q b)) (crossed (p * (1 - q) + q * (1 - p)) b)
        , testProperty "evidence keeps a belief a belief" $
            \b (LogOdds logOdds) ->
                isBelief (evidence logOdds b)
        , testProperty "more evidence for the mirror image makes it no less likely" $
            \b (LogOdds less) (For more) ->
                property (trackedMirrorChance (evidence less b) <= trackedMirrorChance (evidence (less + more) b) + 1e-12)
        , testProperty "evidence multiplies the tracked pose's odds, and says nothing of the reference either way" $
            \(ClearBelief b) (Slight logOdds) ->
                let after = evidence logOdds b
                    odds x = trackedMirrorChance x / (1 - trackedMirrorChance x)
                 in conjoin
                        [ close (1e-9 * odds after) (odds after) (exp logOdds * odds b)
                        , close 1e-9 (referenceMirrored after / bothTrue after) (referenceMirrored b / bothTrue b)
                        , close 1e-9 (bothMirrored after / trackedMirrored after) (bothMirrored b / trackedMirrored b)
                        ]
        , testProperty "evidence adds up" $
            \(ClearBelief b) (Slight first) (Slight second) ->
                closeBelief (evidence second (evidence first b)) (evidence (first + second) b)
        , testProperty "weighing keeps a belief a belief, and weighing all alike changes nothing" $
            \b (Weight alike) (Weight unalike) ->
                isBelief (weighed alike unalike b) .&&. closeBelief (weighed alike alike b) b
        , testProperty "weighing says nothing of which way round, among poses alike and among poses unalike" $
            \(ClearBelief b) (Weight alike) (Weight unalike) ->
                let after = weighed alike unalike b
                 in close 1e-9 (bothMirrored after / bothTrue after) (bothMirrored b / bothTrue b)
                        .&&. close 1e-9 (trackedMirrored after / referenceMirrored after) (trackedMirrored b / referenceMirrored b)
        , testProperty "recentered on, the tracked pose is as likely the mirror image as it was, and the reference with it" $
            \b ->
                conjoin
                    [ isBelief (agreed b)
                    , trackedMirrorChance (agreed b) === trackedMirrorChance b
                    , referenceMirrorChance (agreed b) === trackedMirrorChance b
                    ]
        , testProperty "switching a pose twice changes nothing, in either order, and leaves the other pose be" $
            \b ->
                conjoin
                    [ swapTracked (swapTracked b) === b
                    , swapReference (swapReference b) === b
                    , swapTracked (swapReference b) === swapReference (swapTracked b)
                    , referenceMirrorChance (swapTracked b) === referenceMirrorChance b
                    , trackedMirrorChance (swapReference b) === trackedMirrorChance b
                    , close 1e-12 (trackedMirrorChance (swapTracked b)) (1 - trackedMirrorChance b)
                    , close 1e-12 (referenceMirrorChance (swapReference b)) (1 - referenceMirrorChance b)
                    ]
        ]

chances :: Belief -> [Double]
chances (Belief tt tm mt mm) = [tt, tm, mt, mm]

-- | That a belief's probabilities are probabilities, and add up to one.
isBelief :: Belief -> Property
isBelief b = counterexample (show b ++ " isn't a belief") (all (\p -> p >= 0 && p <= 1 + 1e-12) (chances b)) .&&. close 1e-12 (sum (chances b)) 1

closeBelief :: Belief -> Belief -> Property
closeBelief a b = conjoin (zipWith (close 1e-12) (chances a) (chances b))

-- | Four weights, of which the first isn't nothing.
newtype Weights = Weights Belief
    deriving (Show)

instance Arbitrary Weights where
    arbitrary = Weights <$> genWeights
    shrink (Weights b) = [Weights unknown | b /= unknown]

-- | A belief with every combination clear of the least chance, by enough
-- that some evidence keeps it so.
newtype ClearBelief = ClearBelief Belief
    deriving (Show)

instance Arbitrary ClearBelief where
    arbitrary = ClearBelief . shares <$> (Belief <$> weight <*> weight <*> weight <*> weight)
      where
        weight = choose (0.5, 1)
    shrink (ClearBelief b) = [ClearBelief unknown | b /= unknown]

-- | The chance of something, from none to certain.
newtype Chance = Chance Double
    deriving (Show)

instance Arbitrary Chance where
    arbitrary = Chance <$> choose (0, 1)
    shrink (Chance p) = Chance <$> shrinkWithin (0, 1) 0 p

-- | Evidence for or against the mirror image, as log odds, up to all but
-- certain either way.
newtype LogOdds = LogOdds Double
    deriving (Show)

instance Arbitrary LogOdds where
    arbitrary = LogOdds <$> choose (-30, 30)
    shrink (LogOdds x) = LogOdds <$> shrinkWithin (-30, 30) 0 x

-- | Evidence against the mirror image, as log odds.
newtype Against = Against Double
    deriving (Show)

instance Arbitrary Against where
    arbitrary = Against <$> choose (-30, 0)
    shrink (Against x) = Against <$> shrinkWithin (-30, 0) 0 x

-- | Evidence for the mirror image, as log odds.
newtype For = For Double
    deriving (Show)

instance Arbitrary For where
    arbitrary = For <$> choose (0, 30)
    shrink (For x) = For <$> shrinkWithin (0, 30) 0 x

-- | Some evidence either way, as log odds: little enough to leave a clear
-- belief clear.
newtype Slight = Slight Double
    deriving (Show)

instance Arbitrary Slight where
    arbitrary = Slight <$> choose (-1.5, 1.5)
    shrink (Slight x) = Slight <$> shrinkWithin (-1.5, 1.5) 0 x

-- | How much a pose's likelihood counts for, from next to nothing to all
-- of it.
newtype Weight = Weight Double
    deriving (Show)

instance Arbitrary Weight where
    arbitrary = Weight <$> choose (0.001, 1)
    shrink (Weight w) = Weight <$> shrinkWithin (0.001, 1) 1 w
