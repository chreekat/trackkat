-- | The belief in which pose is the head's (see "TrackKat.Belief").
module Test.Belief (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Test.Gen (close)
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
            forAllShrink genWeights shrinkBelief $ \b ->
                isBelief (normalise b) .&&. property (all (>= leastChance / (sum (chances b) + 4 * leastChance)) (chances (normalise b)))
        , testProperty "evidence against the mirror image leaves it no less likely than the least chance allows" $
            forAllShrink genBelief shrinkBelief $ \b -> forAll (choose (-30, 0)) $ \logOdds ->
                property (trackedMirrorChance (evidence logOdds b) >= 2 * leastChance / (1 + 4 * leastChance))
        , testProperty "normalising leaves a belief clear of the least chance as it is" $
            forAllShrink genClear shrinkBelief $
                \b -> closeBelief (normalise b) b
        , testProperty "crossing keeps a belief a belief, and says nothing of the reference" $
            forAllShrink genBelief shrinkBelief $ \b -> forAll genChance $ \chance ->
                isBelief (crossed chance b) .&&. close 1e-12 (referenceMirrorChance (crossed chance b)) (referenceMirrorChance b)
        , testProperty "crossing for certain switches the tracked pose, and with no chance does nothing" $
            forAllShrink genBelief shrinkBelief $
                \b -> crossed 1 b === swapTracked b .&&. crossed 0 b === b
        , testProperty "crossing as likely as not leaves the tracked pose as likely the mirror image as not" $
            forAllShrink genBelief shrinkBelief $
                \b -> close 1e-12 (trackedMirrorChance (crossed 0.5 b)) 0.5
        , testProperty "crossing twice is crossing once, with the chance of just one of the two" $
            forAllShrink genBelief shrinkBelief $ \b -> forAll genChance $ \p -> forAll genChance $ \q ->
                closeBelief (crossed p (crossed q b)) (crossed (p * (1 - q) + q * (1 - p)) b)
        , testProperty "evidence keeps a belief a belief" $
            forAllShrink genBelief shrinkBelief $
                \b -> forAll (choose (-30, 30)) $ \logOdds -> isBelief (evidence logOdds b)
        , testProperty "more evidence for the mirror image makes it no less likely" $
            forAllShrink genBelief shrinkBelief $ \b -> forAll (choose (-30, 30)) $ \less -> forAll (choose (0, 30)) $ \more ->
                property (trackedMirrorChance (evidence less b) <= trackedMirrorChance (evidence (less + more) b) + 1e-12)
        , testProperty "evidence multiplies the tracked pose's odds, and says nothing of the reference either way" $
            forAllShrink genClear shrinkBelief $ \b -> forAll (choose (-1.5, 1.5)) $ \logOdds ->
                let after = evidence logOdds b
                    odds x = trackedMirrorChance x / (1 - trackedMirrorChance x)
                 in conjoin
                        [ close (1e-9 * odds after) (odds after) (exp logOdds * odds b)
                        , close 1e-9 (referenceMirrored after / bothTrue after) (referenceMirrored b / bothTrue b)
                        , close 1e-9 (bothMirrored after / trackedMirrored after) (bothMirrored b / trackedMirrored b)
                        ]
        , testProperty "evidence adds up" $
            forAllShrink genClear shrinkBelief $ \b -> forAll (choose (-1.5, 1.5)) $ \first -> forAll (choose (-1.5, 1.5)) $ \second ->
                closeBelief (evidence second (evidence first b)) (evidence (first + second) b)
        , testProperty "weighing keeps a belief a belief, and weighing all alike changes nothing" $
            forAllShrink genBelief shrinkBelief $ \b -> forAll (choose (0.001, 1)) $ \alike -> forAll (choose (0.001, 1)) $ \unalike ->
                isBelief (weighed alike unalike b) .&&. closeBelief (weighed alike alike b) b
        , testProperty "weighing says nothing of which way round, among poses alike and among poses unalike" $
            forAllShrink genClear shrinkBelief $ \b -> forAll (choose (0.001, 1)) $ \alike -> forAll (choose (0.001, 1)) $ \unalike ->
                let after = weighed alike unalike b
                 in close 1e-9 (bothMirrored after / bothTrue after) (bothMirrored b / bothTrue b)
                        .&&. close 1e-9 (trackedMirrored after / referenceMirrored after) (trackedMirrored b / referenceMirrored b)
        , testProperty "recentered on, the tracked pose is as likely the mirror image as it was, and the reference with it" $
            forAllShrink genBelief shrinkBelief $ \b ->
                conjoin
                    [ isBelief (agreed b)
                    , trackedMirrorChance (agreed b) === trackedMirrorChance b
                    , referenceMirrorChance (agreed b) === trackedMirrorChance b
                    ]
        , testProperty "switching a pose twice changes nothing, in either order, and leaves the other pose be" $
            forAllShrink genBelief shrinkBelief $ \b ->
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
genWeights :: Gen Belief
genWeights = Belief <$> choose (0.01, 1) <*> weight <*> weight <*> weight
  where
    weight = oneof [pure 0, choose (0, 1)]

-- | A belief: weights, as shares of their sum. Some combinations may be
-- ruled out.
genBelief :: Gen Belief
genBelief = shares <$> genWeights

-- | A belief with every combination clear of the least chance, by enough
-- that some evidence keeps it so.
genClear :: Gen Belief
genClear = shares <$> (Belief <$> weight <*> weight <*> weight <*> weight)
  where
    weight = choose (0.5, 1)

shares :: Belief -> Belief
shares b = Belief (bothTrue b / whole) (referenceMirrored b / whole) (trackedMirrored b / whole) (bothMirrored b / whole)
  where
    whole = sum (chances b)

-- | Knowing nothing, which is the simplest belief of all.
shrinkBelief :: Belief -> [Belief]
shrinkBelief b = [unknown | b /= unknown]

genChance :: Gen Double
genChance = choose (0, 1)
