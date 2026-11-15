-- | The belief in which pose is the head's (see "TrackKat.Belief").
module Test.Belief (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Test.Gen (Likely (..), close, genWeights, shares, shrinkBelief, shrinkTo, shrinkWithin)
import Test.Instances ()
import TrackKat.Belief
import TrackKat.Term (LogOdds (..))

-- | A belief is four probabilities that add up to one, and everything that
-- happens to one keeps them so; sums are right to 1e-12.
tests :: TestTree
tests =
    testGroup
        "Belief"
        [ testProperty "knowing nothing, each pose is as likely the mirror image as not" $
            once $
                isBelief unknown .&&. trackedMirrorChance unknown === chance 0.5 .&&. referenceMirrorChance unknown === chance 0.5
        , testProperty "normalising makes a belief of any weights, none below the least chance's share of them" $
            \(Weights b) ->
                isBelief (normalise b) .&&. property (all (>= least / (sum (chances b) + 4 * least)) (chances (normalise b)))
        , testProperty "evidence against the mirror image leaves it no less likely than the least chance allows" $
            \b logOdds ->
                property (tracked (evidence logOdds b) >= 2 * least / (1 + 4 * least))
        , testProperty "normalising leaves a belief clear of the least chance as it is" $
            \(ClearBelief b) ->
                closeBelief (normalise b) b
        , testProperty "crossing keeps a belief a belief, and says nothing of the reference" $
            \b crossing ->
                isBelief (crossed crossing b) .&&. close 1e-12 (reference (crossed crossing b)) (reference b)
        , testProperty "crossing for certain switches the tracked pose, and with no chance does nothing" $
            \b ->
                crossed (chance 1) b === swapTracked b .&&. crossed (chance 0) b === b
        , testProperty "crossing as likely as not leaves the tracked pose as likely the mirror image as not" $
            \b ->
                close 1e-12 (tracked (crossed (chance 0.5) b)) 0.5
        , testProperty "crossing twice is crossing once, with the chance of just one of the two" $
            \b first second ->
                let (p, q) = (inChance first, inChance second)
                 in closeBelief (crossed first (crossed second b)) (crossed (chance (p * (1 - q) + q * (1 - p))) b)
        , testProperty "evidence keeps a belief a belief" $
            \b logOdds ->
                isBelief (evidence logOdds b)
        , testProperty "more evidence for the mirror image makes it no less likely" $
            \b less (For more) ->
                property (tracked (evidence less b) <= tracked (evidence (less `both` more) b) + 1e-12)
        , testProperty "evidence multiplies the tracked pose's odds, and says nothing of the reference either way" $
            \(ClearBelief b) (Slight logOdds) ->
                let after = evidence logOdds b
                    odds x = tracked x / (1 - tracked x)
                 in conjoin
                        [ close (1e-9 * odds after) (odds after) (exp (oddsNats logOdds) * odds b)
                        , close 1e-9 (referenceMirrored after / bothTrue after) (referenceMirrored b / bothTrue b)
                        , close 1e-9 (bothMirrored after / trackedMirrored after) (bothMirrored b / trackedMirrored b)
                        ]
        , testProperty "evidence adds up" $
            \(ClearBelief b) (Slight first) (Slight second) ->
                closeBelief (evidence second (evidence first b)) (evidence (first `both` second) b)
        , testProperty "weighing keeps a belief a belief, and weighing all alike changes nothing" $
            \b (Likely alike) (Likely unalike) ->
                isBelief (weighed alike unalike b) .&&. closeBelief (weighed alike alike b) b
        , testProperty "weighing says nothing of which way round, among poses alike and among poses unalike" $
            \(ClearBelief b) (Likely alike) (Likely unalike) ->
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
                    , close 1e-12 (tracked (swapTracked b)) (1 - tracked b)
                    , close 1e-12 (reference (swapReference b)) (1 - reference b)
                    ]
        ]

-- | How likely a belief has the tracked pose, and the reference, the
-- mirror image, and the least chance, as bare numbers.
tracked, reference :: Belief -> Double
tracked = inChance . trackedMirrorChance
reference = inChance . referenceMirrorChance

least :: Double
least = inChance leastChance

-- | The log odds of two things together.
both :: LogOdds -> LogOdds -> LogOdds
both (LogOdds a) (LogOdds b) = LogOdds (a + b)

chances :: Belief -> [Double]
chances (Belief tt tm mt mm) = [tt, tm, mt, mm]

-- | That a belief's probabilities are probabilities, and add up to one.
isBelief :: Belief -> Property
isBelief b = counterexample (show b ++ " isn't a belief") (all (\p -> p >= 0 && p <= 1 + 1e-12) (chances b)) .&&. close 1e-12 (sum (chances b)) 1

closeBelief :: Belief -> Belief -> Property
closeBelief a b = conjoin (zipWith (close 1e-12) (chances a) (chances b))

-- | Four weights, of which the first isn't nothing. Simplest all alike,
-- and simpler with one of the others nothing.
newtype Weights = Weights Belief
    deriving (Show)

instance Arbitrary Weights where
    arbitrary = Weights <$> genWeights
    shrink (Weights b) = Weights <$> shrinkTo unknown (\w -> [w{referenceMirrored = 0}, w{trackedMirrored = 0}, w{bothMirrored = 0}]) b

-- | A belief with every combination clear of the least chance, by enough
-- that some evidence keeps it so.
newtype ClearBelief = ClearBelief Belief
    deriving (Show)

instance Arbitrary ClearBelief where
    arbitrary = ClearBelief . shares <$> (Belief <$> weight <*> weight <*> weight <*> weight)
      where
        weight = choose (0.5, 1)
    shrink (ClearBelief b) = ClearBelief <$> shrinkBelief b

-- | Evidence for the mirror image, as log odds.
newtype For = For LogOdds
    deriving (Show)

instance Arbitrary For where
    arbitrary = For . LogOdds <$> choose (0, 30)
    shrink (For x) = For . LogOdds <$> shrinkWithin (0, 30) 0 (oddsNats x)

-- | Some evidence either way, as log odds: little enough to leave a clear
-- belief clear.
newtype Slight = Slight LogOdds
    deriving (Show)

instance Arbitrary Slight where
    arbitrary = Slight . LogOdds <$> choose (-1.5, 1.5)
    shrink (Slight x) = Slight . LogOdds <$> shrinkWithin (-1.5, 1.5) 0 (oddsNats x)
