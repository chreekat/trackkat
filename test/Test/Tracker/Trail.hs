-- | The poses a frame can be followed from, and what a tracked frame
-- carries on from them (see "TrackKat.Tracker.Trail").
module Test.Tracker.Trail (tests) where

import Data.Maybe (isJust)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Test.Simulation (headset)
import TrackKat.Branch (newBranch)
import TrackKat.Linear (V3 (..), identity)
import TrackKat.Pose (Pose (..))
import TrackKat.Tracker.Outcome (Way (..))
import TrackKat.Tracker.Trail

-- | A pose predicts for a while and then doesn't, and nothing is carried
-- on from a pose that doesn't. The poses are told apart here by when they
-- were tracked.
tests :: TestTree
tests =
    testGroup
        "Trail"
        [ testProperty "a pose predicts until the prediction's time is up, and not after" $
            forAllShrink genTrail shrinkTrail $ \spec -> forAll genNow $ \now ->
                let predicts at = now - at <= predictionSeconds
                 in times (stillPredicting now (trail spec)) === Marks (keep predicts (lastAt spec)) (keep predicts (leftAt spec))
        , testProperty "the poses still predicting are still predicting" $
            forAllShrink genTrail shrinkTrail $ \spec -> forAll genNow $ \now ->
                times (stillPredicting now (stillPredicting now (trail spec))) === times (stillPredicting now (trail spec))
        , testProperty "a pose that predicts later predicted sooner" $
            forAllShrink genTrail shrinkTrail $ \spec -> forAll genNow $ \now -> forAll (choose (0, 1)) $ \later ->
                let Marks lastLater leftLater = times (stillPredicting (now + later) (trail spec))
                    Marks lastSooner leftSooner = times (stillPredicting now (trail spec))
                 in property ((isJust lastLater <= isJust lastSooner) && (isJust leftLater <= isJust leftSooner))
        , testProperty "a pose found afresh has been tracked since now, and leaves behind the last pose, unless one was left before it" $
            forAllShrink genTrail shrinkTrail $ \spec -> forAll genNow $ \now ->
                let carried = carryOn FoundAfresh now belief (trail spec)
                 in carriedSince carried === now
                        .&&. leftTime carried === (if isJust (leftAt spec) then leftAt spec else lastAt spec)
        , testProperty "a pose resumed has been tracked since the pose left behind was, and leaves nothing behind" $
            forAllShrink genTrail shrinkTrail $ \spec -> forAll genNow $ \now ->
                isJust (leftAt spec) ==>
                    let carried = carryOn Resumed now belief (trail spec)
                     in Just (carriedSince carried) === fmap (subtract tracked) (leftAt spec) .&&. leftTime carried === Nothing
        , testProperty "a pose that continues the last has been tracked since the last was, and leaves behind what was left" $
            forAllShrink genTrail shrinkTrail $ \spec -> forAll genNow $ \now -> forAll (elements [Followed, Refound]) $ \way ->
                let carried = carryOn way now belief (trail spec)
                 in carriedSince carried === maybe now (subtract tracked) (lastAt spec) .&&. leftTime carried === leftAt spec
        , testProperty "however a pose was come by, it has been tracked since no later than now, and what it leaves behind was there to leave" $
            forAllShrink genTrail shrinkTrail $ \spec -> forAll genNow $ \now -> forAll (elements [Followed, Refound, Resumed, FoundAfresh]) $ \way ->
                let carried = carryOn way now belief (stillPredicting now (trail spec))
                 in property (carriedSince carried <= now) .&&. property (leftTime carried `elem` [Nothing, leftAt spec, lastAt spec])
        ]
  where
    belief = newBranch headset
    keep ok at = if maybe False ok at then at else Nothing

-- | A trail as what to make it from: when the last pose was tracked, and
-- when one left behind was, if there are such; none after time 2.
data TrailSpec = TrailSpec {lastAt :: Maybe Double, leftAt :: Maybe Double}
    deriving (Show)

-- | When a trail's poses were tracked.
data Marks = Marks (Maybe Double) (Maybe Double)
    deriving (Eq, Show)

-- | For how long each pose of a trail had been tracked in every frame.
tracked :: Double
tracked = 3

trail :: TrailSpec -> Trail
trail (TrailSpec recent left) = Trail (at <$> recent) ((\time -> LeftBehind (at time) (newBranch headset)) <$> left)
  where
    at time = Recent (Pose identity (V3 0 0 700)) time time (time - tracked)

times :: Trail -> Marks
times (Trail recent left) = Marks (recentTime <$> recent) ((\(LeftBehind r _) -> recentTime r) <$> left)

leftTime :: Carried -> Maybe Double
leftTime carried = (\(LeftBehind r _) -> recentTime r) <$> carriedLeft carried

genTrail :: Gen TrailSpec
genTrail = TrailSpec <$> time <*> time
  where
    time = oneof [pure Nothing, Just <$> choose (0, 2)]

-- | Trails with fewer poses.
shrinkTrail :: TrailSpec -> [TrailSpec]
shrinkTrail (TrailSpec recent left) = [TrailSpec Nothing left | isJust recent] ++ [TrailSpec recent Nothing | isJust left]

-- | A time no sooner than any pose of a trail, up to a second after.
genNow :: Gen Double
genNow = choose (2, 3)
