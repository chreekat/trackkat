-- | The poses a frame can be followed from, and what a tracked frame
-- carries on from them (see "TrackKat.Old.Tracker.Trail").
module Test.Old.Tracker.Trail (tests) where

import Data.Maybe (isJust)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Test.Gen (Fraction (..), earlier, shrinkParts, shrinkWithin)
import Test.Instances ()
import Test.Old.Instances ()
import Test.Old.Simulation (headset)
import TrackKat.Branch (newBranch)
import TrackKat.Linear (V3 (..), identity)
import TrackKat.Old.Tracker.Outcome (Way (..))
import TrackKat.Old.Tracker.Trail
import TrackKat.Pose (Pose (..))
import TrackKat.World (Instant, clockAt, inClockSeconds, seconds, (.+^), (.-.))

-- | A pose predicts for a while and then doesn't, and nothing is carried
-- on from a pose that doesn't. The poses are told apart here by when they
-- were tracked.
tests :: TestTree
tests =
    testGroup
        "Trail"
        [ testProperty "a pose predicts until the prediction's time is up, and not after" $
            \spec (Now now) ->
                let predicts at = now .-. at <= predictionSeconds
                 in times (stillPredicting now (trail spec)) === Marks (keep predicts (lastAt spec)) (keep predicts (leftAt spec))
        , testProperty "the poses still predicting are still predicting" $
            \spec (Now now) ->
                times (stillPredicting now (stillPredicting now (trail spec))) === times (stillPredicting now (trail spec))
        , testProperty "a pose that predicts later predicted sooner" $
            \spec (Now now) (Fraction later) ->
                let Marks lastLater leftLater = times (stillPredicting (now .+^ seconds later) (trail spec))
                    Marks lastSooner leftSooner = times (stillPredicting now (trail spec))
                 in property ((isJust lastLater <= isJust lastSooner) && (isJust leftLater <= isJust leftSooner))
        , testProperty "a pose found afresh has been tracked since now, and leaves behind the last pose, unless one was left before it" $
            \spec (Now now) ->
                let carried = carryOn FoundAfresh now belief (trail spec)
                 in carriedSince carried === now
                        .&&. leftTime carried === (if isJust (leftAt spec) then leftAt spec else lastAt spec)
        , testProperty "a pose resumed has been tracked since the pose left behind was, and leaves nothing behind" $
            \spec (Now now) ->
                isJust (leftAt spec) ==>
                    let carried = carryOn Resumed now belief (trail spec)
                     in Just (carriedSince carried) === fmap trackedSince (leftAt spec) .&&. leftTime carried === Nothing
        , testProperty "a pose that continues the last has been tracked since the last was, and leaves behind what was left" $
            \spec (Now now) (Continuing way) ->
                let carried = carryOn way now belief (trail spec)
                 in carriedSince carried === maybe now trackedSince (lastAt spec) .&&. leftTime carried === leftAt spec
        , testProperty "however a pose was come by, it has been tracked since no later than now, and what it leaves behind was there to leave" $
            \spec (Now now) way ->
                let carried = carryOn way now belief (stillPredicting now (trail spec))
                 in property (carriedSince carried <= now) .&&. property (leftTime carried `elem` [Nothing, leftAt spec, lastAt spec])
        ]
  where
    belief = newBranch headset
    keep ok at = if maybe False ok at then at else Nothing

-- | A trail as what to make it from: when the last pose was tracked, and
-- when one left behind was, if there are such; none after time 2.
data TrailSpec = TrailSpec {lastAt :: Maybe Instant, leftAt :: Maybe Instant}
    deriving (Show)

-- | When a trail's poses were tracked.
data Marks = Marks (Maybe Instant) (Maybe Instant)
    deriving (Eq, Show)

-- | Since when a pose of a trail, tracked at the given time, had been
-- tracked in every frame: three seconds before.
trackedSince :: Instant -> Instant
trackedSince at = at .+^ seconds (-3)

trail :: TrailSpec -> Trail
trail (TrailSpec recent left) = Trail (at <$> recent) ((\time -> LeftBehind (at time) (newBranch headset)) <$> left)
  where
    at time = Recent (Pose identity (V3 0 0 700)) time time (trackedSince time)

times :: Trail -> Marks
times (Trail recent left) = Marks (recentTime <$> recent) ((\(LeftBehind r _) -> recentTime r) <$> left)

leftTime :: Carried -> Maybe Instant
leftTime carried = (\(LeftBehind r _) -> recentTime r) <$> carriedLeft carried

-- | Simplest with no poses, and simpler with fewer, and tracked at the
-- start.
instance Arbitrary TrailSpec where
    arbitrary = TrailSpec <$> time <*> time
      where
        time = oneof [pure Nothing, Just . clockAt <$> choose (0, 2)]
    shrink =
        shrinkParts
            [ \spec -> [spec{lastAt = at} | at <- sooner (lastAt spec)]
            , \spec -> [spec{leftAt = at} | at <- sooner (leftAt spec)]
            ]
      where
        sooner = maybe [] (\at -> Nothing : map (Just . clockAt) (shrinkWithin (0, 2) 0 (inClockSeconds at)))

-- | A time no sooner than any pose of a trail, up to a second after.
newtype Now = Now Instant
    deriving (Show)

instance Arbitrary Now where
    arbitrary = Now . clockAt <$> choose (2, 3)
    shrink (Now x) = Now . clockAt <$> shrinkWithin (2, 3) 2 (inClockSeconds x)

-- | A way a pose is come by that continues the last.
newtype Continuing = Continuing Way
    deriving (Show)

instance Arbitrary Continuing where
    arbitrary = Continuing <$> elements [Followed, Refound]
    shrink (Continuing way) = Continuing <$> earlier [Followed, Refound] way
