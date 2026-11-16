{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | How to make up any value of trackkat's types of which every value
-- means something, and how to make one simpler.
module Test.Instances () where

import Test.Tasty.QuickCheck

import TrackKat.Belief (Belief, Chance, chance, inChance)
import TrackKat.Branch (Clarity (..))
import TrackKat.CommandLine (Start (..))
import TrackKat.Config (Binding, HeadView (..), KeyOrButton)
import TrackKat.Frame (Blob, Stripe (..), frameHeight, frameWidth)
import TrackKat.Idle (Use (..))
import TrackKat.Linear (V3)
import TrackKat.Match (Sight, Three)
import TrackKat.Protocol (Command (..))
import TrackKat.Response
import TrackKat.ServedGame (HeadTracking (..), ServedGame (..))
import TrackKat.Term (LogOdds (..), Score (..), impossible, possible)
import TrackKat.Tracker.Outcome (Way (..))
import TrackKat.Updater (Looking (..))
import TrackKat.World (Instant, Seconds, clockAt, inClockSeconds, inSeconds, seconds)

import Test.Gen

-- | Up to a metre each way.
instance Arbitrary V3 where
    arbitrary = genV3 1000
    shrink = shrinkV3

-- | A moment in a clock's first day; simplest as the clock begins.
instance Arbitrary Instant where
    arbitrary = clockAt <$> choose (0, 86400)
    shrink at = clockAt <$> shrinkWithin (0, 86400) 0 (inClockSeconds at)

-- | A length of time of up to a day; simplest none.
instance Arbitrary Seconds where
    arbitrary = seconds <$> choose (0, 86400)
    shrink d = seconds <$> shrinkWithin (0, 86400) 0 (inSeconds d)

instance (Arbitrary a) => Arbitrary (Three a) where
    arbitrary = genThree arbitrary
    shrink = shrinkThree shrink

-- | Of up to a lamp's 5000 pixels.
instance Arbitrary Blob where
    arbitrary = genBlob 5000
    shrink = shrinkBlob

-- | Of up to a lamp's 5000 pixels.
instance Arbitrary Sight where
    arbitrary = genSight 5000
    shrink = shrinkSight

instance Arbitrary KeyOrButton where
    arbitrary = genKeyOrButton
    shrink = shrinkKeyOrButton

instance Arbitrary Binding where
    arbitrary = genBinding
    shrink = shrinkBinding

-- | The one byte of nothing, and commands with fewer bytes, and with
-- smaller.
instance Arbitrary Command where
    arbitrary = oneof [Cmd1 <$> arbitrary, Cmd2 <$> arbitrary <*> arbitrary, Cmd4 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary]
    shrink = shrinkTo (Cmd1 0) steps
      where
        steps (Cmd1 op) = Cmd1 <$> shrink op
        steps (Cmd2 op v) = Cmd1 op : [Cmd2 op' v' | (op', v') <- shrink (op, v)]
        steps (Cmd4 op a b c) = Cmd1 op : Cmd2 op a : [Cmd4 op' a' b' c' | (op', a', b', c') <- shrink (op, a, b, c)]

-- | A stripe within the camera's image and its fields' bits; simplest one
-- dark point at the image's corner, and simpler nearer the corner,
-- shorter and dimmer.
instance Arbitrary Stripe where
    arbitrary = do
        line <- chooseInt (0, frameHeight - 1)
        start <- chooseInt (0, frameWidth - 1)
        points <- chooseInt (1, frameWidth - start)
        Stripe line start points <$> chooseInt (0, 0xFFFFF) <*> chooseInt (0, 0x7FFF)
    shrink =
        shrinkParts
            [ \s -> [s{stLine = line} | line <- shrinkFrom 0 (stLine s)]
            , \s -> [s{stStart = start} | start <- shrinkFrom 0 (stStart s)]
            , \s -> [s{stPoints = points} | points <- shrinkFrom 1 (stPoints s)]
            , \s -> [s{stSumX = sumX} | sumX <- shrinkFrom 0 (stSumX s)]
            , \s -> [s{stSum = brightness} | brightness <- shrinkFrom 0 (stSum s)]
            ]

-- | Weights, as shares of their sum: some combinations may be ruled out.
-- Knowing nothing is the simplest belief of all (see 'shrinkBelief').
instance Arbitrary Belief where
    arbitrary = shares <$> genWeights
    shrink = shrinkBelief

-- | Any chance, from none to certain; simplest none.
instance Arbitrary Chance where
    arbitrary = chance <$> choose (0, 1)
    shrink p = chance <$> shrinkWithin (0, 1) 0 (inChance p)

-- | Up to all but certain either way; simplest nothing either way.
instance Arbitrary LogOdds where
    arbitrary = LogOdds <$> choose (-30, 30)
    shrink (LogOdds x) = LogOdds <$> shrinkWithin (-30, 30) 0 x

-- | What can be, as 'Likely' has it, and now and then impossible;
-- simplest nothing against.
instance Arbitrary Score where
    arbitrary = frequency [(9, (\(Likely scored) -> scored) <$> arbitrary), (1, pure impossible)]
    shrink scored
        | scored == impossible = [possible]
        | otherwise = [simpler | Likely simpler <- shrink (Likely scored)]

instance Arbitrary Start where
    arbitrary = elements [ByHand, ByGame, AtLogin]
    shrink = earlier [ByHand, ByGame, AtLogin]

instance Arbitrary Use where
    arbitrary = elements [Used, Unused]
    shrink = earlier [Used, Unused]

-- | A multiplier with a precision, over one of the views' ranges;
-- simplest one for one with no precision over the first of the ranges,
-- and simpler with whole numbers.
instance Arbitrary Multiplier where
    arbitrary = Multiplier <$> choose (0.1, 20) <*> choose (0, 100) <*> elements viewRanges
    shrink =
        shrinkParts
            [ \e -> [e{precision = p} | p <- shrinkWithin (0, 100) 0 (precision e)]
            , \e -> [e{factor = k} | k <- shrinkWithin (0.1, 20) 1 (factor e)]
            , \e -> [e{viewRange = range} | range <- earlier viewRanges (viewRange e)]
            ]

viewRanges :: [Double]
viewRanges = [yawRange, pitchRange, rollRange, leanRange]

-- | Simplest a straight line of one for one; then, for a multiplier, the
-- straight line of it, and simpler multipliers.
instance Arbitrary Curve where
    arbitrary = oneof [Expo <$> arbitrary, Points <$> genPoints]
    shrink = shrinkTo (Points [(1, 1)]) steps
      where
        steps (Points points) = map Points (shrinkPoints points)
        steps (Expo e) = Points [(1, factor e)] : map Expo (shrink e)

instance Arbitrary Aiming where
    arbitrary = elements [NotAiming, Aiming]
    shrink = earlier [NotAiming, Aiming]

-- | Any smoothing; simplest none.
genSmoothing :: Gen Smoothing
genSmoothing = oneof [pure NoSmoothing, tuned <$> choose (0, 100) <*> choose (0, 100)]

-- | Aiming of any curves and smoothing; simplest with every curve a
-- straight line of one for one and no smoothing; and simpler with one of
-- them so, or a simpler curve.
instance Arbitrary Aim where
    arbitrary = Aim <$> genSmoothing <*> arbitrary <*> arbitrary <*> arbitrary
    shrink =
        shrinkParts
            [ \a -> [a{aimSmoothing = NoSmoothing} | aimSmoothing a /= NoSmoothing]
            , \a -> [a{aimYaw = curve} | curve <- shrink (aimYaw a)]
            , \a -> [a{aimPitch = curve} | curve <- shrink (aimPitch a)]
            , \a -> [a{aimRoll = curve} | curve <- shrink (aimRoll a)]
            ]

-- | A response of any curves, smoothing, leaning and aiming; simplest with
-- every curve a straight line of one for one, no smoothing, leaning with
-- the screen, and aiming at its simplest; and simpler with one of them so,
-- or a simpler curve.
instance Arbitrary Response where
    arbitrary =
        Response
            <$> genSmoothing
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> elements [LeanWithView, LeanWithScreen]
            <*> arbitrary
    shrink =
        shrinkParts
            [ \r -> [r{smoothing = NoSmoothing} | smoothing r /= NoSmoothing]
            , \r -> [r{yawCurve = curve} | curve <- shrink (yawCurve r)]
            , \r -> [r{pitchCurve = curve} | curve <- shrink (pitchCurve r)]
            , \r -> [r{rollCurve = curve} | curve <- shrink (rollCurve r)]
            , \r -> [r{xCurve = curve} | curve <- shrink (xCurve r)]
            , \r -> [r{yCurve = curve} | curve <- shrink (yCurve r)]
            , \r -> [r{zCurve = curve} | curve <- shrink (zCurve r)]
            , \r -> [r{lean = LeanWithScreen} | lean r /= LeanWithScreen]
            , \r -> [r{aim = aiming} | aiming <- shrink (aim r)]
            ]

instance Arbitrary HeadView where
    arbitrary = elements [VirtualView, PhysicalView, CameraView]
    shrink = earlier [VirtualView, PhysicalView, CameraView]

-- | A game with any process and window Windows could give it; simplest
-- the first process and window with no more known and tracking stopped,
-- and simpler with less known of it, and smaller numbers.
instance Arbitrary ServedGame where
    arbitrary =
        ServedGame
            <$> chooseInt (1, maxBound)
            <*> oneof [pure Nothing, Just <$> arbitrary]
            <*> arbitrary
            <*> oneof [pure Nothing, Just <$> chooseInt (0, 65535)]
            <*> elements [TrackingStarted, TrackingStopped]
    shrink =
        shrinkParts
            [ \g -> [g{servedStarted = started} | started <- shrink (servedStarted g)]
            , \g -> [g{servedProfile = Nothing} | servedProfile g /= Nothing] ++ [g{servedProfile = Just profile} | Just was <- [servedProfile g], profile <- shrinkFrom 0 was]
            , \g -> [g{servedProcess = process} | process <- shrinkFrom 1 (servedProcess g)]
            , \g -> [g{servedWindow = window} | window <- shrink (servedWindow g)]
            , \g -> [g{servedTracking = tracking} | tracking <- earlier [TrackingStopped, TrackingStarted] (servedTracking g)]
            ]

instance Arbitrary Looking where
    arbitrary = elements [Due, Asked]
    shrink = earlier [Due, Asked]

instance Arbitrary Clarity where
    arbitrary = elements [Clear, Doubtful]
    shrink = earlier [Clear, Doubtful]

instance Arbitrary Way where
    arbitrary = elements [Followed, Refound, Resumed, FoundAfresh]
    shrink = earlier [Followed, Refound, Resumed, FoundAfresh]
