{-# OPTIONS_GHC -Wno-orphans #-}

-- | How to make up any value of trackkat's types of which every value
-- means something, and how to make one simpler.
module Test.Instances () where

import Test.Tasty.QuickCheck

import TrackKat.Belief (Belief (..), unknown)
import TrackKat.Branch (Clarity (..))
import TrackKat.CommandLine (Start (..))
import TrackKat.Config (Binding, HeadView (..))
import TrackKat.Frame (Blob, Stripe (..), frameHeight, frameWidth)
import TrackKat.Idle (Use (..))
import TrackKat.Linear (V3)
import TrackKat.Match (Sight, Three)
import TrackKat.Protocol (Command (..))
import TrackKat.Response
import TrackKat.ServedGame (HeadTracking (..), ServedGame (..))
import TrackKat.Tracker.Outcome (Way (..))
import TrackKat.Updater (Looking (..))

import Test.Gen

-- | Up to a metre each way.
instance Arbitrary V3 where
    arbitrary = genV3 1000
    shrink = shrinkV3

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

instance Arbitrary Binding where
    arbitrary = genBinding
    shrink = shrinkBinding

-- | Commands with smaller bytes, and with fewer.
instance Arbitrary Command where
    arbitrary = oneof [Cmd1 <$> arbitrary, Cmd2 <$> arbitrary <*> arbitrary, Cmd4 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary]
    shrink (Cmd1 op) = Cmd1 <$> shrink op
    shrink (Cmd2 op v) = Cmd1 op : [Cmd2 op' v' | (op', v') <- shrink (op, v)]
    shrink (Cmd4 op a b c) = Cmd2 op a : [Cmd4 op' a' b' c' | (op', a', b', c') <- shrink (op, a, b, c)]

-- | A stripe within the camera's image and its fields' bits; simpler
-- nearer the image's corner, shorter and dimmer.
instance Arbitrary Stripe where
    arbitrary = do
        line <- chooseInt (0, frameHeight - 1)
        start <- chooseInt (0, frameWidth - 1)
        points <- chooseInt (1, frameWidth - start)
        Stripe line start points <$> chooseInt (0, 0xFFFFF) <*> chooseInt (0, 0x7FFF)
    shrink (Stripe line start points sumX brightness) =
        [ Stripe line' start' points' sumX' total'
        | (line', start', points', sumX', total') <- shrink (line, start, points, sumX, brightness)
        , line' >= 0 && start' >= 0 && points' >= 1 && sumX' >= 0 && total' >= 0
        ]

-- | Weights, as shares of their sum: some combinations may be ruled out.
-- Knowing nothing is the simplest belief of all.
instance Arbitrary Belief where
    arbitrary = shares <$> genWeights
    shrink b = [unknown | b /= unknown]

instance Arbitrary Start where
    arbitrary = elements [ByHand, ByGame, AtLogin]
    shrink = earlier [ByHand, ByGame, AtLogin]

instance Arbitrary Use where
    arbitrary = elements [Used, Unused]
    shrink = earlier [Used, Unused]

-- | A multiplier with a precision, over one of the views' ranges; simpler
-- with whole numbers, and no precision.
instance Arbitrary Multiplier where
    arbitrary = Multiplier <$> choose (0.1, 20) <*> choose (0, 100) <*> elements [yawRange, pitchRange, rollRange, leanRange]
    shrink e =
        [e{precision = 0} | precision e /= 0]
            ++ [e{factor = whole} | let whole = fromIntegral (ceiling (factor e) :: Int), whole /= factor e]

instance Arbitrary Curve where
    arbitrary = oneof [Expo <$> arbitrary, Points <$> genPoints]
    shrink (Points points) = map Points (shrinkPoints points)
    shrink (Expo e) = Points [(1, factor e)] : map Expo (shrink e)

-- | A response of any curves, smoothing and leaning; simpler with a curve
-- a straight line, no smoothing, or leaning with the screen.
instance Arbitrary Response where
    arbitrary =
        Response
            <$> oneof [pure NoSmoothing, tuned <$> choose (0, 100) <*> choose (0, 100)]
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> elements [LeanWithView, LeanWithScreen]
    shrink r =
        filter
            (/= r)
            [ r{smoothing = NoSmoothing}
            , r{yawCurve = straight}
            , r{pitchCurve = straight}
            , r{rollCurve = straight}
            , r{xCurve = straight}
            , r{yCurve = straight}
            , r{zCurve = straight}
            , r{lean = LeanWithScreen}
            ]
      where
        straight = Points [(1, 1)]

instance Arbitrary HeadView where
    arbitrary = elements [VirtualView, PhysicalView, CameraView]
    shrink = earlier [VirtualView, PhysicalView, CameraView]

-- | A game with any process and window Windows could give it; simpler
-- with less known of it, and smaller numbers.
instance Arbitrary ServedGame where
    arbitrary =
        ServedGame
            <$> chooseInt (1, maxBound)
            <*> oneof [pure Nothing, Just <$> arbitrary]
            <*> arbitrary
            <*> oneof [pure Nothing, Just <$> chooseInt (0, 65535)]
            <*> elements [TrackingStarted, TrackingStopped]
    shrink g =
        [g{servedStarted = Nothing} | servedStarted g /= Nothing]
            ++ [g{servedProfile = Nothing} | servedProfile g /= Nothing]
            ++ [g{servedProcess = p} | p <- shrink (servedProcess g), p >= 1]
            ++ [g{servedWindow = w} | w <- shrink (servedWindow g)]

instance Arbitrary Looking where
    arbitrary = elements [Due, Asked]
    shrink = earlier [Due, Asked]

instance Arbitrary Clarity where
    arbitrary = elements [Clear, Doubtful]
    shrink = earlier [Clear, Doubtful]

instance Arbitrary Way where
    arbitrary = elements [Followed, Refound, Resumed, FoundAfresh]
    shrink = earlier [Followed, Refound, Resumed, FoundAfresh]
