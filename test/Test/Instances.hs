{-# OPTIONS_GHC -Wno-orphans #-}

-- | How to make up any value of trackkat's types of which every value
-- means something, and how to make one simpler.
module Test.Instances () where

import Test.Tasty.QuickCheck

import TrackKat.Belief (Belief (..), unknown)
import TrackKat.CommandLine (Start (..))
import TrackKat.Config (Binding)
import TrackKat.Controls (Hotkey)
import TrackKat.Frame (Blob, Stripe (..), frameHeight, frameWidth)
import TrackKat.Idle (Use (..))
import TrackKat.Linear (V3)
import TrackKat.Match (Sight, Three)
import TrackKat.Protocol (Command (..))

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

instance Arbitrary Hotkey where
    arbitrary = arbitraryBoundedEnum
    shrink = shrinkBoundedEnum

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
