{-# OPTIONS_GHC -Wno-orphans #-}

-- | Arbitrary instances for the old tracker's own types, as
-- "Test.Instances" has them for the rest.
module Test.Old.Instances () where

import Test.Tasty.QuickCheck

import Test.Gen (earlier)
import TrackKat.Old.Tracker.Outcome (Way (..))

instance Arbitrary Way where
    arbitrary = elements [Followed, Refound, Resumed, FoundAfresh]
    shrink = earlier [Followed, Refound, Resumed, FoundAfresh]
