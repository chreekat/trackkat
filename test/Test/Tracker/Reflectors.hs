-- | What a frame shows of the clip, given where its reflectors are taken
-- to be (see "TrackKat.Tracker.Reflectors").
module Test.Tracker.Reflectors (tests) where

import Data.Foldable (toList)
import Data.Maybe (isJust)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Test.Gen
import Test.Instances ()
import Test.Tracker.Known
import TrackKat.Match (Kind (..), Sight (..), Three (..))
import TrackKat.Pose (Reflector (..))
import TrackKat.Term
import TrackKat.Tracker.Account (expecting)
import TrackKat.Tracker.Presence
import TrackKat.Tracker.Reflectors
import TrackKat.Tracker.Sizes (sizeAt)
import TrackKat.World (World (..), inPixels, world)

-- | What a frame says for the reflectors seen some way is a sum of named
-- terms, each greatest where the frame is as expected: so the clip seen
-- just as expected says the most a frame can, and saying so doesn't go by
-- which reflector is called which.
tests :: TestTree
tests =
    testGroup
        "Reflectors"
        [ testProperty "what a frame says is what each of its terms found, and nothing else" $
            \(Apart30 placing) (Seeing sights) ->
                let term = whatShows (expected placing)
                 in close 1e-9 (nats (score term sights)) (sum (map (nats . findingScore) (findings term sights)))
        , testProperty "every reflector just where and as big as expected says the most a frame can: one moved, resized or unseen says no more" $
            \(Apart30 placing) (Seeing sights) ->
                let expect = expected placing
                    says = nats . score (whatShows expect)
                 in counterexample (show (says (asExpected expect), says sights)) (says sights <= says (asExpected expect) + 1e-9)
        , testProperty "a blob just where its reflector should be says what such a blob does, less the further off, and against it some way off" $
            \(Apart30 placing) (Off near) (Off further) ->
                let expect = expected placing
                    Three (u, v) _ _ = expectAt expect
                    moved by = (\s -> s{sightAt = (u + by, v)}) <$> pick LeftReflector (asExpected expect)
                    place by = sum [nats (findingScore f) | f <- findings (whatShows expect) (Three (moved by) Nothing Nothing), brief (findingName f) == "left place"]
                 in conjoin
                        [ close 1e-9 (place 0) blobGain
                        , property (place (near + further) <= place near + 1e-9 && place near <= place 0 + 1e-9)
                        , property (place (6 * inPixels (centroidNoise world)) < 0)
                        ]
        , testProperty "what a frame says doesn't go by which reflector is called which: two swapped, in what is expected and what is seen, say the same" $
            \(Apart30 placing) (Seeing sights) ->
                let expect = expected placing
                    swap (Three a b c) = Three b a c
                    swapped = expect{expectAt = swap (expectAt expect), expectSizes = swap (expectSizes expect), expectShowed = (\(showed, since) -> (swap showed, since)) <$> expectShowed expect}
                    sharing = length [() | Just s <- toList sights, sightKind s == Merged]
                 in sharing < 3 ==> close 1e-9 (nats (score (whatShows swapped) (swap sights))) (nats (score (whatShows expect) sights))
        , testProperty "a reflector unseen says only how likely it is out of sight: as it comes and goes, given how it showed and how long ago" $
            \(Apart30 placing) showed (FrameGap since) ->
                let expect = (expected placing){expectShowed = Just (showed, since)}
                    found = findings (whatShows expect) (pure Nothing)
                 in map (brief . findingName) found === ["left shows", "right shows", "top shows", "alike"]
                        .&&. conjoin
                            [ close 1e-12 (nats (findingScore f)) (passing reflectorComings since was Gone)
                            | (f, was) <- zip found (toList showed)
                            ]
        , testProperty "with nothing known of how the reflectors showed, each is as likely in sight as it usually is" $
            \(Apart30 placing) ->
                let expect = (expected placing){expectShowed = Nothing}
                    showing sights = [nats (findingScore f) | f <- findings (whatShows expect) sights, brief (findingName f) == "top shows"]
                 in showing (asExpected expect) === [usually reflectorComings Present] .&&. showing (pure Nothing) === [usually reflectorComings Gone]
        , testProperty "the most a frame could say for the reflectors is no less than it says, wherever they are expected" $
            \(Apart30 placing) (Seeing sights) ->
                let expect = expected placing
                 in property (nats (atBest expect sights) >= nats (score (whatShows expect) sights) - 1e-9)
        , testProperty "a reflector's place counts in full when it is a blob of its own as big as expected, less when dim, half when shared, and never for nothing" $
            \(Seen s) (Fraction dim) ->
                let full = s{sightKind = Whole}
                    weight = sightWeight (sightPixels s)
                 in conjoin
                        [ weight full === 1
                        , property (weight full{sightPixels = dim * sightPixels s} <= 1 && weight full{sightPixels = dim * sightPixels s} > 0)
                        , weight full{sightKind = Merged} === 0.5
                        , weight full{sightPixels = 2 * sightPixels s} === 1
                        ]
        , testProperty "a reflector shows just when it is seen" $
            \(Seeing sights) -> presences sights === fmap (\s -> if isJust s then Present else Gone) sights
        ]

-- | What is expected of the reflectors a frame after tracking the clip at
-- a pose, were it still there.
expected :: Placing -> Expect
expected placing = expecting (taught placing) aFrameOn (placed placing)

-- | Each reflector seen as a blob of its own, just where and as big as
-- expected.
asExpected :: Expect -> Three (Maybe Sight)
asExpected expect = (\place size -> Just (Sight place (sizeAt (expectMetres expect) size) Whole)) <$> expectAt expect <*> expectSizes expect

-- | How each reflector is seen: anywhere in the picture, as a blob of its
-- own or sharing one, of up to 500 pixels, or not at all. Simplest none
-- seen. (None as two blobs taken for one: that is two blobs accounted
-- for, which says more than one.)
newtype Seeing = Seeing (Three (Maybe Sight))
    deriving (Show)

instance Arbitrary Seeing where
    arbitrary = Seeing <$> genThree (oneof [pure Nothing, Just <$> genSight 500 `suchThat` ((/= Rejoined) . sightKind)])
    shrink (Seeing sights) = Seeing <$> shrinkThree (maybe [] (\s -> Nothing : map Just (filter ((/= Rejoined) . sightKind) (shrinkSight s)))) sights

-- | A sight of up to 500 pixels.
newtype Seen = Seen Sight
    deriving (Show)

instance Arbitrary Seen where
    arbitrary = Seen <$> genSight 500
    shrink (Seen s) = Seen <$> shrinkSight s

-- | How far off a blob is, in pixels, up to ten.
newtype Off = Off Double
    deriving (Show)

instance Arbitrary Off where
    arbitrary = Off <$> choose (0, 10)
    shrink (Off d) = Off <$> shrinkWithin (0, 10) 0 d
