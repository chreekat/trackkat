-- | The ways of making out the clip near where a pose predicts it, and
-- for how long a pose is told that the frame doesn't fix (see
-- "TrackKat.Tracker.Follow").
module Test.Tracker.Follow (tests) where

import Data.Foldable (toList)
import Data.Maybe (catMaybes, isJust)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Test.Gen
import Test.Instances ()
import Test.Simulation (headset)
import Test.Tracker.Known
import TrackKat.Lights (noMask)
import TrackKat.Match (Kind (..), Sight (..), Three (..))
import TrackKat.Pose
import TrackKat.Term
import TrackKat.Tracker.Account
import TrackKat.Tracker.Blobs
import TrackKat.Tracker.Follow
import TrackKat.Tracker.Reflectors (Expect (..), whatShows)
import TrackKat.Tracker.Sizes (sizeAt)
import TrackKat.World (Seconds, inSeconds, seconds, times, (.+^))

-- | The ways tried near a prediction are few, each of blobs there are,
-- and the first of the clip where it was predicted is the clip; what a
-- frame could say for each at the most is no less than it says of the
-- pose solved; and a pose is told of the clip showing less only for so
-- long.
tests :: TestTree
tests =
    testGroup
        "Follow"
        [ testProperty "the ways tried near a prediction are no more than may be solved, each seeing two reflectors or more in blobs there are" $
            \(Apart30 placing) (Strays strays) ->
                let blobs = clipBlobs headset placing ++ strays
                    tried = followed (taught placing) aFrameOn (lastAt (placed placing)) (sortBlobs noMask blobs)
                 in property (length tried <= followTries)
                        .&&. conjoin
                            [ property (length seen >= 2) .&&. property (all (\s -> sightKind s /= Whole || any ((== sightAt s) . at) blobs) seen)
                            | (sights, _, _) <- tried
                            , let seen = catMaybes (toList sights)
                            ]
        , testProperty "the first way tried of the clip where the last pose has it sees each reflector as its own blob, and is solved to that pose" $
            \(Apart30 placing) ->
                case followed (taught placing) aFrameOn (lastAt (placed placing)) (sortBlobs noMask (clipBlobs headset placing)) of
                    (sights, _, Just c) : _ ->
                        conjoin
                            [ counterexample (show sights) (all (maybe False ((== Whole) . sightKind)) sights)
                            , candidateSights c === sights
                            , candidatePicking c === ByPrediction
                            , counterexample ("fit " ++ show (candidateFit c)) (candidateFit c < 0.5)
                            , counterexample ("turned " ++ show (poseChange pivot (placed placing) (candidatePose c))) (fst (poseChange pivot (placed placing) (candidatePose c)) < 1)
                            ]
                    tried -> counterexample (show (length tried) ++ " tried, or no pose: " ++ show [sights | (sights, _, _) <- take 1 tried]) False
        , testProperty "the most a frame could say for a way tried is no less than it says of the pose solved, to within two nats" $
            \(Apart30 placing) (Strays strays) ->
                let known = taught placing
                 in conjoin
                        [ counterexample (show (most, says)) (nats most + 2 >= says)
                        | (sights, most, Just c) <- followed known aFrameOn (lastAt (placed placing)) (sortBlobs noMask (clipBlobs headset placing ++ strays))
                        , let says = nats (score (whatShows (expecting known aFrameOn (candidatePose c))) sights)
                        ]
        , testProperty "reflectors each a blob of their own all show, and with one unseen, one is out" $
            \(Apart30 placing) ->
                let expect = expecting (taught placing) aFrameOn (placed placing)
                    Three l r top = asSeen expect
                 in showing expect (Three l r top) === AllShowing
                        .&&. conjoin [showing expect sights === OneOut | sights <- [Three Nothing r top, Three l Nothing top, Three l r Nothing]]
        , testProperty "two reflectors sharing a blob both show, or one is perhaps behind the other: never is one surely out" $
            \(Apart30 placing) (Fraction share) ->
                let expect = expecting (taught placing) aFrameOn (placed placing)
                    Three l r top = asSeen expect
                    shared = fmap (\s -> s{sightKind = Merged, sightPixels = (0.2 + share) * sightPixels s})
                 in property (showing expect (Three (shared l) (shared r) top) `elem` [AllShowing, OneBehind])
        , testProperty "the less of the clip shows, the shorter a pose is told of it" $
            once (property (showingSeconds AllShowing > showingSeconds OneBehind && showingSeconds OneBehind > showingSeconds OneOut && showingSeconds OneOut > seconds 0))
        , testProperty "a pose is told of every reflector a blob of its own however long since; of less, only as long as when all showed more lately, and never if they haven't" $
            \(Apart30 placing) (Since since) (Fraction sooner) (Lit lit) ->
                let expect = expecting (taught placing) aFrameOn (placed placing)
                    less = (\s shown -> if shown then s else Nothing) <$> asSeen expect <*> lit
                    may gap = accepts (partlySeen expect (lastTracked .+^ gap) (Just lastTracked))
                 in conjoin
                        [ property (may since (asSeen expect))
                        , property (not (may since less) || may (times sooner since) less)
                        , accepts (partlySeen expect lastTracked Nothing) less === all isJust less
                        ]
        ]

-- | Each reflector seen as a blob of its own, just where and as big as
-- expected.
asSeen :: Expect -> Three (Maybe Sight)
asSeen expect = (\place size -> Just (Sight place (sizeAt (expectMetres expect) size) Whole)) <$> expectAt expect <*> expectSizes expect

-- | Seconds since all three reflectors showed, up to four.
newtype Since = Since Seconds
    deriving (Show)

instance Arbitrary Since where
    arbitrary = Since . seconds <$> choose (0, 4)
    shrink (Since x) = Since . seconds <$> shrinkWithin (0, 4) 0 (inSeconds x)

-- | Which reflectors are lit: all of them, or all but one. Simplest all.
newtype Lit = Lit (Three Bool)
    deriving (Show)

instance Arbitrary Lit where
    arbitrary = Lit <$> elements lights
    shrink (Lit lit) = Lit <$> earlier lights lit

lights :: [Three Bool]
lights = [Three True True True, Three False True True, Three True False True, Three True True False]
