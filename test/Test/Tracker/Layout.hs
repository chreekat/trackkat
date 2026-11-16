-- | The ways of making out the clip by the layout of a frame's blobs (see
-- "TrackKat.Tracker.Layout").
module Test.Tracker.Layout (tests) where

import Data.Foldable (toList)
import Data.List (permutations, sortOn)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Test.Gen
import Test.Instances ()
import Test.Simulation (headset)
import Test.Tracker.Known
import TrackKat.Frame (Blob (..))
import TrackKat.Lights (noMask)
import TrackKat.Match (Kind (..), Sight (..))
import TrackKat.Pose
import TrackKat.Term
import TrackKat.Tracker.Account
import TrackKat.Tracker.Blobs
import TrackKat.Tracker.Layout
import TrackKat.Tracker.Reflectors (whatShows)

-- | Three blobs are laid out as the clip at one pose, whatever order they
-- come in; the ways tried are threes of the blobs in view; and the most
-- that is reckoned for each, before its pose is solved, is no less than
-- it then scores.
tests :: TestTree
tests =
    testGroup
        "Layout"
        [ testProperty "three blobs are the clip at the same pose, or at none, in whatever order they come" $
            \(ReflectorBlobs three) ->
                let told blobs = (\(pose, fit, taken) -> (poseR pose, poseT pose, fit, inOrder (toList taken))) <$> asClip knowing blobs
                 in conjoin [told ordered === told (toList three) | ordered <- permutations (toList three)]
        , testProperty "the clip alone in view is laid out one way: its blobs, each a reflector's own, at a pose that puts the reflectors there" $
            \(Apart20 placing) ->
                let blobs = clipBlobs headset placing
                 in case laidOut knowing aFrameOn LeaveMasked (sortBlobs noMask blobs) of
                        [Laid sights _ _ (Just c) masked] ->
                            conjoin
                                [ counterexample ("fit " ++ show (candidateFit c)) (candidateFit c < 0.5)
                                , inOrder blobs === inOrder [roundAt s | Just s <- toList sights]
                                , property (all (maybe False ((== Whole) . sightKind)) sights)
                                , (candidateStrays c, candidateMasked c, candidatePicking c) === (0, 0, ByLayout)
                                , candidateSights c === sights
                                , masked === []
                                ]
                        ways -> counterexample (show (length ways) ++ " ways, or no pose") False
        , testProperty "the ways by layout are threes of the heaviest few blobs in view, each blob a reflector's own, the rest told as strays; none of fewer than three" $
            \(Blobs blobs) ->
                let ways = laidOut knowing aFrameOn LeaveMasked (sortBlobs noMask blobs)
                    searched = take clipSearch blobs
                 in property (length ways <= length (picks 3 searched))
                        .&&. (length blobs < 3 ==> null ways)
                        .&&. conjoin
                            [ conjoin
                                [ property (all (`elem` inOrder searched) (inOrder [roundAt s | Just s <- toList sights]))
                                , maybe (property True) (\c -> candidateStrays c === length blobs - 3) clip
                                ]
                            | Laid sights _ _ clip _ <- ways
                            ]
        , testProperty "the most reckoned for a way by layout is no less than the frame says of the pose solved, to within two nats; nor, with its room, than it scores come afresh" $
            \(Apart30 placing) (Strays strays) ->
                let known = taught placing
                 in conjoin
                        [ counterexample (show (most, says)) (nats most + 2 >= says)
                            .&&. counterexample (show (afresh, scored)) (nats afresh + 2 >= scored)
                        | Laid sights most afresh (Just c) _ <- laidOut known aFrameOn LeaveMasked (sortBlobs noMask (clipBlobs headset placing ++ strays))
                        , let says = nats (score (whatShows (expecting known aFrameOn (candidatePose c))) sights)
                              cameAfresh = last (judgeClip known aFrameOn (Origins Nothing Nothing) c)
                              scored = sum [nats (findingScore f) | f <- judgedFindings cameAfresh, brief (findingName f) `notElem` ["in view", "left"]]
                        , not (isImpossible (judgedScore cameAfresh))
                        ]
        , testProperty "making out the clip by layout wants three blobs, and says how many there are" $
            \(Blobs blobs) ->
                accepts threeBlobs (length blobs) === (length blobs >= 3)
        ]

-- | Blobs in an order that doesn't go by the order they came in.
inOrder :: [Blob] -> [(Double, Double, Int)]
inOrder = sortOn id . map (\b -> (blobX b, blobY b, blobPoints b))

-- | A sight of a reflector's own blob, as the blob's place and pixels.
roundAt :: Sight -> Blob
roundAt s = Blob (fst (sightAt s)) (snd (sightAt s)) (round (sightPixels s)) 0 0 0 0

-- | A few blobs of sizes from a fragment's to a lamp's, heaviest first as
-- a frame's come.
newtype Blobs = Blobs [Blob]
    deriving (Show)

instance Arbitrary Blobs where
    arbitrary = do
        n <- chooseInt (0, 6)
        Blobs . sortOn (negate . blobWeight) <$> vectorOf n (oneof [genBlob 40, genBlob 400, genBlob 5000])
    shrink (Blobs blobs) = Blobs . sortOn (negate . blobWeight) <$> shrinkList shrinkBlob blobs
