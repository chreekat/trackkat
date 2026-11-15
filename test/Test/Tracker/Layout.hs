-- | Making out the clip by the layout of the big blobs (see
-- "TrackKat.Tracker.Layout").
module Test.Tracker.Layout (tests) where

import Data.Foldable (toList)
import Data.List (permutations, sortOn)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Test.Gen
import Test.Simulation (headset)
import TrackKat.Frame (Blob (..))
import TrackKat.Lights (noMask)
import TrackKat.Linear
import TrackKat.Match (Sight (..), Size, Three (..), noSize)
import TrackKat.Pose
import TrackKat.Term
import TrackKat.Tracker.Blobs
import TrackKat.Tracker.Judge
import TrackKat.Tracker.Layout
import TrackKat.Tracker.Outcome
import TrackKat.Tracker.Sighting
import TrackKat.Tracker.Trail (Recent (..))
import TrackKat.World (clockAt, seconds)

-- | Whatever blobs are tried, by layout the clip is made only of three of
-- them that are like the clip, and what is told of a frame adds up to its
-- blobs. The clip itself, alone in view and facing the camera within 20
-- degrees, is made out where it is.
tests :: TestTree
tests =
    testGroup
        "Layout"
        [ testProperty "three blobs are the clip only at a pose where they are like it, and say what rejected them otherwise" $
            \(ReflectorBlobs three) (Sizes sizes) ->
                let known = knowing sizes Nothing
                    isClip = clipLike (knownReach known) sizes
                 in case asClip known (toList three) of
                        Right (found@(Found _ taken), fit, judged) ->
                            conjoin
                                [ property (accepts isClip found)
                                , judged === findings isClip found
                                , property (fit >= 0)
                                , inOrder (toList taken) === inOrder (toList three)
                                ]
                        Left (Unfit failed) -> property (not (null failed) && all (isImpossible . findingScore) failed)
                        Left NoPoseFits -> property True
                        Left NothingMatches -> counterexample "nothing matches" False
        , testProperty "three blobs are the clip, or aren't, in whatever order they come" $
            \(ReflectorBlobs three) (Sizes sizes) ->
                let told blobs = either (Left . show) (\(Found pose _, fit, _) -> Right (poseR pose, poseT pose, fit)) (asClip (knowing sizes Nothing) blobs)
                 in conjoin [told ordered === told (toList three) | ordered <- permutations (toList three)]
        , testProperty "the clip alone in view is found afresh where its blobs are, each a reflector, nothing else told" $
            \(Apart20 placing) ->
                let blobs = clipBlobs headset placing
                 in case findAfresh (knowing (pure noSize) Nothing) (sortBlobs noMask blobs) of
                        Right (Sighting pose fit sights strays masked judged, restored) ->
                            conjoin
                                [ counterexample ("fit " ++ show fit) (fit < 0.5)
                                , inOrder blobs === inOrder [roundAt s | Just s <- toList sights]
                                , (strays, masked, length restored) === (0, 0, 0)
                                , property (all (not . isImpossible . findingScore) judged)
                                , counterexample "reflectors aren't where the blobs are" $
                                    and ((\which sight -> maybe False (\s -> distance2 (seenAt defaultIntrinsics headset pose which) (sightAt s) < 1) sight) <$> Three LeftReflector RightReflector TopReflector <*> sights)
                                ]
                        Left why -> counterexample (explainWhy why) False
        , testProperty "found afresh, the clip is three of the blobs, and the rest are told as strays" $
            \(Blobs others) (Apart20 placing) (Sizes sizes) ->
                let blobs = sortOn (negate . blobWeight) (others ++ clipBlobs headset placing)
                 in case findAfresh (knowing sizes Nothing) (sortBlobs noMask blobs) of
                        Right (found, restored) ->
                            conjoin
                                [ sightingStrays found + 3 === length blobs
                                , sightingMasked found === 0
                                , length restored === 0
                                , property (all (\s -> any ((== sightAt s) . at) blobs) [s | Just s <- toList (sightingSights found)])
                                ]
                        Left (Why looking _) -> looking === Afresh
        , testProperty "with other than three big blobs in view, nothing masked and the clip not seen before, nothing is found, for there not being three" $
            \(Blobs blobs) ->
                let big = length (bigAmong blobs)
                 in big /= 3 ==> case findAfresh (knowing (pure noSize) Nothing) (sortBlobs noMask blobs) of
                        Left (Why Afresh (Unfit [Finding name _ (Just (Measure counted _ 3 _))])) -> brief name === "big blobs" .&&. counted === fromIntegral big
                        other -> counterexample (either explainWhy (const "found") other) False
        , testProperty "with a pose predicting, nothing is tried by layout without three big blobs, nor of more before the clip was seen" $
            \(Blobs blobs) ->
                length (bigAmong blobs) /= 3 ==> case findHere (knowing (pure noSize) (Just facing)) (clockAt 1) (lastAt facing) Nothing (sortBlobs noMask blobs) Nothing of
                    Nothing -> property True
                    Just tried -> counterexample (either show (show . layoutScore) tried) False
        , testProperty "by layout with a pose predicting, the clip is refound just when no impossible jump from it, and scored no better than nothing against it" $
            \(Apart20 placing) (Turned turn) (FrameGap gap) ->
                let was = turnedAbout pivot (rodrigues turn) (placed placing)
                    dt = seconds gap
                 in case findHere (knowing (pure noSize) (Just was)) (clockAt 1) (lastAt was){recentTime = clockAt (1 - gap)} Nothing (sortBlobs noMask (clipBlobs headset placing)) Nothing of
                        Just (Right found) ->
                            counterexample (show (layoutScore found, layoutWay found, renderFindings (layoutJump found), renderFindings (sightingJudged (layoutFound found)), afreshCost dt)) . conjoin $
                                [ (layoutWay found == Refound) === all (not . isImpossible . findingScore) (layoutJump found)
                                , property (layoutWay found `elem` [Refound, FoundAfresh])
                                , property (layoutScore found <= possible)
                                , property (layoutWay found /= FoundAfresh || nats (layoutScore found) <= 1e-9 - afreshCost dt)
                                , sightingStrays (layoutFound found) === 0
                                ]
                        Just (Left whyNot) -> counterexample (show whyNot) False
                        Nothing -> counterexample "not tried" False
        , testProperty "by layout, the clip where the pose predicting has it is refound" $
            \(Apart20 placing) ->
                let was = placed placing
                 in case findHere (knowing (pure noSize) (Just was)) (clockAt 1) (lastAt was){recentTime = clockAt (1 - 1 / 120)} Nothing (sortBlobs noMask (clipBlobs headset placing)) Nothing of
                        Just (Right found) -> counterexample (renderFindings (layoutJump found) ++ show (sightingPose (layoutFound found))) (layoutWay found === Refound)
                        other -> counterexample (maybe "not tried" (either show (show . layoutScore)) other) False
        , testProperty "the three blobs following took aren't tried by layout too, nor any against following with nothing against it" $
            \(Apart20 placing) (Scored scored) ->
                let was = placed placing
                    blobs = clipBlobs headset placing
                    tried taken = findHere (knowing (pure noSize) (Just was)) (clockAt 1) (lastAt was) Nothing (sortBlobs noMask blobs) (Just taken)
                    untried = maybe (property True) (const (counterexample "tried" False))
                 in case blobs of
                        [a, b, c] -> untried (tried (Score scored, whole <$> Three a b c)) .&&. untried (tried (possible, pure Nothing))
                        _ -> counterexample "not three blobs" False
        ]

-- | What is known going into a frame: the headset's clip before the
-- default camera, recentered nowhere, with the reflectors' sizes and the
-- last pose given.
knowing :: Three Size -> Maybe Pose -> Known
knowing = Known defaultIntrinsics headset (Reach pivot Nothing)

-- | Where the head turns about, in the clip's axes.
pivot :: V3
pivot = V3 0 100 100

-- | The clip squarely facing the camera, 700 mm off.
facing :: Pose
facing = Pose identity (V3 0 0 700)

-- | A pose tracked at time 1, all three reflectors showing.
lastAt :: Pose -> Recent
lastAt pose = Recent pose (clockAt 1) (clockAt 1) (clockAt 0)

-- | Blobs in an order that doesn't go by the order they came in.
inOrder :: [Blob] -> [(Double, Double, Int)]
inOrder = sortOn id . map (\b -> (blobX b, blobY b, blobPoints b))

-- | A sight of a reflector's own blob, as the blob's place and pixels.
roundAt :: Sight -> Blob
roundAt s = Blob (fst (sightAt s)) (snd (sightAt s)) (round (sightPixels s)) 0 0 0 0

-- | A few blobs of sizes from a fragment's to a lamp's.
newtype Blobs = Blobs [Blob]
    deriving (Show)

instance Arbitrary Blobs where
    arbitrary = do
        n <- chooseInt (0, 6)
        Blobs <$> vectorOf n (oneof [genBlob 40, genBlob 400, genBlob 5000])
    shrink (Blobs blobs) = Blobs <$> shrinkList shrinkBlob blobs

-- | How a head has turned since the last pose, by up to 30 degrees.
newtype Turned = Turned V3
    deriving (Show)

instance Arbitrary Turned where
    arbitrary = Turned <$> genTurn 30
    shrink (Turned w) = Turned <$> shrinkTurn w

-- | What following scored, in nats, from nothing against it down to 60.
newtype Scored = Scored Double
    deriving (Show)

instance Arbitrary Scored where
    arbitrary = Scored <$> choose (-60, 0)
    shrink (Scored x) = Scored <$> shrinkWithin (-60, 0) 0 x
