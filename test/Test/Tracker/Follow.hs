-- | Making out the clip by following a pose (see
-- "TrackKat.Tracker.Follow").
module Test.Tracker.Follow (tests) where

import Data.Maybe (isJust)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Test.Gen
import Test.Simulation (headset)
import TrackKat.Frame (Blob)
import TrackKat.Lights (noMask)
import TrackKat.Linear
import TrackKat.Match (Kind (..), Match (..), Sight (..), Three (..), noSize, unseenScore)
import TrackKat.Pose
import TrackKat.Term
import TrackKat.Tracker.Blobs
import TrackKat.Tracker.Choose (weakScore)
import TrackKat.Tracker.Follow
import TrackKat.Tracker.Judge
import TrackKat.Tracker.Outcome
import TrackKat.Tracker.Sighting
import TrackKat.Tracker.Trail (Recent (..))

-- | The clip is followed only to a pose that follows the last, as the
-- gates on following say; where it is just where the last pose has it,
-- it is followed there without doubt, every reflector seen. And the clip
-- is still where it was the likelier the more of its reflectors have
-- blobs.
tests :: TestTree
tests =
    testGroup
        "Follow"
        [ testProperty "the clip where the last pose has it is followed, every reflector seen and nothing else" $
            forAllShrink (genClipApart 30 (400, 1500) headset) (shrinkClipApart headset) $ \placing ->
                case follow (knowing (placed placing)) (1 + 1 / 120) (lastAt (placed placing)) (sortBlobs noMask (clipBlobs headset placing)) of
                    Right (scored, Sighting _ fit sights strays masked _) ->
                        conjoin
                            [ property (all isJust sights)
                            , (strays, masked) === (0, 0)
                            , counterexample ("fit " ++ show fit) (fit < 0.5)
                            , counterexample ("scored " ++ show scored) (scored <= possible && scored >= weakScore)
                            ]
                    Left why -> counterexample (explainWhy why) False
        , testProperty "a stray far from the clip doesn't change where it is followed to" $
            forAllShrink (genClipApart 30 (400, 1500) headset) (shrinkClipApart headset) $ \placing ->
                let blobs = clipBlobs headset placing
                    far stray = all (\b -> distance2 (at b) (at stray) > 200 * 200) blobs
                    followedTo shown = either (Left . explainWhy) (\(_, s) -> Right (poseR (sightingPose s), poseT (sightingPose s))) (follow (knowing (placed placing)) (1 + 1 / 120) (lastAt (placed placing)) (sortBlobs noMask shown))
                 in forAllShrink (genBlob 400 `suchThat` far) (filter far . shrinkBlob) $ \stray ->
                        followedTo (blobs ++ [stray]) === followedTo blobs
        , testProperty "one blob in view isn't the clip, whatever it is" $
            forAllShrink (genSeen 30 (400, 1500) headset) (shrinkSeen headset) $ \placing -> forAllShrink (genBlob 2000) shrinkBlob $ \b ->
                case follow (knowing (placed placing)) (1 + 1 / 120) (lastAt (placed placing)) (sortBlobs noMask [b]) of
                    Left (Why looking _) -> looking === Following
                    Right (_, s) -> counterexample (renderFindings (sightingJudged s)) False
        , testProperty "the clip is followed only to a pose that follows the last, and is otherwise lost as following" $
            forAllShrink (genClipApart 30 (400, 1500) headset) (shrinkClipApart headset) $ \placing -> forAllShrink (genTurn 25) shrinkTurn $ \turn ->
                forAllShrink (genV3 60) shrinkV3 $ \move -> forAll (choose (1 / 120, 0.5)) $ \dt ->
                    let Pose r t = turnedAbout pivot (rodrigues turn) (placed placing)
                        was = Pose r (t ^+^ move)
                     in case follow (knowing was) (1 + dt) (lastAt was) (sortBlobs noMask (clipBlobs headset placing)) of
                            Right (scored, Sighting pose fit _ _ _ judged) ->
                                let (turned, moved) = poseChange pivot was pose
                                 in conjoin
                                        [ property (accepts (followable (Reach pivot Nothing) dt) (Moved pose fit turned moved))
                                        , property (scored <= possible)
                                        , property (all (not . isImpossible . findingScore) judged)
                                        ]
                            Left (Why looking _) -> looking === Following
        , testProperty "the clip isn't followed to where a head can't be, though it is just where the last pose has it" $
            forAllShrink (genClipApart 30 (400, 1500) headset) (shrinkClipApart headset) $ \placing ->
                let centre = turnedAbout pivot (rodrigues (V3 0 (150 * pi / 180) 0)) (placed placing)
                    known = (knowing (placed placing)){knownReach = Reach pivot (Just centre)}
                 in case follow known (1 + 1 / 120) (lastAt (placed placing)) (sortBlobs noMask (clipBlobs headset placing)) of
                        Left why -> whyRejectedBy why === ["turned"]
                        Right _ -> counterexample "followed" False
        , testProperty "with no blobs in view, nothing matches" $
            forAllShrink (genSeen 30 (400, 1500) headset) (shrinkSeen headset) $ \placing ->
                case follow (knowing (placed placing)) (1 + 1 / 120) (lastAt (placed placing)) (sortBlobs noMask []) of
                    Left why -> why === Why Following NothingMatches
                    Right _ -> counterexample "followed" False
        , testProperty "a match showing every reflector as a blob of its own may be followed however long since; one showing less, only as long as when all showed more lately" $
            forAllShrink (genThree (genSight 500)) (shrinkThree shrinkSight) $ \seen -> forAll (choose (0, 4)) $ \since -> forAll (choose (0, 1)) $ \sooner ->
                forAll (elements [Three True True True, Three False True True, Three True False True, Three True True False]) $ \lit ->
                    let all3 = Match ((\s -> Just s{sightKind = Whole}) <$> seen) 0 0 []
                        less = Match ((\s shown -> if shown then Just s else Nothing) <$> seen <*> lit) 0 0 []
                        may gap c = accepts (partlySeen (pure noSize) (1 + gap) (lastAt facing)) c
                     in property (may since all3) .&&. property (not (may since less) || may (sooner * since) less)
        , testProperty "the clip still where it was has as many reflectors out of sight as have no blob near them, each costing what one unseen costs a match" $
            forAllShrink (genClipApart 30 (400, 1500) headset) (shrinkClipApart headset) $ \placing ->
                forAll (sublistOf (clipBlobs headset placing)) $ \blobs ->
                    stillThere defaultIntrinsics headset 1.5 (lastAt (placed placing)) blobs
                        === (3 - length blobs, Score (fromIntegral (3 - length blobs) * nats unseenScore))
        , testProperty "other blobs in view leave no more of the clip's reflectors out of sight, and one blob is never two reflectors" $
            forAllShrink (genClipApart 30 (400, 1500) headset) (shrinkClipApart headset) $ \placing ->
                forAll (sublistOf (clipBlobs headset placing)) $ \blobs -> forAllShrink (listOf (genBlob 400)) (shrinkList shrinkBlob) $ \others ->
                    let missing = fst . stillThere defaultIntrinsics headset 1.5 (lastAt (placed placing))
                     in conjoin
                            [ property (missing (others ++ blobs) <= missing blobs)
                            , property (missing (others ++ blobs) >= 0)
                            , property (all (\b -> missing [b] >= 2) (others ++ blobs))
                            ]
        , testProperty "the clip can't be still where it was once it has shown less than all three for longer than it may" $
            forAllShrink (genClipApart 30 (400, 1500) headset) (shrinkClipApart headset) $ \placing -> forAll (choose (0, 3)) $ \since ->
                let blobs = clipBlobs headset placing :: [Blob]
                 in isImpossible (snd (stillThere defaultIntrinsics headset since (lastAt (placed placing)){recentAllSeen = 0} (take 2 blobs))) === (since > 1)
        ]

-- | What is known going into a frame: the headset's clip before the
-- default camera, recentered nowhere, its reflectors not yet seen, last
-- tracked at the pose given.
knowing :: Pose -> Known
knowing was = Known defaultIntrinsics headset (Reach pivot Nothing) (pure noSize) (Just was)

-- | Where the head turns about, in the clip's axes.
pivot :: V3
pivot = V3 0 100 100

-- | The clip squarely facing the camera, 700 mm off.
facing :: Pose
facing = Pose identity (V3 0 0 700)

-- | A pose tracked at time 1, all three reflectors showing.
lastAt :: Pose -> Recent
lastAt pose = Recent pose 1 1 0
