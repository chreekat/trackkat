-- | Making out the clip by following a pose (see
-- "TrackKat.Tracker.Follow").
module Test.Tracker.Follow (tests) where

import Data.Maybe (isJust)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Test.Gen
import Test.Instances ()
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
import TrackKat.Tracker.Sizes (Showing (..), showingSeconds)
import TrackKat.Tracker.Trail (Recent (..))
import TrackKat.World (Instant, Seconds, clockAt, inSeconds, seconds, (.+^))

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
            \(Apart30 placing) ->
                case follow (knowing (placed placing)) aFrameOn (lastAt (placed placing)) (sortBlobs noMask (clipBlobs headset placing)) of
                    Right (scored, Sighting _ fit sights strays masked _) ->
                        conjoin
                            [ property (all isJust sights)
                            , (strays, masked) === (0, 0)
                            , counterexample ("fit " ++ show fit) (fit < 0.5)
                            , counterexample ("scored " ++ show scored) (scored <= possible && scored >= weakScore)
                            ]
                    Left why -> counterexample (explainWhy why) False
        , testProperty "a stray far from the clip doesn't change where it is followed to" $
            \(Apart30 placing) ->
                let blobs = clipBlobs headset placing
                    far stray = all (\b -> distance2 (at b) (at stray) > 200 * 200) blobs
                    followedTo shown = either (Left . explainWhy) (\(_, s) -> Right (poseR (sightingPose s), poseT (sightingPose s))) (follow (knowing (placed placing)) aFrameOn (lastAt (placed placing)) (sortBlobs noMask shown))
                 in forAllShrink (genBlob 400 `suchThat` far) (filter far . shrinkBlob) $ \stray ->
                        followedTo (blobs ++ [stray]) === followedTo blobs
        , testProperty "one blob in view isn't the clip, whatever it is" $
            \(SeenNear placing) (Lone b) ->
                case follow (knowing (placed placing)) aFrameOn (lastAt (placed placing)) (sortBlobs noMask [b]) of
                    Left (Why looking _) -> looking === Following
                    Right (_, s) -> counterexample (renderFindings (sightingJudged s)) False
        , testProperty "the clip is followed only to a pose that follows the last, and is otherwise lost as following" $
            \(Apart30 placing) (Turned turn) (Shifted move) (FrameGap gap) ->
                let Pose r t = turnedAbout pivot (rodrigues turn) (placed placing)
                    was = Pose r (t ^+^ move)
                    dt = seconds gap
                 in case follow (knowing was) (lastTracked .+^ dt) (lastAt was) (sortBlobs noMask (clipBlobs headset placing)) of
                        Right (scored, Sighting pose fit _ _ _ judged) ->
                            let (turned, moved) = poseChange pivot was pose
                             in conjoin
                                    [ property (accepts (followable (Reach pivot Nothing) dt) (Moved pose fit turned moved))
                                    , property (scored <= possible)
                                    , property (all (not . isImpossible . findingScore) judged)
                                    ]
                        Left (Why looking _) -> looking === Following
        , testProperty "the clip isn't followed to where a head can't be, though it is just where the last pose has it" $
            \(Apart30 placing) ->
                let centre = turnedAbout pivot (rodrigues (V3 0 (150 * pi / 180) 0)) (placed placing)
                    known = (knowing (placed placing)){knownReach = Reach pivot (Just centre)}
                 in case follow known aFrameOn (lastAt (placed placing)) (sortBlobs noMask (clipBlobs headset placing)) of
                        Left why -> whyRejectedBy why === ["turned"]
                        Right _ -> counterexample "followed" False
        , testProperty "with no blobs in view, nothing matches" $
            \(SeenNear placing) ->
                case follow (knowing (placed placing)) aFrameOn (lastAt (placed placing)) (sortBlobs noMask []) of
                    Left why -> why === Why Following NothingMatches
                    Right _ -> counterexample "followed" False
        , testProperty "a match showing every reflector as a blob of its own may be followed however long since; one showing less, only as long as when all showed more lately" $
            \(ReflectorSights seen) (Since since) (Fraction sooner) (Lit lit) ->
                let all3 = Match ((\s -> Just s{sightKind = Whole}) <$> seen) 0 0 []
                    less = Match ((\s shown -> if shown then Just s else Nothing) <$> seen <*> lit) 0 0 []
                    may gap c = accepts (partlySeen (pure noSize) (lastTracked .+^ seconds gap) (lastAt facing)) c
                 in property (may since all3) .&&. property (not (may since less) || may (sooner * since) less)
        , testProperty "the clip still where it was has as many reflectors out of sight as have no blob near them, each costing what one unseen costs a match" $
            \(Apart30 placing) ->
                forSomeOf (clipBlobs headset placing) $ \blobs ->
                    stillThere defaultIntrinsics headset (clockAt 1.5) (lastAt (placed placing)) blobs
                        === (3 - length blobs, Score (fromIntegral (3 - length blobs) * nats unseenScore))
        , testProperty "other blobs in view leave no more of the clip's reflectors out of sight, and one blob is never two reflectors" $
            \(Apart30 placing) ->
                forSomeOf (clipBlobs headset placing) $ \blobs (Strays others) ->
                    let missing = fst . stillThere defaultIntrinsics headset (clockAt 1.5) (lastAt (placed placing))
                     in conjoin
                            [ property (missing (others ++ blobs) <= missing blobs)
                            , property (missing (others ++ blobs) >= 0)
                            , property (all (\b -> missing [b] >= 2) (others ++ blobs))
                            ]
        , testProperty "the clip can't be still where it was once it has shown less than all three for longer than it may" $
            \(Apart30 placing) (SinceAll since) ->
                let blobs = clipBlobs headset placing :: [Blob]
                 in isImpossible (snd (stillThere defaultIntrinsics headset (clockAt 0 .+^ since) (lastAt (placed placing)){recentAllSeen = clockAt 0} (take 2 blobs))) === (since > showingSeconds OneOut)
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

-- | A pose tracked a second after the clock began, all three reflectors
-- showing, and tracked since it began.
lastAt :: Pose -> Recent
lastAt pose = Recent pose lastTracked lastTracked (clockAt 0)

-- | When 'lastAt' has its pose tracked, and a frame after.
lastTracked, aFrameOn :: Instant
lastTracked = clockAt 1
aFrameOn = lastTracked .+^ seconds (1 / 120)

-- | A pose of the headset clip that the camera sees whole, from 400 mm to
-- 1.5 m off, turned up to 30 degrees from squarely facing the camera.
newtype SeenNear = SeenNear Placing
    deriving (Show)

instance Arbitrary SeenNear where
    arbitrary = SeenNear <$> genSeen 30 (400, 1500) headset
    shrink (SeenNear placing) = SeenNear <$> shrinkSeen (400, 1500) headset placing

-- | One blob, of up to 2000 pixels, anywhere in the picture.
newtype Lone = Lone Blob
    deriving (Show)

instance Arbitrary Lone where
    arbitrary = Lone <$> genBlob 2000
    shrink (Lone b) = Lone <$> shrinkBlob b

-- | How a head has turned since the last pose, by up to 25 degrees.
newtype Turned = Turned V3
    deriving (Show)

instance Arbitrary Turned where
    arbitrary = Turned <$> genTurn 25
    shrink (Turned w) = Turned <$> shrinkTurn w

-- | How a head has moved since the last pose, by up to 60 mm each way.
newtype Shifted = Shifted V3
    deriving (Show)

instance Arbitrary Shifted where
    arbitrary = Shifted <$> genV3 60
    shrink (Shifted v) = Shifted <$> shrinkV3 v

-- | Seconds since all three reflectors showed, up to four.
newtype Since = Since Double
    deriving (Show)

instance Arbitrary Since where
    arbitrary = Since <$> choose (0, 4)
    shrink (Since x) = Since <$> shrinkWithin (0, 4) 0 x

-- | Seconds the clip has shown less than all three reflectors, up to three.
newtype SinceAll = SinceAll Seconds
    deriving (Show)

instance Arbitrary SinceAll where
    arbitrary = SinceAll . seconds <$> choose (0, 3)
    shrink (SinceAll x) = SinceAll . seconds <$> shrinkWithin (0, 3) 0 (inSeconds x)

-- | Which reflectors are lit: all of them, or all but one. Simplest all.
newtype Lit = Lit (Three Bool)
    deriving (Show)

instance Arbitrary Lit where
    arbitrary = Lit <$> elements lights
    shrink (Lit lit) = Lit <$> earlier lights lit

lights :: [Three Bool]
lights = [Three True True True, Three False True True, Three True False True, Three True True False]
