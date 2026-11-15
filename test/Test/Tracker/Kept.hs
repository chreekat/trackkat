-- | What a frame is taken into, for the next: the still lights (see
-- "TrackKat.Tracker.Still"), which pose is the head's (see
-- "TrackKat.Tracker.Mirror"), what learns from poses (see
-- "TrackKat.Tracker.Learners"), and what a user is told of the clip
-- staying out of sight (see "TrackKat.Tracker.Absence").
module Test.Tracker.Kept (tests) where

import Data.List (isSubsequenceOf, mapAccumL)
import Data.Maybe (isJust)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Test.Gen
import Test.Simulation (headset)
import TrackKat.Branch (Clarity (..), Correction (..), newBranch)
import TrackKat.Calibration (defaultCalibration)
import TrackKat.Frame (Blob (..))
import TrackKat.Lights (Masking (..), View (..), inMask, lightAreas, noLights)
import TrackKat.Linear
import TrackKat.Match (Kind (..), Sight (..), Three (..), noSize)
import TrackKat.Pose
import TrackKat.Term (Finding (..), Name (..), impossible)
import TrackKat.Tracker.Absence
import TrackKat.Tracker.Blobs
import TrackKat.Tracker.Learners
import TrackKat.Tracker.Mirror
import TrackKat.Tracker.Outcome
import TrackKat.Tracker.Still

tests :: TestTree
tests = testGroup "Kept" [still, mirror, learners, absent]

-- | Nothing is masked that the lights learnt don't hold, nor where the
-- clip is predicted to be; and with masking off nothing is learnt or
-- masked at all.
still :: TestTree
still =
    testGroup
        "Still"
        [ testProperty "a lamp that stays put while the clip isn't seen is learnt, once, as an area around it" $
            once $ case lightAreas (stillLights lampLit) of
                [(place, radius)] -> counterexample (show (place, radius)) (distance2 place lampAt < 1 && radius > 0 && radius < 40)
                areas -> counterexample (show areas) False
        , testProperty "a blob is masked just when in a light's area, with no reflector predicted in the area or within 20 pixels of it" $
            forAll (genBy 25) $ \place -> forAll (genBy 60) $ \predicted ->
                let (radius, apart) = (lampRadius, sqrt (distance2 predicted lampAt))
                    b = Blob (fst place) (snd place) 100 10000 8 8 0
                    clear = abs (apart - (radius + 20)) > 1e-6 && abs (sqrt (distance2 place lampAt) - radius) > 1e-6
                 in clear ==>
                        inMask (maskFor lampLit [predicted]) b === (distance2 place lampAt <= radius * radius && apart > radius + 20)
        , testProperty "a mask sorts blobs into those masked and those in view, each in the order they came, none lost" $
            forAllShrink genBlobs (shrinkList shrinkBlob) $ \blobs ->
                let Shown mask masked visible = sortBlobs (maskFor lampLit []) blobs
                 in conjoin
                        [ property (masked `isSubsequenceOf` blobs && visible `isSubsequenceOf` blobs)
                        , length masked + length visible === length blobs
                        , property (all (inMask mask) masked && not (any (inMask mask) visible))
                        ]
        , testProperty "with masking off, nothing is masked, learnt or told" $
            forAllShrink genBlobs (shrinkList shrinkBlob) $ \blobs -> forAll (choose (0, 10)) $ \now ->
                let off = lampLit{stillMasking = NoMasking}
                    (lights, news) = learnUnseen now blobs unwanted off
                 in conjoin
                        [ property (not (any (inMask (maskFor off [])) blobs))
                        , lightAreas lights === lightAreas (stillLights off)
                        , length news === 0
                        ]
        , testProperty "nothing is learnt from one frame, and what would say the clip is in a light isn't asked" $
            forAllShrink genBlobs (shrinkList shrinkBlob) $ \blobs ->
                let (lights, news) = learnUnseen 0 blobs unwanted (Still MaskLights noLights)
                 in lightAreas lights === [] .&&. length news === 0
        , testProperty "the clip is confirmed once tracked for long enough, and stays so" $
            forAll (choose (0, 2)) $ \for -> forAll (choose (0, 1)) $ \longer ->
                standing for 0 === (if for >= confirmSeconds then Confirmed else Unconfirmed)
                    .&&. property (standing for 0 /= Confirmed || standing (for + longer) 0 == Confirmed)
        , testProperty "lights are learnt clear of the clip only once it is confirmed: around each reflector, by 15 pixels or more" $
            forAllShrink (genSeen 30 trackedRange headset) (shrinkSeen headset) $ \placing -> forAllShrink (genThree (genSight 500)) (shrinkThree shrinkSight) $ \seen ->
                let view confirmed = clipView defaultIntrinsics headset confirmed (placed placing) (Just <$> seen) (Just 300)
                 in conjoin
                        [ length (viewDiscs (view Unconfirmed)) === 0
                        , viewUsual (view Unconfirmed) === Nothing
                        , map fst (viewDiscs (view Confirmed)) === map (seenAt defaultIntrinsics headset (placed placing)) [LeftReflector, RightReflector, TopReflector]
                        , property (all ((>= 15) . snd) (viewDiscs (view Confirmed)))
                        , viewUsual (view Confirmed) === Just 300
                        , length (viewSights (view Confirmed)) === 3 .&&. length (viewSights (view Unconfirmed)) === 3
                        ]
        ]

-- | A frame shows the reflectors clearly only as three blobs of their
-- own, and no more clearly for more blobs; and one frame switches no
-- pose for its mirror image.
mirror :: TestTree
mirror =
    testGroup
        "Mirror"
        [ testProperty "the clip alone in view, facing the camera within 20 degrees, shows clearly" $
            forAllShrink (genClipApart 20 (400, 1500) headset) (shrinkClipApart headset) $ \placing ->
                case clipBlobs headset placing of
                    blobs@[a, b, c] -> clarity (pure noSize) blobs (placed placing) (whole <$> Three a b c) === Clear
                    blobs -> counterexample (show blobs) False
        , testProperty "reflectors don't show clearly with one unseen, or not a blob of its own, nor more clearly for another blob in view" $
            forAllShrink (genClipApart 20 (400, 1500) headset) (shrinkClipApart headset) $ \placing -> forAllShrink (genBlob 400) shrinkBlob $ \another ->
                forAllShrink (genSizes 3000) shrinkSizes $ \sizes -> forAll (elements [Rejoined, Merged]) $ \kind ->
                    case clipBlobs headset placing of
                        blobs@[a, b, c] ->
                            let seen = whole <$> Three a b c
                                clear with sights = clarity sizes with (placed placing) sights == Clear
                             in conjoin
                                    [ property (not (clear blobs (Three Nothing (whole b) (whole c))))
                                    , property (not (clear blobs (Three (fmap (\s -> s{sightKind = kind}) (whole a)) (whole b) (whole c))))
                                    , property (not (clear (another : blobs) seen) || clear blobs seen)
                                    ]
                        blobs -> counterexample (show blobs) False
        , testProperty "recentered on a pose, that pose is the centre" $
            forAllShrink (genSeen 60 trackedRange headset) (shrinkSeen headset) $ \placing ->
                let Pose r t = placed placing
                 in fmap (\(Pose r' t') -> (r', t')) (centrePose (recenterOn (placed placing) (Centre Nothing (newBranch headset)))) === Just (r, t)
        , testProperty "with nothing yet believed, one frame switches neither the pose nor the centre for its mirror image" $
            forAllShrink (genSeen 60 trackedRange headset) (shrinkSeen headset) $ \placing -> forAllShrink (genSeen 60 trackedRange headset) (shrinkSeen headset) $ \centre ->
                forAll (elements [Clear, Doubtful]) $ \clear ->
                    let Pose r t = placed placing
                        Pose cr ct = placed centre
                        told = tellApart defaultIntrinsics headset pivot 1 clear (placed placing) (pure (Just (Sight (0, 0) 100 Whole))) (Centre (Just (placed centre)) (newBranch headset))
                     in conjoin
                            [ toldCorrection told === MirrorNeither
                            , (poseR (toldPose told), poseT (toldPose told)) === (r, t)
                            , fmap (\(Pose r' t') -> (r', t')) (centrePose (toldCentre told)) === Just (cr, ct)
                            ]
        ]

-- | A calibration asked for watches for its seconds and then ends, once,
-- whatever else the learners went through meanwhile.
learners :: TestTree
learners =
    testGroup
        "Learners"
        [ testProperty "a calibration asked for says nothing while it watches, ends once when its seconds are up, and says nothing after" $
            forAll (choose (0, 10)) $ \asked -> forAll (listOf (choose (0, 2 * calibrationSeconds))) $ \gaps ->
                forAll (sublistOf [0 .. length gaps]) $ \lost -> forAll (sublistOf [0 .. length gaps]) $ \corrected ->
                    let times = scanl1 (+) (map (/ 4) gaps)
                        frame (i, after) =
                            ( if i `elem` lost then lostSightOf else id
                            , if i `elem` corrected then startOver else id
                            , asked + after
                            )
                        watched = snd (mapAccumL (\l (lose, correct, now) -> sweepOn now facing facing (correct (lose l))) (startSweep asked (newLearners headset defaultCalibration)) (map frame (zip [0 :: Int ..] times)))
                        ended = [length events | (after, events) <- zip times watched, asked + after >= asked + calibrationSeconds]
                        before = [length events | (after, events) <- zip times watched, asked + after < asked + calibrationSeconds]
                     in counterexample (show (times, map length watched)) $
                            all (== 0) before && take 1 ended == [1 | not (null ended)] && all (== 0) (drop 1 ended)
        , testProperty "with no calibration asked for, poses are taken in with nothing said of one" $
            forAll (choose (0, 100)) $ \now ->
                length (snd (sweepOn now facing facing (newLearners headset defaultCalibration))) === 0
        ]

-- | A user is told that the clip is out of sight only when it has been
-- for a while with lights enough in view to be it, and not again soon.
absent :: TestTree
absent =
    testGroup
        "Absence"
        [ testProperty "the clip out of sight is told only after a second of it, with three lights or more in view, and never twice in ten seconds" $
            forAllShrink genSpells (shrinkList (const [])) $ \spells ->
                let frames = zip [0, 0.25 ..] (concat [replicate n spell | (n, spell) <- spells])
                    step (since, a) (now, spell) =
                        let (a', said) = absence now (outcome spell) a
                            since' = case spell of
                                InSight -> Nothing
                                _ -> Just (maybe now id since)
                         in ((since', a'), (now, spell, since', isJust said))
                    told = [frame | frame@(_, _, _, True) <- snd (mapAccumL step (Nothing, noAbsence) frames)]
                 in conjoin
                        [ property (and [lights >= 3 | (_, OutOfSight lights _, _, _) <- told])
                        , property (null [() | (_, InSight, _, _) <- told])
                        , property (and [now - began > 1 | (now, _, Just began, _) <- told])
                        , property (and [later - sooner >= 10 | ((sooner, _, _, _), (later, _, _, _)) <- zip told (drop 1 told)])
                        ]
        ]

-- | What a quarter of a second of frames showed: the clip, or so many
-- lights that weren't made out as it, for the reason numbered.
data Spell = InSight | OutOfSight Int Int
    deriving (Show)

outcome :: Spell -> Outcome
outcome InSight = Tracked (HeadPose 0 0 0 0 0 0) 800 0 (Seen [] 0 0 0) [] (Chosen Followed Nothing)
outcome (OutOfSight lights numbered) = Lost lights 0 (Why Following (Unfit [Finding (Name ("reason " ++ show numbered) "") impossible Nothing])) Nothing

-- | Spells of up to a minute each, in sight and out.
genSpells :: Gen [(Int, Spell)]
genSpells = listOf ((,) <$> chooseInt (1, 240) <*> oneof [pure InSight, OutOfSight <$> chooseInt (0, 5) <*> chooseInt (1, 2)])

-- | Something that mustn't be looked at.
unwanted :: a
unwanted = error "something was worked out that wasn't needed"

-- | Where the head turns about, in the clip's axes.
pivot :: V3
pivot = V3 0 100 100

-- | The clip squarely facing the camera, 700 mm off.
facing :: Pose
facing = Pose identity (V3 0 0 700)

-- | Where a lamp is in the picture.
lampAt :: (Double, Double)
lampAt = (200, 150)

-- | The lights learnt from a lamp of 2000 pixels seen for four seconds,
-- 120 times a second, with the clip not in sight.
lampLit :: Still
lampLit = foldl' seen (Still MaskLights noLights) [0 .. 480 :: Int]
  where
    lamp = Blob (fst lampAt) (snd lampAt) 2000 200000 160 160 0
    seen s i = s{stillLights = fst (learnUnseen (fromIntegral i / 120) [lamp] (const []) s)}

-- | A place within so many pixels of the lamp, each way.
genBy :: Double -> Gen (Double, Double)
genBy most = (\du dv -> (fst lampAt + du, snd lampAt + dv)) <$> choose (negate most, most) <*> choose (negate most, most)

-- | The radius of the area masked around the lamp.
lampRadius :: Double
lampRadius = sum (map snd (lightAreas (stillLights lampLit)))

-- | A few blobs of sizes from a fragment's to a lamp's, some of them by
-- the lamp.
genBlobs :: Gen [Blob]
genBlobs = do
    n <- chooseInt (0, 8)
    vectorOf n (oneof [genBlob 400, genBlob 5000, (\b -> b{blobX = fst lampAt + blobX b / 100, blobY = snd lampAt + blobY b / 100}) <$> genBlob 400])
