-- | The terms a frame's hypotheses are judged by (see
-- "TrackKat.Tracker.Judge").
module Test.Tracker.Judge (tests) where

import Data.Foldable (toList)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Test.Check
import Test.Gen
import Test.Simulation (headset)
import TrackKat.Frame (Blob (..))
import TrackKat.Linear
import TrackKat.Match (Kind (..), Sight (..), Size (..), Three (..), noSize, sizeScore)
import TrackKat.Pose
import TrackKat.Term
import TrackKat.Tracker.Judge

-- Note [What the tracker's terms promise]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A gate has a side that is safe: what it lets through, it lets through
-- when nearer, slower, better fitting or bigger, so a limit can be moved
-- without something on its safe side falling out. A score has a peak of
-- nothing against, where its figure is just what was expected, and falls
-- away from it. And the two kinds agree where they judge the same figure:
-- what continues best is never likelier anywhere, and a blob the size gate
-- refuses scores as badly as the gate's own limit.
tests :: TestTree
tests = group "Judge" $ do
    checkShowing "finding afresh gives up, a frame, 0.1 s and 0.5 s after the last pose" (show (map afreshCost [1 / 120, 0.1, 0.5])) $
        and (zipWith (\gave roughly -> abs (gave - roughly) < 0.05) (map afreshCost [1 / 120, 0.1, 0.5]) [22.4, 7.5, 1.8])
    tree gates
    tree scores

gates :: TestTree
gates =
    testGroup
        "gates"
        [ testProperty "a jump that is possible is possible of a pose that turned and moved less, and after longer" $
            forAllShrink genJump shrinkJump $ \(turn, move, dt) -> forAll (choose (0, 1)) $ \less -> forAll (choose (0, 1)) $ \longer ->
                let jumped = Moved facing 0 turn move
                 in accepts (jumpable dt) jumped ==> accepts (jumpable (dt + longer)) jumped{movedTurn = less * turn, movedMove = less * move}
        , testProperty "a jump is possible just when neither the turn nor the move is past what a head does in the time" $
            forAllShrink genJump shrinkJump $ \(turn, move, dt) ->
                accepts (jumpable dt) (Moved facing 0 turn move) === (turn <= maxTurn dt && move <= maxMove dt)
        , testProperty "a pose follows just when it fits, is no impossible jump, and is somewhere a head can be" $
            forAllShrink (genSeen 150 (100, 3000) headset) (shrinkSeen headset) $ \placing ->
                forAllShrink genCentre shrinkCentre $ \centre -> forAllShrink genJump shrinkJump $ \(turn, move, dt) -> forAll (choose (0, 4)) $ \fit ->
                    let reach = Reach pivot (placed <$> centre)
                        moved = Moved (placed placing) fit turn move
                     in accepts (followable reach dt) moved
                            === (fit <= maxFit && accepts (jumpable dt) moved && accepts (canBeThere reach) (placed placing))
        , testProperty "with nowhere recentered, a head can be anywhere in the camera's range, and nowhere else" $
            forAllShrink (genSeen 180 (100, 3000) headset) (shrinkSeen headset) $ \placing ->
                let distance = norm (poseT (placed placing))
                 in accepts (canBeThere (Reach pivot Nothing)) (placed placing) === (distance >= nearest && distance <= farthest)
        , testProperty "where a head can be, recentered somewhere, it can be with nowhere recentered" $
            forAllShrink (genSeen 180 (100, 3000) headset) (shrinkSeen headset) $ \placing -> forAllShrink genCentre shrinkCentre $ \centre ->
                accepts (canBeThere (Reach pivot (placed <$> centre))) (placed placing) ==> accepts (canBeThere (Reach pivot Nothing)) (placed placing)
        , testProperty "a head can be where it was recentered, and as near or as far along the line of sight as the camera sees" $
            forAllShrink (genSeen 60 trackedRange headset) (shrinkSeen headset) $ \centre -> forAll (choose (-2500, 2500)) $ \back ->
                let Pose r t = placed centre
                    leaned = Pose r (t ^+^ back *^ normalize (poseAt (placed centre) pivot))
                    distance = norm (poseT leaned)
                 in accepts (canBeThere (Reach pivot (Just (placed centre)))) leaned === (distance >= nearest && distance <= farthest)
        , testProperty "before the clip has been seen, blobs of any size are big enough" $
            forAllShrink (genThree (genBlob 5000)) (shrinkThree shrinkBlob) $ \three -> forAllShrink (genSeen 60 trackedRange headset) (shrinkSeen headset) $ \placing ->
                property (accepts (bigEnough (pure noSize)) (Found (placed placing) three))
        , testProperty "blobs big enough are big enough when bigger, further off, and for reflectors that were smaller" $
            forAllShrink (genThree (genBlob 500)) (shrinkThree shrinkBlob) $ \three -> forAllShrink (genSizes 3000) shrinkSizes $ \sizes ->
                forAllShrink (genSeen 60 trackedRange headset) (shrinkSeen headset) $ \placing ->
                    forAll (chooseInt (0, 500)) $ \more -> forAll (choose (1, 2)) $ \further -> forAll (choose (0, 1)) $ \smaller ->
                        let Pose r t = placed placing
                            bigger = (\b -> b{blobPoints = blobPoints b + more}) <$> three
                            were = (\s -> s{sizeUsual = smaller * sizeUsual s}) <$> sizes
                         in accepts (bigEnough sizes) (Found (placed placing) three) ==> accepts (bigEnough were) (Found (Pose r (further *^ t)) bigger)
        , testProperty "the size told is of the blob furthest below what its reflector would light, on the side of its limit that the gate says" $
            forAllShrink (genThree (genBlob 500)) (shrinkThree shrinkBlob) $ \three -> forAllShrink (genSizes 3000) shrinkSizes $ \sizes ->
                forAllShrink (genSeen 60 trackedRange headset) (shrinkSeen headset) $ \placing ->
                    let found = Found (placed placing) three
                        least size = sizeUsual size / (smallestShare * metres (placed placing) ^ (2 :: Int))
                        short = toList ((\size b -> fromIntegral (blobPoints b) - least size) <$> sizes <*> three)
                     in case findings (bigEnough sizes) found of
                            [Finding _ _ (Just (Measure lit AtLeast wanted "px"))] ->
                                close 1e-6 (lit - wanted) (minimum short)
                                    .&&. (abs (lit - wanted) > 1e-6 ==> accepts (bigEnough sizes) found === (lit >= wanted))
                            other -> counterexample (show other) False
        , testProperty "three blobs are the clip just when they are big enough, somewhere a head can be" $
            forAllShrink (genThree (genBlob 500)) (shrinkThree shrinkBlob) $ \three -> forAllShrink (genSizes 3000) shrinkSizes $ \sizes ->
                forAllShrink (genSeen 150 (100, 3000) headset) (shrinkSeen headset) $ \placing -> forAllShrink genCentre shrinkCentre $ \centre ->
                    let reach = Reach pivot (placed <$> centre)
                        found = Found (placed placing) three
                     in accepts (clipLike reach sizes) found === (accepts (canBeThere reach) (placed placing) && accepts (bigEnough sizes) found)
        ]

scores :: TestTree
scores =
    testGroup
        "scores"
        [ testProperty "nothing scores a pose above nothing against it" $
            forAllShrink (genSeen 150 trackedRange headset) (shrinkSeen headset) $ \placing -> forAllShrink genCentre shrinkCentre $ \centre ->
                forAllShrink genJump shrinkJump $ \(turn, move, dt) -> forAll (choose (0, 4)) $ \fit ->
                    forAllShrink (genSizes 3000) shrinkSizes $ \sizes -> forAllShrink genSights shrinkSights $ \sights ->
                        let moved = Moved (placed placing) fit turn move
                            reach = Reach pivot (placed <$> centre)
                         in conjoin
                                [ counterexample name (property (score term moved <= possible))
                                | (name, term) <- [("misfit", misfit), ("continuing", continuing dt), ("anywhere", anywhere reach dt), ("blobs", blobsSay sizes (placed placing) sights)]
                                ]
        , testProperty "a pose that neither turned nor moved continues the last as well as any can" $
            forAll genGap $
                \dt -> score (continuing dt) (Moved facing 0 0 0) === possible
        , testProperty "a pose continues the last no better for turning or moving further, and no worse for longer between" $
            forAllShrink genJump shrinkJump $ \(turn, move, dt) -> forAll (choose (0, 1)) $ \less -> forAll (choose (0, 1)) $ \longer ->
                let jumped = Moved facing 0 turn move
                    nearer = jumped{movedTurn = less * turn, movedMove = less * move}
                 in property (score (continuing dt) jumped <= score (continuing dt) nearer && score (continuing dt) jumped <= score (continuing (dt + longer)) jumped)
        , testProperty "a pose fits no better for fitting worse, and one that fits exactly has nothing against it" $
            forAll (choose (0, 4)) $ \fit -> forAll (choose (0, 1)) $ \less ->
                property (score misfit (Moved facing fit 0 0) <= score misfit (Moved facing (less * fit) 0 0)) .&&. score misfit (Moved facing 0 0 0) === possible
        , testProperty "finding afresh gives up something, less the longer since the last pose, and the same within a frame of it" $
            forAll genGap $ \dt -> forAll (choose (0, 1)) $ \longer ->
                conjoin
                    [ property (afreshCost dt >= 0)
                    , property (afreshCost (dt + longer) <= afreshCost dt)
                    , afreshCost (dt / 240) === afreshCost (1 / 120)
                    ]
        , testProperty "a head anywhere scores what finding afresh gives up, and no better for where it was recentered" $
            forAllShrink (genSeen 150 trackedRange headset) (shrinkSeen headset) $ \placing -> forAllShrink genCentre shrinkCentre $ \centre -> forAll genGap $ \dt ->
                let moved = Moved (placed placing) 0 0 0
                 in score (anywhere (Reach pivot Nothing) dt) moved === Score (negate (afreshCost dt))
                        .&&. property (score (anywhere (Reach pivot (placed <$> centre)) dt) moved <= Score (negate (afreshCost dt)))
        , testProperty "a pose that stayed where it was is never likelier anywhere than continuing" $
            forAllShrink (genSeen 150 trackedRange headset) (shrinkSeen headset) $ \placing -> forAllShrink genCentre shrinkCentre $ \centre -> forAll genGap $ \dt ->
                let still = Moved (placed placing) 0 0 0
                 in property (score (continuing dt) still >= score (anywhere (Reach pivot (placed <$> centre)) dt) still)
        , testProperty "sizes say nothing before the clip has been seen, nor of a reflector unseen or sharing a blob" $
            forAllShrink (genSeen 60 trackedRange headset) (shrinkSeen headset) $ \placing ->
                forAllShrink (genSizes 3000) shrinkSizes $ \sizes -> forAllShrink genSights shrinkSights $ \sights ->
                    let shared = fmap (\s -> s{sightKind = Merged}) <$> sights
                     in conjoin
                            [ score (usualSize (pure noSize)) (placed placing, sights) === possible
                            , score (usualSize sizes) (placed placing, shared) === possible
                            , score (usualSize sizes) (placed placing, pure Nothing) === possible
                            ]
        , testProperty "blobs just the size their reflectors have been have nothing against them, and others the more the further off, bigger or smaller alike" $
            forAllShrink (genSeen 60 trackedRange headset) (shrinkSeen headset) $ \placing -> forAllShrink (genSizes 3000) shrinkSizes $ \sizes ->
                forAll (choose (1, 10)) $ \off -> forAll (choose (1, 2)) $ \further ->
                    let pose = placed placing
                        blobsOf by = (\size -> Just (Sight (0, 0) (by * sizeUsual size / metres pose ^ (2 :: Int)) Whole)) <$> sizes
                        scored by = score (usualSize sizes) (pose, blobsOf by)
                     in conjoin
                            [ scored 1 === possible
                            , property (scored (off * further) <= scored off)
                            , close 1e-9 (nats (scored off)) (nats (scored (1 / off)))
                            ]
        , testProperty "blobs too small to be the clip score no better than one at the limit of big enough" $
            forAllShrink (genThree (genBlob 500)) (shrinkThree shrinkBlob) $ \three -> forAllShrink (genSizes 3000) shrinkSizes $ \sizes ->
                forAllShrink (genSeen 60 trackedRange headset) (shrinkSeen headset) $ \placing ->
                    let pose = placed placing
                        whole b = Just (Sight (blobX b, blobY b) (fromIntegral (blobPoints b)) Whole)
                        refused = not (accepts (bigEnough sizes) (Found pose three))
                     in property (not refused || nats (score (usualSize sizes) (pose, whole <$> three)) <= nats (sizeScore smallestShare 1) + 1e-9)
        ]

-- | The headset's clip squarely facing the camera, 700 mm off.
facing :: Pose
facing = Pose identity (V3 0 0 700)

-- | Where the head turns about, in the clip's axes.
pivot :: V3
pivot = V3 0 100 100

-- | Degrees turned and mm moved, up to twice the most a head does in a
-- frame, in up to half a second.
genJump :: Gen (Double, Double, Double)
genJump = (,,) <$> choose (0, 2 * maxTurn 0) <*> choose (0, 2 * maxMove 0) <*> genGap

-- | No turn, no move, and no time.
shrinkJump :: (Double, Double, Double) -> [(Double, Double, Double)]
shrinkJump (turn, move, dt) = [(0, move, dt) | turn /= 0] ++ [(turn, 0, dt) | move /= 0] ++ [(turn, move, 0) | dt /= 0]

-- | Seconds since the last pose, while it predicts.
genGap :: Gen Double
genGap = choose (0, 0.5)

-- | Where the head was recentered, if anywhere.
genCentre :: Gen (Maybe Placing)
genCentre = oneof [pure Nothing, Just <$> genSeen 60 trackedRange headset]

shrinkCentre :: Maybe Placing -> [Maybe Placing]
shrinkCentre Nothing = []
shrinkCentre (Just placing) = Nothing : map Just (shrinkSeen headset placing)

-- | How each reflector showed, if it did.
genSights :: Gen (Three (Maybe Sight))
genSights = genThree (oneof [pure Nothing, Just <$> genSight 5000])

shrinkSights :: Three (Maybe Sight) -> [Three (Maybe Sight)]
shrinkSights = shrinkThree (maybe [] (\s -> Nothing : map Just (shrinkSight s)))
