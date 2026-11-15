-- | Poses solved from where the reflectors show, and level from turning.
module Test.Pose (tests) where

import Control.Monad (forM_)
import Data.Maybe (isNothing)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Test.Check
import Test.Gen
import Test.Instances ()
import Test.Simulation (clipBent, deg, headset)
import TrackKat.Config
import TrackKat.Linear
import TrackKat.Pose
import TrackKat.Synth

tests :: TestTree
tests = group "Pose" $ do
    -- Pose: project known head poses and recover them.
    let reference = Pose identity (V3 0 0 700)
        neck = V3 0 160 50
        poses =
            [ (y, p, r, dt0)
            | (y, p, r) <- [(0, 0, 0), (20, 0, 0), (-35, 10, 0), (10, -15, 8), (0, 0, -20), (40, 20, 10)]
            , dt0 <- [V3 0 0 0, V3 30 (-20) 50]
            ]
    forM_ poses $ \(y, p, r, dt) -> do
        let truth = Pose (rotYXZ y p r) (V3 0 0 700 ^+^ dt)
            img = (project defaultIntrinsics truth (mLeft trackClip), project defaultIntrinsics truth (mRight trackClip), project defaultIntrinsics truth (mTop trackClip))
            -- Start from a pose a few degrees and mm off, as frame-to-frame tracking would.
            guess = Pose (rotYXZ (y + 3) (p - 2) (r + 2)) (V3 0 0 710 ^+^ dt)
        case solvePose defaultIntrinsics trackClip (Just guess) img of
            Nothing -> checkShowing "solve" (show (y, p, r)) False
            Just (sol, err) -> do
                let hp = relativePose cameraLevel neck reference sol
                    V3 ex ey ez = poseAt truth neck ^-^ (neck ^+^ V3 0 0 700)
                checkShowing "reprojection" (show (y, p, r, err)) (err < 1e-3)
                check
                    ("angles " ++ show (y, p, r) ++ " got " ++ show hp)
                    (all (< 0.01) [abs (yaw hp - y), abs (pitch hp - p), abs (roll hp - r)])
                checkShowing "translation" (show hp) (all (< 0.1) [abs (tx hp - ex), abs (ty hp - ey), abs (tz hp - ez)])

    -- With no previous pose, start from "facing the camera"; fine for modest angles.
    forM_ [(15, 5, 0), (-10, -10, 5)] $ \(y, p, r) -> do
        let truth = Pose (rotYXZ y p r) (V3 40 (-30) 650)
            img = (project defaultIntrinsics truth (mLeft trackClip), project defaultIntrinsics truth (mRight trackClip), project defaultIntrinsics truth (mTop trackClip))
        case solvePose defaultIntrinsics trackClip Nothing img of
            Just (sol, _) ->
                let hp = relativePose cameraLevel neck reference sol
                 in check
                        ("cold start " ++ show (y, p, r) ++ " got " ++ show hp)
                        (all (< 0.01) [abs (yaw hp - y), abs (pitch hp - p), abs (roll hp - r)])
            Nothing -> check "cold start solved" False

    -- Headset clip, post leaning back: from a cold start the solver should pick
    -- the right one of the two mirror-image solutions.
    forM_ [(0, 0, 0), (12, -8, 0), (-10, 10, 5)] $ \(y, p, r) -> do
        let truth = Pose (rotYXZ y p r) (V3 20 (-150) 700)
            img = (project defaultIntrinsics truth (mLeft headset), project defaultIntrinsics truth (mRight headset), project defaultIntrinsics truth (mTop headset))
        case solvePose defaultIntrinsics headset Nothing img of
            Just (sol, _) ->
                let hp = relativePose cameraLevel neck reference sol
                 in check
                        ("headset cold start " ++ show (y, p, r) ++ " got " ++ show hp)
                        (all (< 0.01) [abs (yaw hp - y), abs (pitch hp - p), abs (roll hp - r)])
            Nothing -> check "headset cold start solved" False

    -- Level axes from turning the head left and right, seen by a camera tipped
    -- 13 degrees down and 4 to its right.
    let tilt = rodrigues (V3 0 0 (deg 4)) `mulMM` rodrigues (V3 (deg 13) 0 0)
        seenFrom rLevel tLevel = Pose (tilt `mulMM` rLevel) (mulMV tilt tLevel)
        neutral = seenFrom identity (V3 0 0 700)
        observe truth =
            let img = (project defaultIntrinsics truth (mLeft headset), project defaultIntrinsics truth (mRight headset), project defaultIntrinsics truth (mTop headset))
             in fst <$> solvePose defaultIntrinsics headset (Just truth) img
        relativeRot pose = poseR pose `mulMM` transpose (poseR neutral)
        sweep = [seenFrom (rotYXZ y 0 0) (V3 0 0 700) | y <- [-40, -30 .. 40]]
    case levelFromYaws . map relativeRot =<< traverse observe sweep of
        Nothing -> check "level from yaws" False
        Just level -> do
            let (down, right) = cameraTilt level
            checkShowing "camera tilt" (show (down, right)) (abs (down - 13) < 0.01 && abs (right - 4) < 0.01)
            forM_ [(20, 10, 5), (-30, -15, 0), (0, 0, 25)] $ \(y, p, r) ->
                case observe (seenFrom (rotYXZ y p r) (V3 0 0 700)) of
                    Nothing -> check "levelled pose solved" False
                    Just sol -> do
                        let hp = relativePose level neck neutral sol
                        check
                            ("levelled pose " ++ show (y, p, r) ++ " got " ++ show hp)
                            (all (< 0.01) [abs (yaw hp - y), abs (pitch hp - p), abs (roll hp - r)])
    check "too little turning" $ isNothing (levelFromYaws [identity, rotYXZ 2 0 0])

    check "identify" $ identify trackClip id [(297, 154), (254, 122), (249, 154)] == Just ((249, 154), (297, 154), (254, 122))

    -- The clip's sides are the wearer's; the model's are the camera's.
    case clipShape 70 100 120 of
        Just cl ->
            let m = clipModel cl
             in check "clip sides are the wearer's" $
                    abs (norm (mTop m ^-^ mLeft m) - 120) < 1e-9 && abs (norm (mTop m ^-^ mRight m) - 100) < 1e-9
        Nothing -> check "asymmetric clip" False
    tree properties

-- | Lengths here are up to a few metres, and right to 1e-9 mm; angles are
-- right to 1e-5 degrees (see Note [Tolerances of turns] in "Test.Linear").
properties :: TestTree
properties =
    testGroup
        "properties"
        [ testProperty "a pose moves the model rigidly: its origin to where the pose is, distances kept" $
            \placing (Point a) (Point b) ->
                let pose = placed placing
                 in poseAt pose (V3 0 0 0) === poseT pose .&&. close 1e-9 (norm (poseAt pose a ^-^ poseAt pose b)) (norm (a ^-^ b))
        , testProperty "where a point shows depends only on which way it is from the camera" $
            \(SquareOn (Placing _ at)) (Nearer nearer) ->
                let shown t = project defaultIntrinsics (Pose identity t) (V3 0 0 0)
                 in closePoint 1e-9 (shown (nearer *^ at)) (shown at)
        , testProperty "a point straight ahead shows at the picture's centre" $
            \(Ahead z) ->
                project defaultIntrinsics (Pose identity (V3 0 0 z)) (V3 0 0 0) === (cx defaultIntrinsics, cy defaultIntrinsics)
        , testProperty "a pose is no way from itself, and as far from another as the other is from it" $
            \a b (Point pivot) ->
                poseChange pivot (placed a) (placed a) === (0, 0) .&&. poseChange pivot (placed a) (placed b) === poseChange pivot (placed b) (placed a)
        , testProperty "how far apart poses are doesn't depend on where the camera is" $
            \a b camera (Point pivot) ->
                let moved = movedCamera (placed camera)
                 in closeChange (poseChange pivot (moved (placed a)) (moved (placed b))) (poseChange pivot (placed a) (placed b))
        , testProperty "by way of a third pose is no shorter" $
            \a b c (Point pivot) ->
                let (turn, move) = poseChange pivot (placed a) (placed c)
                    (turn1, move1) = poseChange pivot (placed a) (placed b)
                    (turn2, move2) = poseChange pivot (placed b) (placed c)
                 in property (turn <= turn1 + turn2 + 1e-5 && move <= move1 + move2 + 1e-9)
        , testProperty "turning about the pivot is turning alone, by the turn made" $
            \reference (Point pivot) (Angles (y, p, r)) ->
                let hp = relativePose cameraLevel pivot (placed reference) (turnedAbout pivot (rotYXZ y p r) (placed reference))
                 in conjoin
                        [ close 1e-7 (yaw hp) y
                        , close 1e-7 (pitch hp) p
                        , close 1e-7 (roll hp) r
                        , closeV 1e-9 (V3 (tx hp) (ty hp) (tz hp)) (V3 0 0 0)
                        ]
        , testProperty "a head's pose from its reference turns and moves as far as the poses are apart" $
            \a b (Point pivot) (Tipped tipped) ->
                let up = mulMV (rodrigues tipped) (V3 0 (-1) 0)
                    hp = relativePose (levelFromUp up) pivot (placed a) (placed b)
                    (_, move) = poseChange pivot (placed a) (placed b)
                 in close 1e-9 (norm (V3 (tx hp) (ty hp) (tz hp))) move
                        .&&. closeM 1e-9 (rotYXZ (yaw hp) (pitch hp) (roll hp)) (levelTurn up `mulMM` poseR (placed b) `mulMM` transpose (poseR (placed a)) `mulMM` transpose (levelTurn up))
        , testProperty "level axes have the up they were made from, and tell the camera's tilt" $
            \(Tilt down) (Tilt right) ->
                let up = mulMV (rotYXZ 0 0 right `mulMM` rotYXZ 0 down 0) (V3 0 (-1) 0)
                    (toldDown, toldRight) = cameraTilt (levelFromUp up)
                 in closeV 1e-12 (levelUp (levelFromUp up)) up .&&. close 1e-9 toldDown down .&&. close 1e-9 toldRight right
        , testProperty "a clip mounted one way, then another, is as mounted the other way at once, its shape kept" $
            \(Mount (Placing turn1 pivot1)) (Mount (Placing turn2 pivot2)) (AClip m) ->
                let direct = mounted (Mounting (rodrigues turn2) pivot2) m
                    twice = mounted (Mounting (rodrigues turn2) pivot2) (mounted (Mounting (rodrigues turn1) pivot1) m)
                    (lr, lt, rt) = wornDistances m
                    (lr', lt', rt') = wornDistances direct
                 in conjoin
                        [ closeV 1e-9 (mLeft twice) (mLeft direct)
                        , closeV 1e-9 (mRight twice) (mRight direct)
                        , closeV 1e-9 (mTop twice) (mTop direct)
                        , closeV 1e-9 (V3 lr' lt' rt') (V3 lr lt rt)
                        , closeV 1e-12 (clipBack direct) (mulMV (rodrigues turn2) (V3 0 0 1))
                        ]
        , testProperty "a pose reflected is a pose, its reflectors mirrored across the line of sight about their centre" $
            \(ClipSeen m placing) ->
                let pose = placed placing
                    mirrored = reflectedPose m pose
                    reflectors = [mLeft m, mRight m, mTop m]
                    centre p = (1 / 3) *^ foldr ((^+^) . poseAt p) (V3 0 0 0) reflectors
                    sight = normalize (centre pose)
                    across v = v ^-^ (2 * dot v sight) *^ sight
                    M33 a b c = poseR mirrored
                 in conjoin $
                        [ closeM 1e-12 (poseR mirrored `mulMM` transpose (poseR mirrored)) identity
                        , closeV 1e-12 (cross a b) c
                        , closeV 1e-9 (centre mirrored) (centre pose)
                        ]
                            ++ [closeV 1e-9 (poseAt mirrored p ^-^ centre pose) (across (poseAt pose p ^-^ centre pose)) | p <- reflectors]
        , testProperty "a pose reflected twice is the pose" $
            \(ClipSeen m placing) ->
                let pose = placed placing
                    back = reflectedPose m (reflectedPose m pose)
                 in closeM 1e-12 (poseR back) (poseR pose) .&&. closeV 1e-9 (poseT back) (poseT pose)
        , testProperty "which blob is which reflector doesn't depend on the order blobs come in" $
            \(AClip m) (InPicture a) (InPicture b) (InPicture c) -> forAnyOrder [a, b, c] $ \shuffled ->
                identify m id shuffled === identify m (id :: (Double, Double) -> (Double, Double)) [a, b, c]
        , -- Solving stops once a step gains less than a hundredth of a
          -- pixel, squared, so the pose it stops at shows the reflectors
          -- within that of where they were seen; how near the true pose
          -- that is depends on how squarely the triangle faces the camera,
          -- and is some hundredths of a degree and tenths of a mm at
          -- worst. A degree and a mm tell it from any other pose that
          -- fits: the mirror image is tens of degrees away.
          testProperty "a clip seen from a pose near it is solved to that pose" $
            \(ClipFacing m placing) (Nudge nudge) ->
                let truth = placed placing
                    seen = (project defaultIntrinsics truth (mLeft m), project defaultIntrinsics truth (mRight m), project defaultIntrinsics truth (mTop m))
                 in case solvePose defaultIntrinsics m (Just (nudged nudge truth)) seen of
                        Nothing -> counterexample "not solved" False
                        Just (solved, misfit) ->
                            let (turn, move) = poseChange (V3 0 0 0) truth solved
                             in counterexample (show (misfit, turn, move)) (misfit < 0.01 && turn < 1 && move < 1)
        , testProperty "a clip's reflectors are as far apart as it was made, the lower pair level about the middle" $
            \(Sides (lr, lt, rt)) ->
                case clipShape lr lt rt of
                    Nothing -> counterexample "not a clip" False
                    Just made ->
                        let m = clipModel made
                            (lr', lt', rt') = wornDistances m
                         in conjoin
                                [ clipDistances made === (lr, lt, rt)
                                , closeV (1e-12 * (lr + lt + rt)) (V3 lr' lt' rt') (V3 lr lt rt)
                                , mLeft m === (-1) *^ mRight m
                                , mRight m === V3 (lr / 2) 0 0
                                ]
        , testProperty "distances that don't make a triangle make no clip" $
            \(Sides (a, b, _)) (Beyond beyond) ->
                clipShape a b (beyond * (a + b)) === Nothing .&&. clipShape (beyond * (a + b)) a b === Nothing .&&. clipShape a (beyond * (a + b)) b === Nothing
        ]

-- | That two points of the picture are within a tolerance of each other.
closePoint :: Double -> (Double, Double) -> (Double, Double) -> Property
closePoint slack (u, v) (u', v') = close slack u u' .&&. close slack v v'

-- | That two changes of pose (see 'poseChange') are the same.
closeChange :: (Double, Double) -> (Double, Double) -> Property
closeChange (turn, move) (turn', move') = close 1e-5 turn turn' .&&. close 1e-9 move move'

-- | A point of a clip or a head, up to 300 mm each way from the clip's
-- origin.
newtype Point = Point V3
    deriving (Show)

instance Arbitrary Point where
    arbitrary = Point <$> genV3 300
    shrink (Point p) = Point <$> shrinkV3 p

-- | Where a TrackClip squarely facing the camera is seen whole, within the
-- range a clip is tracked in.
newtype SquareOn = SquareOn Placing
    deriving (Show)

instance Arbitrary SquareOn where
    arbitrary = SquareOn <$> genSeen 0 trackedRange trackClip
    shrink (SquareOn placing) = SquareOn <$> shrinkSeen trackedRange trackClip placing

-- | How many times as far from the camera, from half to twice.
newtype Nearer = Nearer Double
    deriving (Show)

instance Arbitrary Nearer where
    arbitrary = Nearer <$> choose (0.5, 2)
    shrink (Nearer times) = Nearer <$> shrinkWithin (0.5, 2) 1 times

-- | How far straight ahead of the camera, up to 3 m.
newtype Ahead = Ahead Double
    deriving (Show)

instance Arbitrary Ahead where
    arbitrary = Ahead <$> choose (1, 3000)
    shrink (Ahead z) = Ahead <$> shrinkWithin (1, 3000) 1 z

-- | The clips the examples above use: a TrackClip, and headset clips.
clips :: [Model]
clips = [trackClip, headset, clipBent]

-- | One of 'clips'; simpler the earlier of them.
newtype AClip = AClip Model
    deriving (Show)

instance Arbitrary AClip where
    arbitrary = AClip <$> elements clips
    shrink (AClip m) = AClip <$> earlier clips m

-- | One of 'clips' at a pose the camera sees it whole in, within the range
-- a clip is tracked in, turned any way.
data ClipSeen = ClipSeen Model Placing
    deriving (Show)

instance Arbitrary ClipSeen where
    arbitrary = elements clips >>= \m -> ClipSeen m <$> genSeen 180 trackedRange m
    shrink (ClipSeen m placing) = [ClipSeen m' placing | m' <- earlier clips m, inView m' placing] ++ (ClipSeen m <$> shrinkSeen trackedRange m placing)

-- | One of 'clips' as 'ClipSeen' has it, turned up to 25 degrees from
-- squarely facing the camera.
data ClipFacing = ClipFacing Model Placing
    deriving (Show)

instance Arbitrary ClipFacing where
    arbitrary = elements clips >>= \m -> ClipFacing m <$> genSeen 25 trackedRange m
    shrink (ClipFacing m placing) = [ClipFacing m' placing | m' <- earlier clips m, inView m' placing] ++ (ClipFacing m <$> shrinkSeen trackedRange m placing)

-- | Degrees of yaw, pitch and roll, short of where they stop telling a
-- turn apart: pitch straight up or down, and yaw or roll half a turn.
newtype Angles = Angles (Double, Double, Double)
    deriving (Show)

instance Arbitrary Angles where
    arbitrary = Angles <$> ((,,) <$> choose (-170, 170) <*> choose (-80, 80) <*> choose (-170, 170))
    shrink (Angles angles) = Angles <$> shrink angles

-- | A turn of up to 60 degrees, as the camera is tipped from level by.
newtype Tipped = Tipped V3
    deriving (Show)

instance Arbitrary Tipped where
    arbitrary = Tipped <$> genTurn 60
    shrink (Tipped w) = Tipped <$> shrinkTurn w

-- | Degrees the camera is tilted down or to its right, up to 60 either
-- way.
newtype Tilt = Tilt Double
    deriving (Show)

instance Arbitrary Tilt where
    arbitrary = Tilt <$> choose (-60, 60)
    shrink (Tilt degrees) = Tilt <$> shrinkWithin (-60, 60) 0 degrees

-- | How a clip is mounted: turned any way, about a pivot up to 300 mm each
-- way from its origin.
newtype Mount = Mount Placing
    deriving (Show)

instance Arbitrary Mount where
    arbitrary = Mount <$> genPlacing 300
    shrink (Mount placing) = Mount <$> shrinkPlacing placing

-- | The turn from the camera's axes to level axes with the given up: its
-- rows are level right, down and forward (see 'levelFromUp').
levelTurn :: V3 -> M33
levelTurn up = M33 (cross down forward) down forward
  where
    down = (-1) *^ normalize up
    forward = normalize (V3 0 0 1 ^-^ (dot (V3 0 0 1) down *^ down))

-- | How far a guess is from a pose: up to 3 degrees and 10 mm each way, as
-- a pose is from the frame before's.
newtype Nudge = Nudge Placing
    deriving (Show)

instance Arbitrary Nudge where
    arbitrary = Nudge <$> (Placing <$> genTurn 3 <*> genV3 10)
    shrink (Nudge placing) = Nudge <$> shrinkPlacing placing

nudged :: Placing -> Pose -> Pose
nudged (Placing turn move) (Pose r t) = Pose (rodrigues turn `mulMM` r) (t ^+^ move)

-- | The sides of a triangle, each well short of the other two together;
-- simpler with whole sides, where they still make one.
newtype Sides = Sides (Double, Double, Double)
    deriving (Show)

instance Arbitrary Sides where
    arbitrary = do
        a <- choose (10, 300)
        b <- choose (10, 300)
        c <- choose (abs (a - b) + 0.01 * (a + b), 0.99 * (a + b))
        pure (Sides (a, b, c))
    shrink (Sides (a, b, c)) =
        [ Sides (a', b', c')
        | (a', b', c') <- [(whole a, b, c), (a, whole b, c), (a, b, whole c)]
        , (a', b', c') /= (a, b, c)
        , c' > abs (a' - b') + 0.01 * (a' + b') && c' < 0.99 * (a' + b')
        ]
      where
        whole x = fromIntegral (round x :: Int)

-- | How many times the other two sides together a side too long for a
-- triangle is.
newtype Beyond = Beyond Double
    deriving (Show)

instance Arbitrary Beyond where
    arbitrary = Beyond <$> choose (1.001, 3)
    shrink (Beyond times) = Beyond <$> shrinkWithin (1.001, 3) 2 times
