-- | Generators, each with the shrinks that go with it, and comparisons to
-- within a tolerance, for properties of several subjects.
module Test.Gen (
    -- * Comparing
    close,
    closeV,
    closeM,

    -- * Vectors and turns
    genV3,
    shrinkV3,
    genDirection,
    genTurn,
    shrinkTurn,
    degreesOf,

    -- * Poses
    Placing (..),
    placed,
    genPlacing,
    shrinkPlacing,
    trackedRange,
    genSeen,
    shrinkSeen,
    inView,
    movedCamera,
    turnedAbout,
) where

import Test.Tasty.QuickCheck

import TrackKat.Frame (frameHeight, frameWidth)
import TrackKat.Linear
import TrackKat.Pose
import TrackKat.World (World (..), inMillimetres, world)

-- | That two numbers are within a tolerance of each other.
close :: Double -> Double -> Double -> Property
close slack a b =
    counterexample (show a ++ " and " ++ show b ++ " are more than " ++ show slack ++ " apart") (abs (a - b) <= slack)

-- | That two vectors are within a tolerance of each other.
closeV :: Double -> V3 -> V3 -> Property
closeV slack a b =
    counterexample (show a ++ " and " ++ show b ++ " are more than " ++ show slack ++ " apart") (norm (a ^-^ b) <= slack)

-- | That each row of two matrices is within a tolerance of the other's.
closeM :: Double -> M33 -> M33 -> Property
closeM slack (M33 a b c) (M33 x y z) = conjoin [closeV slack a x, closeV slack b y, closeV slack c z]

-- | A vector with each component no bigger than the given size.
genV3 :: Double -> Gen V3
genV3 size = V3 <$> component <*> component <*> component
  where
    component = choose (negate size, size)

-- | Vectors with a component zeroed or rounded to a whole number.
shrinkV3 :: V3 -> [V3]
shrinkV3 (V3 x y z) = [V3 x' y z | x' <- simpler x] ++ [V3 x y' z | y' <- simpler y] ++ [V3 x y z' | z' <- simpler z]
  where
    simpler c = [0 | c /= 0] ++ [whole | let whole = fromIntegral (truncate c :: Int), whole /= c, whole /= 0]

-- | A unit vector.
genDirection :: Gen V3
genDirection = normalize <$> genV3 1 `suchThat` ((> 0.1) . norm)

-- | A turn, as the vector along its axis as long as its angle in radians
-- (see 'rodrigues'), of up to the given degrees.
genTurn :: Double -> Gen V3
genTurn most = (\axis angle -> (angle * pi / 180) *^ axis) <$> genDirection <*> choose (0, most)

-- | Turns about simpler axes, and by half as much and not at all.
shrinkTurn :: V3 -> [V3]
shrinkTurn w = [V3 0 0 0 | norm w > 0] ++ [0.5 *^ w | norm w > 1e-3] ++ [w' | w' <- shrinkV3 w, norm w' <= norm w]

-- | The angle of a turn, in degrees.
degreesOf :: V3 -> Double
degreesOf w = norm w * 180 / pi

-- * Poses

-- | A pose as what to make it from: its turn, as 'genTurn' gives one, and
-- where it puts the model's origin.
data Placing = Placing V3 V3
    deriving (Show)

placed :: Placing -> Pose
placed (Placing turn at) = Pose (rodrigues turn) at

-- | Any rigid motion: a turn of up to half a turn, and a move of up to the
-- given mm each way.
genPlacing :: Double -> Gen Placing
genPlacing size = Placing <$> genTurn 180 <*> genV3 size

-- | Motions that turn less and move less.
shrinkPlacing :: Placing -> [Placing]
shrinkPlacing (Placing turn at) = [Placing turn' at | turn' <- shrinkTurn turn] ++ [Placing turn at' | at' <- shrinkV3 at]

-- | The nearest and farthest the world has a clip tracked, mm.
trackedRange :: (Double, Double)
trackedRange = (inMillimetres (cameraNearest world), inMillimetres (cameraFarthest world))

-- | A pose of a clip that the camera sees whole: turned from squarely
-- facing the camera by up to the given degrees, between the given nearest
-- and farthest mm from it, every reflector in the picture.
genSeen :: Double -> (Double, Double) -> Model -> Gen Placing
genSeen most range m = seen `suchThat` inView m
  where
    k = defaultIntrinsics
    seen = do
        turn <- genTurn most
        distance <- choose range
        u <- choose (0, fromIntegral frameWidth)
        v <- choose (0, fromIntegral frameHeight)
        pure (Placing turn (distance *^ normalize (V3 ((u - cx k) / focal k) ((v - cy k) / focal k) 1)))

-- | Whether a pose has every reflector of a clip in front of the camera
-- and in its picture.
inView :: Model -> Placing -> Bool
inView m placing = all shown [mLeft m, mRight m, mTop m]
  where
    pose = placed placing
    shown p =
        let V3 _ _ z = poseAt pose p
            (u, v) = project defaultIntrinsics pose p
         in z > 0 && u >= 0 && u < fromIntegral frameWidth && v >= 0 && v < fromIntegral frameHeight

-- | Poses turned less, and nearer the middle of the picture, still seen
-- whole.
shrinkSeen :: Model -> Placing -> [Placing]
shrinkSeen m (Placing turn at@(V3 _ _ z)) =
    filter (inView m) ([Placing turn' at | turn' <- shrinkTurn turn] ++ [Placing turn (V3 0 0 z) | at /= V3 0 0 z])

-- | A pose as a camera itself moved would see it: the motion after the
-- pose.
movedCamera :: Pose -> Pose -> Pose
movedCamera (Pose turn move) (Pose r t) = Pose (turn `mulMM` r) (mulMV turn t ^+^ move)

-- | A pose with the head turned about a point of the model, which stays
-- where it was: the turn is in the camera's axes.
turnedAbout :: V3 -> M33 -> Pose -> Pose
turnedAbout pivot turn pose@(Pose r _) = Pose turned (poseAt pose pivot ^-^ mulMV turned pivot)
  where
    turned = turn `mulMM` r
