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

    -- * Settings
    genWritten,
    written,
    genBinding,
    shrinkBinding,
    genConfig,
    shrinkConfig,
) where

import Data.Maybe (fromMaybe)
import Test.Tasty.QuickCheck

import TrackKat.Config
import TrackKat.Frame (frameHeight, frameWidth)
import TrackKat.Lights (Masking (..))
import TrackKat.Linear
import TrackKat.Number
import TrackKat.Pose
import TrackKat.Response
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

-- * Settings

-- | A number as trackkat's text files give one: so many ten-thousandths,
-- between the given numbers of them, as 'parseNumber' reads them written
-- out, which isn't always the nearest number to them.
genWritten :: (Int, Int) -> Gen Double
genWritten range = written <$> chooseInt range

written :: Int -> Double
written tenThousandths = fromMaybe (error "written") (parseNumber (renderNumber (fromIntegral tenThousandths / 10000)))

-- | A key or mouse button with modifiers, as trackkat's window gives one
-- from Windows' codes, or a joystick's button.
genBinding :: Gen Binding
genBinding = oneof [key, JoystickButton <$> (Joystick <$> joystickName <*> chooseInt (1, 4)) <*> chooseInt (1, maxButton)]
  where
    key = elements [b | mask <- [0 .. 15], code <- [0 .. 255], Just b <- [bindingFromCodes mask code]]
    joystickName = unwords <$> listOf1 (elements ["Throttle", "Joystick", "-", "HOTAS", "Warthog", "T.16000M", "VPC", "MT-50CM2", "(HID)", "button", "Zoë's", "3D", "2"])

-- | Bindings with fewer modifiers, a shorter joystick's name, and the
-- first joystick and button.
shrinkBinding :: Binding -> [Binding]
shrinkBinding (Key mods k) = [Key (filter (/= m) mods) k | m <- mods]
shrinkBinding (JoystickButton (Joystick name index) button) =
    [JoystickButton (Joystick (unwords shorter) index) button | shorter <- shrinkList (const []) (words name), not (null shorter)]
        ++ [JoystickButton (Joystick name 1) button | index /= 1]
        ++ [JoystickButton (Joystick name index) 1 | button /= 1]

-- | Settings a settings file can say: numbers as it writes them, smoothing
-- from its two knobs, each curve's points going up from above zero or a
-- multiplier with a precision over the axis's own range, and a clip that
-- is a triangle. (See the tests' report for the settings it can't say.)
genConfig :: Gen Config
genConfig = do
    recenter <- bindings
    pause <- bindings
    trim <- bindings
    smooth <- oneof [pure NoSmoothing, tuned <$> genWritten (0, 1000000) <*> genWritten (0, 1000000)]
    curves <-
        Response smooth
            <$> curve yawRange
            <*> curve pitchRange
            <*> curve rollRange
            <*> curve leanRange
            <*> curve leanRange
            <*> curve leanRange
    leaning <- elements [LeanWithView, LeanWithScreen]
    worn <- clip' `suchThatMap` id
    Config recenter pause trim (curves leaning) worn
        <$> elements [TrackKatNPClient, TrackIRNPClient]
        <*> elements [UpdatesOn, UpdatesOff]
        <*> elements [StartAtLogin, NoStartAtLogin]
        <*> elements [MaskLights, NoMasking]
        <*> elements [VirtualView, PhysicalView, CameraView]
  where
    bindings = scale (`div` 10) (listOf genBinding)
    curve range =
        oneof
            [ do
                steps <- scale (`div` 10) (listOf1 (chooseInt (1, 500000)))
                Points <$> mapM (\input -> (,) (written input) <$> genWritten (-1000000, 1000000)) (drop 1 (scanl (+) 0 steps))
            , (\m p -> Expo (Multiplier m p range)) <$> genWritten (1, 200000) <*> genWritten (0, 1000000)
            ]
    clip' = do
        lr <- genWritten (100000, 3000000)
        lt <- genWritten (100000, 3000000)
        clipShape lr lt <$> genWritten (100000, 3000000)

-- | Settings with one more of them as the default has it, and with fewer
-- and simpler bindings.
shrinkConfig :: Config -> [Config]
shrinkConfig c =
    filter
        (/= c)
        [ c{response = r{smoothing = smoothing dr}}
        , c{response = r{yawCurve = yawCurve dr}}
        , c{response = r{pitchCurve = pitchCurve dr}}
        , c{response = r{rollCurve = rollCurve dr}}
        , c{response = r{xCurve = xCurve dr}}
        , c{response = r{yCurve = yCurve dr}}
        , c{response = r{zCurve = zCurve dr}}
        , c{response = r{lean = lean dr}}
        , c{clip = clip d}
        , c{npclient = npclient d}
        , c{updates = updates d}
        , c{startup = startup d}
        , c{lightMasking = lightMasking d}
        , c{headView = headView d}
        ]
        ++ [c{recenterBindings = bs} | bs <- shrinkList shrinkBinding (recenterBindings c)]
        ++ [c{pauseBindings = bs} | bs <- shrinkList shrinkBinding (pauseBindings c)]
        ++ [c{trimBindings = bs} | bs <- shrinkList shrinkBinding (trimBindings c)]
  where
    d = defaultConfig
    r = response c
    dr = response d
