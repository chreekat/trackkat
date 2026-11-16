-- | What properties of several subjects are about: a type for each kind of
-- thing they hold of, made up and made simpler by its 'Arbitrary'
-- instance; the generators and shrinks those are made of; and comparisons
-- to within a tolerance. (See "Test.Instances" for trackkat's own types.)
module Test.Gen (
    -- * Comparing
    close,
    closeV,
    closeM,

    -- * Numbers and choices
    shrinkWithin,
    shrinkFrom,
    earlier,
    shrinkTo,
    shrinkParts,
    Fraction (..),
    FrameGap (..),
    forAnyOrder,
    forSomeOf,
    forOneOf,

    -- * Vectors and turns
    genV3,
    shrinkV3,
    genDirection,
    shrinkDirection,
    genTurn,
    shrinkTurn,
    degreesOf,
    Turn (..),

    -- * Poses
    Placing (..),
    placed,
    genPlacing,
    shrinkPlacing,
    trackedRange,
    genSeen,
    squarelyAhead,
    shrinkSeen,
    inView,
    movedCamera,
    turnedAbout,
    SeenWhole (..),

    -- * Scores and beliefs
    Likely (..),
    genWeights,
    shares,
    shrinkBelief,

    -- * What the camera shows
    InPicture (..),
    shrinkPlace,
    clipBlobs,
    genClipApart,
    reflectorsApart,
    shrinkClipApart,
    Apart20 (..),
    Apart30 (..),
    genBlob,
    roundBlob,
    shrinkBlob,
    Stray (..),
    Strays (..),
    ReflectorBlobs (..),
    genSizes,
    shrinkSizes,
    genSight,
    shrinkSight,
    ReflectorSights (..),
    genThree,
    shrinkThree,
    Sizes (..),

    -- * Settings
    genWritten,
    shrinkWritten,
    written,
    genPoints,
    shrinkPoints,
    genKeyOrButton,
    shrinkKeyOrButton,
    genBinding,
    shrinkBinding,
    genConfig,
    shrinkConfig,
    Sayable (..),
) where

import Data.Maybe (fromMaybe)
import Test.Tasty.QuickCheck

import Test.Simulation (headset)
import TrackKat.Belief (Belief (..), unknown)
import TrackKat.Config
import TrackKat.Frame (Blob (..), frameHeight, frameWidth)
import TrackKat.Lights (Masking (..))
import TrackKat.Linear
import TrackKat.Match (Kind (..), Sight (..), Size (..), Three (..), noSize)
import TrackKat.Number
import TrackKat.Pose
import TrackKat.Response
import TrackKat.Synth (allLit, pointDiscs, renderDiscs)
import TrackKat.Term (Score (..))
import TrackKat.Tracker.Blobs (findBlobs)
import TrackKat.World (Seconds, World (..), inMillimetres, inSeconds, seconds, world)

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

-- * Numbers and choices

-- | Numbers of a range simpler than one of them: the simplest of the
-- range, and the whole number toward nothing, where the range has it.
shrinkWithin :: (Double, Double) -> Double -> Double -> [Double]
shrinkWithin (least, most) simplest x =
    [simplest | x /= simplest]
        ++ [whole | let whole = fromIntegral (truncate x :: Int), whole /= x, whole /= simplest, whole >= least, whole <= most]

-- | Whole numbers nearer the least of a range.
shrinkFrom :: Int -> Int -> [Int]
shrinkFrom least n = map (+ least) (shrink (n - least))

-- | The values that come before one in a list of them all, as simpler.
earlier :: (Eq a) => [a] -> a -> [a]
earlier every x = takeWhile (/= x) every

-- Note [Simplest first]
-- ~~~~~~~~~~~~~~~~~~~~~
-- QuickCheck tries what a shrink gives in order, takes the first that still
-- fails, and starts again from that. So every shrink here gives its
-- domain's simplest value first, which most failures still fail at, and
-- which ends the shrinking at once; then big steps (fewer members, a
-- simpler kind of thing); then steps in one part. Each step is simpler by
-- some measure that can't go on falling, so that shrinking ends; none is
-- the value itself, or outside the domain. "Test.Shrink" checks as much of
-- each domain shared here.
--
-- 'shrinkTo' is for a domain whose simplest value is known, and
-- 'shrinkParts' for one made of parts, each with a shrink of its own that
-- keeps to this.

-- | Simpler values of a domain: its simplest, and then the steps given
-- that are neither that nor the value itself; of the simplest, none. See
-- Note [Simplest first].
shrinkTo :: (Eq a) => a -> (a -> [a]) -> a -> [a]
shrinkTo simplest steps x
    | x == simplest = []
    | otherwise = simplest : [step | step <- steps x, step /= simplest, step /= x]

-- | Simpler values of something made of parts, given the ways to make it
-- simpler in each part alone: every part at its simplest at once, and
-- then each way's values. A way's first value is taken to be its part at
-- its simplest. See Note [Simplest first].
shrinkParts :: [a -> [a]] -> a -> [a]
shrinkParts ways x = [foldl simplestBy x ways | length (filter (not . null . ($ x)) ways) > 1] ++ concatMap ($ x) ways
  where
    simplestBy y way = case way y of
        simplest : _ -> simplest
        [] -> y

-- | A share of something, from none of it to all.
newtype Fraction = Fraction Double
    deriving (Eq, Show)

instance Arbitrary Fraction where
    arbitrary = Fraction <$> choose (0, 1)
    shrink (Fraction share) = Fraction <$> shrinkWithin (0, 1) 0 share

-- | The time from one frame to the next that follows it: from a frame of
-- the camera's fastest to half a second.
newtype FrameGap = FrameGap Seconds
    deriving (Eq, Show)

instance Arbitrary FrameGap where
    arbitrary = FrameGap . seconds <$> choose (1 / 120, 0.5)
    shrink (FrameGap dt) = FrameGap . seconds <$> shrinkWithin (1 / 120, 0.5) (1 / 120) (inSeconds dt)

-- | That something holds of a list in any order; simplest in the order it
-- came.
forAnyOrder :: (Eq a, Show a, Testable prop) => [a] -> ([a] -> prop) -> Property
forAnyOrder xs = forAllShrink (shuffle xs) (\shuffled -> [xs | shuffled /= xs])

-- | That something holds of any of a list's members, in their order;
-- simpler of fewer.
forSomeOf :: (Show a, Testable prop) => [a] -> ([a] -> prop) -> Property
forSomeOf xs = forAllShrink (sublistOf xs) (shrinkList (const []))

-- | That something holds of each of a list's members; the earlier ones
-- simpler.
forOneOf :: (Eq a, Show a, Testable prop) => [a] -> (a -> prop) -> Property
forOneOf xs = forAllShrink (elements xs) (earlier xs)

-- * Vectors and turns

-- | A vector with each component no bigger than the given size.
genV3 :: Double -> Gen V3
genV3 size = V3 <$> component <*> component <*> component
  where
    component = choose (negate size, size)

-- | No vector at all, and vectors with a component zeroed or rounded to a
-- whole number.
shrinkV3 :: V3 -> [V3]
shrinkV3 =
    shrinkParts
        [ \(V3 x y z) -> [V3 x' y z | x' <- simpler x]
        , \(V3 x y z) -> [V3 x y' z | y' <- simpler y]
        , \(V3 x y z) -> [V3 x y z' | z' <- simpler z]
        ]
  where
    simpler c = [0 | c /= 0] ++ [whole | let whole = fromIntegral (truncate c :: Int), whole /= c, whole /= 0]

-- | A unit vector.
genDirection :: Gen V3
genDirection = normalize <$> genV3 1 `suchThat` ((> 0.1) . norm)

-- | The axes, the first of them simplest.
shrinkDirection :: V3 -> [V3]
shrinkDirection = earlier [V3 1 0 0, V3 0 1 0, V3 0 0 1]

-- | A turn, as the vector along its axis as long as its angle in radians
-- (see 'rodrigues'), of up to the given degrees.
genTurn :: Double -> Gen V3
genTurn most = (\axis angle -> (angle * pi / 180) *^ axis) <$> genDirection <*> choose (0, most)

-- | No turn at all, and turns by half as much and about simpler axes by
-- no more.
shrinkTurn :: V3 -> [V3]
shrinkTurn = shrinkTo (V3 0 0 0) (\w -> [0.5 *^ w | norm w > 1e-3] ++ [w' | w' <- shrinkV3 w, norm w' <= norm w])

-- | The angle of a turn, in degrees.
degreesOf :: V3 -> Double
degreesOf w = norm w * 180 / pi

-- | Any turn, of up to half a turn (see 'genTurn').
newtype Turn = Turn V3
    deriving (Eq, Show)

instance Arbitrary Turn where
    arbitrary = Turn <$> genTurn 180
    shrink (Turn w) = Turn <$> shrinkTurn w

-- * Poses

-- | A pose as what to make it from: its turn, as 'genTurn' gives one, and
-- where it puts the model's origin.
data Placing = Placing V3 V3
    deriving (Eq, Show)

placed :: Placing -> Pose
placed (Placing turn at) = Pose (rodrigues turn) at

-- | Any rigid motion: a turn of up to half a turn, and a move of up to the
-- given mm each way.
genPlacing :: Double -> Gen Placing
genPlacing size = Placing <$> genTurn 180 <*> genV3 size

-- | No motion at all, and motions that turn less and move less.
shrinkPlacing :: Placing -> [Placing]
shrinkPlacing =
    shrinkParts
        [ \(Placing turn at) -> [Placing turn' at | turn' <- shrinkTurn turn]
        , \(Placing turn at) -> [Placing turn at' | at' <- shrinkV3 at]
        ]

-- | Up to two metres each way.
instance Arbitrary Placing where
    arbitrary = genPlacing 2000
    shrink = shrinkPlacing

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

-- | The simplest pose 'genSeen' gives of the given range: squarely facing
-- the camera, straight ahead of it, a metre off or as near that as the
-- range has.
squarelyAhead :: (Double, Double) -> Placing
squarelyAhead (nearest, farthest) = Placing (V3 0 0 0) (V3 0 0 (min farthest (max nearest 1000)))

-- | Of the poses 'genSeen' gives of a range and a clip: the simplest (see
-- 'squarelyAhead'), and poses straight ahead of the camera as far from it, and
-- turned less, still seen whole.
shrinkSeen :: (Double, Double) -> Model -> Placing -> [Placing]
shrinkSeen range m = filter (inView m) . shrinkTo (squarelyAhead range) steps
  where
    steps (Placing turn at) = Placing turn (V3 0 0 (norm at)) : [Placing turn' at | turn' <- shrinkTurn turn]

-- | A pose of the headset clip that the camera sees whole, within the
-- range the clip is tracked in, turned up to 60 degrees from squarely
-- facing the camera.
newtype SeenWhole = SeenWhole Placing
    deriving (Eq, Show)

instance Arbitrary SeenWhole where
    arbitrary = SeenWhole <$> genSeen 60 trackedRange headset
    shrink (SeenWhole placing) = SeenWhole <$> shrinkSeen trackedRange headset placing

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

-- * Scores and beliefs

-- | What something scores that can be: from nothing against it, down to
-- sixty nats against.
newtype Likely = Likely Score
    deriving (Eq, Show)

instance Arbitrary Likely where
    arbitrary = Likely . Score <$> choose (-60, 0)
    shrink (Likely scored) = Likely . Score <$> shrinkWithin (-60, 0) 0 (nats scored)

-- | Four weights, of which the first isn't nothing.
genWeights :: Gen Belief
genWeights = Belief <$> choose (0.01, 1) <*> weight <*> weight <*> weight
  where
    weight = oneof [pure 0, choose (0, 1)]

-- | Weights as shares of their sum, which are a belief.
shares :: Belief -> Belief
shares b = Belief (bothTrue b / whole) (referenceMirrored b / whole) (trackedMirrored b / whole) (bothMirrored b / whole)
  where
    whole = bothTrue b + referenceMirrored b + trackedMirrored b + bothMirrored b

-- | Knowing nothing; then a belief that knows nothing of the reference,
-- and one that knows nothing of the tracked pose, each saying of the
-- other what the belief did; and one halfway to knowing nothing, each
-- only if it is some way off the belief. They are shares of weights if
-- the belief is, by more nearly equal weights.
shrinkBelief :: Belief -> [Belief]
shrinkBelief = shrinkTo unknown (\b -> filter (far b) [ofTracked b, ofReference b, halfway b])
  where
    ofTracked (Belief tt tm mt mm) = Belief ((tt + tm) / 2) ((tt + tm) / 2) ((mt + mm) / 2) ((mt + mm) / 2)
    ofReference (Belief tt tm mt mm) = Belief ((tt + mt) / 2) ((tm + mm) / 2) ((tt + mt) / 2) ((tm + mm) / 2)
    halfway (Belief tt tm mt mm) = Belief ((tt + 0.25) / 2) ((tm + 0.25) / 2) ((mt + 0.25) / 2) ((mm + 0.25) / 2)
    far (Belief tt tm mt mm) (Belief tt' tm' mt' mm') = maximum (map abs [tt - tt', tm - tm', mt - mt', mm - mm']) > 0.01

-- * What the camera shows

-- | Somewhere in the picture, in pixels; simplest at its corner, and
-- simpler at a whole number of pixels.
newtype InPicture = InPicture (Double, Double)
    deriving (Eq, Show)

instance Arbitrary InPicture where
    arbitrary = InPicture <$> ((,) <$> choose (0, 640) <*> choose (0, 480))
    shrink (InPicture place) = InPicture <$> shrinkPlace place

-- | The picture's corner, and the whole number of pixels a place is at.
shrinkPlace :: (Double, Double) -> [(Double, Double)]
shrinkPlace = shrinkTo (0, 0) (\(u, v) -> [(whole u, whole v)])
  where
    whole x = fromIntegral (truncate x :: Int)

-- | The blobs the camera shows of a clip at a pose, every reflector lit,
-- heaviest first.
clipBlobs :: Model -> Placing -> [Blob]
clipBlobs m placing = findBlobs (renderDiscs (pointDiscs defaultIntrinsics m allLit (placed placing)))

-- | A pose of a clip as 'genSeen' gives one, where the camera shows each
-- reflector as a blob of its own, whole: 40 pixels or more inside the
-- picture's edges.
genClipApart :: Double -> (Double, Double) -> Model -> Gen Placing
genClipApart most range m = genSeen most range m `suchThat` reflectorsApart m

-- | Poses as 'shrinkSeen' gives them, each reflector still a blob of its
-- own, whole.
shrinkClipApart :: (Double, Double) -> Model -> Placing -> [Placing]
shrinkClipApart range m = filter (reflectorsApart m) . shrinkSeen range m

-- | A pose of the headset clip, from 400 mm to 1.5 m off and turned up to
-- 20 degrees from squarely facing the camera, where the camera shows each
-- reflector as a blob of its own, whole (see 'genClipApart').
newtype Apart20 = Apart20 Placing
    deriving (Eq, Show)

instance Arbitrary Apart20 where
    arbitrary = Apart20 <$> genClipApart 20 (400, 1500) headset
    shrink (Apart20 placing) = Apart20 <$> shrinkClipApart (400, 1500) headset placing

-- | A pose as 'Apart20' has one, turned up to 30 degrees.
newtype Apart30 = Apart30 Placing
    deriving (Eq, Show)

instance Arbitrary Apart30 where
    arbitrary = Apart30 <$> genClipApart 30 (400, 1500) headset
    shrink (Apart30 placing) = Apart30 <$> shrinkClipApart (400, 1500) headset placing

-- | Whether the camera shows each reflector of a clip at a pose as a blob
-- of its own, 40 pixels or more inside the picture's edges.
reflectorsApart :: Model -> Placing -> Bool
reflectorsApart m placing = length blobs == 3 && all inside blobs
  where
    blobs = clipBlobs m placing
    inside b = blobX b >= 40 && blobX b <= fromIntegral frameWidth - 40 && blobY b >= 40 && blobY b <= fromIntegral frameHeight - 40

-- | A round blob of up to the given number of pixels, anywhere in the
-- picture.
genBlob :: Int -> Gen Blob
genBlob most = do
    points <- chooseInt (6, most)
    u <- choose (0, fromIntegral frameWidth)
    v <- choose (0, fromIntegral frameHeight)
    pure (roundBlob (u, v) points)

-- | A round blob of so many pixels at a place.
roundBlob :: (Double, Double) -> Int -> Blob
roundBlob (u, v) points = Blob u v points (100 * points) spread spread 0
  where
    spread = fromIntegral points / (4 * pi)

-- | The smallest round blob, at the picture's corner; and round blobs of
-- fewer pixels, and at simpler places.
shrinkBlob :: Blob -> [Blob]
shrinkBlob =
    shrinkParts
        [ \b -> [roundBlob (blobX b, blobY b) points | points <- shrinkFrom 6 (blobPoints b)]
        , \b -> [roundBlob place (blobPoints b) | place <- shrinkPlace (blobX b, blobY b)]
        ]

-- | A blob that isn't the clip's, of up to 400 pixels.
newtype Stray = Stray Blob
    deriving (Eq, Show)

instance Arbitrary Stray where
    arbitrary = Stray <$> genBlob 400
    shrink (Stray b) = Stray <$> shrinkBlob b

-- | A few of 'Stray'.
newtype Strays = Strays [Blob]
    deriving (Eq, Show)

instance Arbitrary Strays where
    arbitrary = Strays <$> listOf (genBlob 400)
    shrink (Strays blobs) = Strays <$> shrinkList shrinkBlob blobs

-- | A blob for each reflector, of up to 500 pixels.
newtype ReflectorBlobs = ReflectorBlobs (Three Blob)
    deriving (Eq, Show)

instance Arbitrary ReflectorBlobs where
    arbitrary = ReflectorBlobs <$> genThree (genBlob 500)
    shrink (ReflectorBlobs three) = ReflectorBlobs <$> shrinkThree shrinkBlob three

-- | The sizes the reflectors have had, in pixels at 1 m: none seen yet, or
-- each seen at up to the given size, its usual size from twice its latest
-- down to a twentieth of it, as when it has faded out of sight.
genSizes :: Double -> Gen (Three Size)
genSizes most = oneof [pure (pure noSize), genThree size]
  where
    size = do
        latest <- choose (1, most)
        Size latest . (latest *) <$> choose (0.05, 2)

-- | None seen yet, and each with its usual size its latest.
shrinkSizes :: Three Size -> [Three Size]
shrinkSizes sizes =
    [pure noSize | any ((> 0) . sizeUsual) sizes]
        ++ [settled | let settled = (\s -> s{sizeUsual = sizeLatest s}) <$> sizes, or ((/=) <$> (sizeUsual <$> sizes) <*> (sizeUsual <$> settled))]

-- | The sizes the reflectors have had, each seen at up to 3000 pixels at
-- 1 m (see 'genSizes').
newtype Sizes = Sizes (Three Size)
    deriving (Eq, Show)

instance Arbitrary Sizes where
    arbitrary = Sizes <$> genSizes 3000
    shrink (Sizes sizes) = Sizes <$> shrinkSizes sizes

-- | How a reflector showed: a blob of its own, two taken as one, or its
-- share of a blob, of up to the given number of pixels, anywhere in the
-- picture.
genSight :: Double -> Gen Sight
genSight most = do
    u <- choose (0, fromIntegral frameWidth)
    v <- choose (0, fromIntegral frameHeight)
    Sight (u, v) <$> choose (6, most) <*> elements [Whole, Rejoined, Merged]

-- | The smallest blob of its own, at the picture's corner; and a blob of
-- its own, for one that isn't, of the fewest pixels and a whole number of
-- them, and at simpler places.
shrinkSight :: Sight -> [Sight]
shrinkSight =
    shrinkParts
        [ \s -> [s{sightKind = kind} | kind <- earlier [Whole, Rejoined, Merged] (sightKind s)]
        , \s -> [s{sightPixels = pixels} | pixels <- shrinkWithin (6, sightPixels s) 6 (sightPixels s)]
        , \s -> [s{sightAt = place} | place <- shrinkPlace (sightAt s)]
        ]

-- | How each reflector showed, of up to 500 pixels.
newtype ReflectorSights = ReflectorSights (Three Sight)
    deriving (Eq, Show)

instance Arbitrary ReflectorSights where
    arbitrary = ReflectorSights <$> genThree (genSight 500)
    shrink (ReflectorSights three) = ReflectorSights <$> shrinkThree shrinkSight three

-- | One of something for each reflector.
genThree :: Gen a -> Gen (Three a)
genThree g = Three <$> g <*> g <*> g

-- | Each of the three at its simplest, and one of them made simpler.
shrinkThree :: (a -> [a]) -> Three a -> [Three a]
shrinkThree f =
    shrinkParts
        [ \(Three a b c) -> [Three a' b c | a' <- f a]
        , \(Three a b c) -> [Three a b' c | b' <- f b]
        , \(Three a b c) -> [Three a b c' | c' <- f c]
        ]

-- * Settings

-- | A number as trackkat's text files give one: so many ten-thousandths,
-- between the given numbers of them, as 'parseNumber' reads them written
-- out, which isn't always the nearest number to them.
genWritten :: (Int, Int) -> Gen Double
genWritten range = written <$> chooseInt range

-- | Nothing, and the whole number a written number is nearest.
shrinkWritten :: Double -> [Double]
shrinkWritten = shrinkTo 0 (\x -> [fromIntegral (round x :: Int)])

written :: Int -> Double
written tenThousandths = fromMaybe (error "written") (parseNumber (renderNumber (fromIntegral tenThousandths / 10000)))

-- | A curve's points: inputs going up from above zero, and any outputs.
genPoints :: Gen [(Double, Double)]
genPoints = do
    steps <- scale (`div` 10) (listOf1 (choose (0.1, 50)))
    mapM (\input -> (,) input <$> choose (-200, 200)) (drop 1 (scanl (+) 0 steps))

-- | A straight line of one for one, and curves of fewer points.
shrinkPoints :: [(Double, Double)] -> [[(Double, Double)]]
shrinkPoints = shrinkTo [(1, 1)] (filter (not . null) . shrinkList (const []))

-- | A key or mouse button with modifiers, as trackkat's window gives one
-- from Windows' codes, or a joystick's button.
genKeyOrButton :: Gen KeyOrButton
genKeyOrButton = oneof [key, JoystickButton <$> (Joystick <$> joystickName <*> chooseInt (1, 4)) <*> chooseInt (1, maxButton)]
  where
    key = elements [b | mask <- [0 .. 15], code <- [0 .. 255], Just b <- [keyFromCodes mask code]]
    joystickName = unwords <$> listOf1 (elements ["Throttle", "Joystick", "-", "HOTAS", "Warthog", "T.16000M", "VPC", "MT-50CM2", "(HID)", "button", "Zoë's", "3D", "2", "hold"])

-- | The first key, alone; then, of a key, it with no modifiers or fewer,
-- and the first key with its modifiers; and, of a joystick's button, the
-- first button of the first joystick of the simplest name, a shorter
-- name, and the first joystick and button.
shrinkKeyOrButton :: KeyOrButton -> [KeyOrButton]
shrinkKeyOrButton = shrinkTo (Key [] "F1") steps
  where
    steps (Key mods k) = [Key [] k, Key mods "F1"] ++ [Key (filter (/= m) mods) k | m <- mods]
    steps (JoystickButton (Joystick name index) button) =
        [JoystickButton (Joystick "Throttle" 1) 1]
            ++ [JoystickButton (Joystick (unwords shorter) index) button | shorter <- shrinkList (const []) (words name), not (null shorter)]
            ++ [JoystickButton (Joystick name 1) button | index /= 1]
            ++ [JoystickButton (Joystick name index) 1 | button /= 1]

-- | A key or button as 'genKeyOrButton' has them, pressed any of the ways.
genBinding :: Gen Binding
genBinding = Binding <$> genKeyOrButton <*> elements [Tap, Hold, Double]

-- | A tap of the first key; and a tap of the same key or button, and a
-- simpler one pressed the same way.
shrinkBinding :: Binding -> [Binding]
shrinkBinding =
    shrinkParts
        [ \b -> [b{boundGesture = g} | g <- earlier [Tap, Hold, Double] (boundGesture b)]
        , \b -> [b{boundKey = i} | i <- shrinkKeyOrButton (boundKey b)]
        ]

-- | Settings a settings file can say: numbers as it writes them, each
-- smoothing from its two knobs, each curve's points going up from above
-- zero or a multiplier with a precision over the axis's own range, and a
-- clip that is a triangle. (See the tests' report for the settings it
-- can't say.)
genConfig :: Gen Config
genConfig = do
    recenter <- bindings
    pause <- bindings
    trim <- bindings
    aiming <- bindings
    marking <- bindings
    smooth <- smoothed
    aimed <- Aim <$> smoothed <*> curve yawRange <*> curve pitchRange <*> curve rollRange
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
    Config recenter pause trim aiming marking (curves leaning aimed) worn
        <$> elements [TrackKatNPClient, TrackIRNPClient]
        <*> elements [UpdatesOn, UpdatesOff]
        <*> elements [StartAtLogin, NoStartAtLogin]
        <*> elements [MaskLights, NoMasking]
        <*> elements [VirtualView, PhysicalView, CameraView]
  where
    bindings = scale (`div` 10) (listOf genBinding)
    smoothed = oneof [pure NoSmoothing, tuned <$> genWritten (0, 1000000) <*> genWritten (0, 1000000)]
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

-- | The default settings with nothing bound; and settings with fewer and
-- simpler bindings, and with one more of them as the default has it.
shrinkConfig :: Config -> [Config]
shrinkConfig =
    shrinkParts
        [ \c -> [c{recenterBindings = bs} | bs <- shrinkList shrinkBinding (recenterBindings c)]
        , \c -> [c{pauseBindings = bs} | bs <- shrinkList shrinkBinding (pauseBindings c)]
        , \c -> [c{trimBindings = bs} | bs <- shrinkList shrinkBinding (trimBindings c)]
        , \c -> [c{aimBindings = bs} | bs <- shrinkList shrinkBinding (aimBindings c)]
        , \c -> [c{markBindings = bs} | bs <- shrinkList shrinkBinding (markBindings c)]
        , asDefault (smoothing . response) (\c x -> c{response = (response c){smoothing = x}})
        , asDefault (yawCurve . response) (\c x -> c{response = (response c){yawCurve = x}})
        , asDefault (pitchCurve . response) (\c x -> c{response = (response c){pitchCurve = x}})
        , asDefault (rollCurve . response) (\c x -> c{response = (response c){rollCurve = x}})
        , asDefault (xCurve . response) (\c x -> c{response = (response c){xCurve = x}})
        , asDefault (yCurve . response) (\c x -> c{response = (response c){yCurve = x}})
        , asDefault (zCurve . response) (\c x -> c{response = (response c){zCurve = x}})
        , asDefault (lean . response) (\c x -> c{response = (response c){lean = x}})
        , asDefault (aim . response) (\c x -> c{response = (response c){aim = x}})
        , asDefault clip (\c x -> c{clip = x})
        , asDefault npclient (\c x -> c{npclient = x})
        , asDefault updates (\c x -> c{updates = x})
        , asDefault startup (\c x -> c{startup = x})
        , asDefault lightMasking (\c x -> c{lightMasking = x})
        , asDefault headView (\c x -> c{headView = x})
        ]
  where
    asDefault :: (Eq b) => (Config -> b) -> (Config -> b -> Config) -> Config -> [Config]
    asDefault part with c = [c `with` part defaultConfig | part c /= part defaultConfig]

-- | Settings a settings file can say (see 'genConfig').
newtype Sayable = Sayable Config
    deriving (Eq, Show)

instance Arbitrary Sayable where
    arbitrary = Sayable <$> genConfig
    shrink (Sayable c) = Sayable <$> shrinkConfig c
