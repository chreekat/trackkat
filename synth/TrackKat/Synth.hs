-- | A simulated camera watching a simulated head wear a clip: the frames
-- the tracker would be given, for tests and benchmarks.
--
-- See Note [Simulated frames].
module TrackKat.Synth (
    -- * The camera
    Camera (..),
    userCamera,

    -- * The clip and how it is worn
    Look (..),
    Wear (..),
    wornPitched,
    Clip (..),
    pivotInClip,

    -- * The head
    rotYXZ,
    headPose,

    -- * Images
    Disc,
    Lit,
    allLit,
    unlit,
    clipDiscs,
    pointDiscs,
    tapeDiscs,
    renderDiscs,
    squareStripes,
    breakAt,
    jitter,
    inPieces,
    fireDiscs,
    occluded,

    -- * Noise
    unitNoise,
    noise,
    gaussian,
) where

import Data.List (sortOn)

import TrackKat.Frame (Stripe (..), frameHeight, frameWidth)
import TrackKat.Linear
import TrackKat.Pose

-- Note [Simulated frames]
-- ~~~~~~~~~~~~~~~~~~~~~~~
-- A head turns about its neck pivot, and the clip is worn on it: turned
-- some way on the head ('Wear'), its origin, the middle of the lower
-- reflectors, some way from the pivot. A camera, tipped down, sees the
-- head; with the head turned no way, the pivot is at a fixed place in the
-- camera's level axes. Head axes are the camera's level axes with the head
-- squarely facing it: x to the camera's right (the wearer's left), y down,
-- z back.
--
-- Each reflector shows as a disc of lit pixels: for points, at where the
-- point projects, and for tape on curved tabs, at the centre of its light
-- worked out afresh here by summing strips of tape, each as bright as it
-- is square to the camera times how much light tape returns at that
-- slant, which falls off as a gaussian of 45 degrees. That is a different
-- falloff from the one the tracker assumes (see Note [Curved reflectors]
-- in "TrackKat.Pose"), so that tests don't only check the model against
-- itself. A disc's radius goes as one over the reflector's distance and
-- with how lit it is. Discs that touch make one blob, as reflectors that
-- run together do; other troubles (a reflector in pieces, a line of
-- pixels dropped, jitter of the centroid, a fire's flicker, the head
-- hiding what is behind it) are added to the discs.

-- | The camera: its lens, how far it is tipped down, in degrees, and where
-- the head's neck pivot is, with the head turned no way, in the camera's
-- level axes, mm.
data Camera = Camera
    { camIntrinsics :: Intrinsics
    , camDown :: Double
    , camPivot :: V3
    }

-- | The user's camera: a TrackIR 5 tipped 12 degrees down, the pivot about
-- 1 m away and 310 mm below its line of sight.
userCamera :: Camera
userCamera = Camera defaultIntrinsics 12 (V3 0 310 935)

-- | How the reflectors show: as points, or as tape on the model's curved
-- tabs.
data Look = AsPoints | AsTape
    deriving (Eq, Show)

-- | How the clip sits on the head: turned from the head's axes, and where
-- the neck pivot is from the clip's origin, in the head's axes, mm.
data Wear = Wear
    { wearTurn :: M33
    , wearNeck :: V3
    }

-- | The clip pitched by the given degrees on a head whose neck pivot is
-- 130 mm below and 90 behind it: how high the top reflector shows depends
-- on the pitch.
wornPitched :: Double -> Wear
wornPitched worn = Wear (rotYXZ 0 worn 0) (V3 0 130 90)

-- | A clip: its reflectors (the truth, which the tracker may be told
-- otherwise), how they show, and how it is worn.
data Clip = Clip
    { clipTruth :: Model
    , clipLook :: Look
    , clipWear :: Wear
    }

-- | The neck pivot in the axes of a clip worn so.
pivotInClip :: Wear -> V3
pivotInClip (Wear turn neck) = mulMV (transpose turn) neck

-- | The rotation turning by the given yaw, then pitch, then roll, in
-- degrees, as 'eulerYXZ' takes them apart.
rotYXZ :: Double -> Double -> Double -> M33
rotYXZ a b c = rodrigues (V3 0 (deg a) 0) `mulMM` rodrigues (V3 (deg b) 0 0) `mulMM` rodrigues (V3 0 0 (deg c))

deg :: Double -> Double
deg = (* (pi / 180))

-- | The pose of a clip worn so, the head turned by the given yaw, pitch and
-- roll in degrees about its neck pivot.
headPose :: Camera -> Wear -> Double -> Double -> Double -> Pose
headPose cam (Wear worn neck) y p r =
    Pose (tilt `mulMM` turned `mulMM` worn) (mulMV tilt (camPivot cam ^-^ mulMV turned neck))
  where
    tilt = rodrigues (V3 (deg (camDown cam)) 0 0)
    turned = rotYXZ y p r

-- | A disc of lit pixels: its centre in the image, and radius, in pixels.
type Disc = ((Double, Double), Double)

-- | How lit each reflector is (left, right, top), from 0 (unseen) to 1.
type Lit = (Double, Double, Double)

allLit, unlit :: Lit
allLit = (1, 1, 1)
unlit = (0, 0, 0)

-- | The radii, in pixels at 1 m, of the lower reflectors' discs and the top
-- one's: at 830 mm the lower ones light about 350 pixels and the top one
-- 265, as on the user's camera.
lowerRadius, topRadius :: Double
lowerRadius = 8.8
topRadius = 7.6

-- | The clip's reflectors' discs in the image at a pose, as they show.
clipDiscs :: Intrinsics -> Clip -> Lit -> Pose -> [Disc]
clipDiscs k c = case clipLook c of
    AsPoints -> pointDiscs k (clipTruth c)
    AsTape -> tapeDiscs k (clipTruth c)

-- | The reflectors' discs in the image for a pose, taking them for points,
-- each with its radius scaled by how lit it is (0 for unseen).
pointDiscs :: Intrinsics -> Model -> Lit -> Pose -> [Disc]
pointDiscs k m (litL, litR, litT) pose =
    [ (project k pose p, lit * radiusAt1m * 1000 / z)
    | (p, radiusAt1m, lit) <- [(mLeft m, lowerRadius, litL), (mRight m, lowerRadius, litR), (mTop m, topRadius, litT)]
    , let V3 _ _ z = mulMV (poseR pose) p ^+^ poseT pose
    , lit > 0
    ]

-- | The reflectors' discs in the image for a pose, taking them for tape on
-- the model's curved tabs (see Note [Simulated frames]), each with its
-- radius scaled by how lit it is (0 for unseen); one whose tape faces
-- wholly away isn't seen.
tapeDiscs :: Intrinsics -> Model -> Lit -> Pose -> [Disc]
tapeDiscs k m (litL, litR, litT) (Pose r t) =
    [ (project k (Pose r t) centre, lit * radiusAt1m * 1000 / z)
    | (middle, axis, radiusAt1m, lit) <- [(mLeft m, upright, lowerRadius, litL), (mRight m, upright, lowerRadius, litR), (mTop m, leaning, topRadius, litT)]
    , lit > 0
    , let facing = normalize (V3 0 0 (-1) ^+^ (dot (V3 0 0 1) axis *^ axis))
          across = cross axis facing
          strips =
            [ (w, at)
            | i <- [0 .. 99 :: Int]
            , let phi = (fromIntegral i + 0.5 - 50) / 100 * (arc / radius)
                  normal = (cos phi *^ facing) ^+^ (sin phi *^ across)
                  at = middle ^-^ (radius *^ facing) ^+^ (radius *^ normal)
                  inCamera = mulMV r at ^+^ t
                  slant = dot (mulMV r normal) (normalize ((-1) *^ inCamera))
                  angle = acos (min 1 slant)
                  w = slant * exp (negate ((angle / (pi / 4)) ^ (2 :: Int)))
            , slant > 0
            ]
          centre = (1 / sum (map fst strips)) *^ foldr ((^+^) . uncurry (*^)) (V3 0 0 0) strips
          V3 _ _ z = mulMV r centre ^+^ t
    , not (null strips)
    ]
  where
    Tabs radius arc bend = mTabs m
    crossbar = normalize (mRight m ^-^ mLeft m)
    upright = normalize (cross (V3 0 0 1) crossbar)
    leaning = (cos (bend * pi / 180) *^ upright) ^+^ (sin (bend * pi / 180) *^ cross crossbar upright)

-- | Discs of lit pixels as the camera's stripes. Runs that touch on a line
-- are one stripe, so discs that touch make one blob.
renderDiscs :: [Disc] -> [Stripe]
renderDiscs discs = concatMap line [0 .. frameHeight - 1]
  where
    line y = map (stripe y) (joinRuns (sortOn fst (concatMap (run y) discs)))
    run y ((u, v), r) =
        let dy = fromIntegral y - v
            halfWidth = sqrt (r * r - dy * dy)
            a = max 0 (ceiling (u - halfWidth))
            b = min (frameWidth - 1) (floor (u + halfWidth))
         in [(a, b) | r * r >= dy * dy, a <= b]
    joinRuns ((a, b) : (c, d) : rest) | c <= b + 1 = joinRuns ((a, max b d) : rest)
    joinRuns (x : rest) = x : joinRuns rest
    joinRuns [] = []
    stripe y (a, b) = let n = b - a + 1 in Stripe y a n (100 * n * (n - 1) `div` 2) (100 * n)

-- | A square of 5 by 5 lit pixels around a point, as the camera's stripes.
squareStripes :: (Double, Double) -> [Stripe]
squareStripes (u, v) =
    [ Stripe line (round u - 2) 5 (10 * 50) (5 * 50)
    | line <- [round v - 2 .. round v + 2]
    ]

-- | The pixels of one image line near a point dropped, breaking a reflector
-- there in two.
breakAt :: (Double, Double) -> [Stripe] -> [Stripe]
breakAt (u, v) = filter (\s -> not (stLine s == round v && abs (fromIntegral (stStart s) - u) < 15))

-- | Discs, in the i-th frame, each moved by up to half the given pixels
-- across and down, differently each frame, as noise moves a blob's
-- centroid.
jitter :: Double -> Int -> [Disc] -> [Disc]
jitter amp i = zipWith move [0 ..]
  where
    move j ((u, v), r) = ((u + amp * noise i j, v + amp * noise i (j + 0.5)), r)

-- | The j-th reflector's disc in the i-th frame, with the head pitched by
-- the given degrees: the lower reflectors, looking down more than 35
-- degrees, break into two pieces in about half the frames, a piece apart
-- and the rest off centre; and every disc moves by up to a tenth of a
-- pixel.
inPieces :: Int -> Double -> Int -> Disc -> [Disc]
inPieces i pitched j ((u0, v0), r)
    | j < 2 && pitched > 35 && noise i (7 + fromIntegral j) > 0 =
        let a = 2 * pi * noise (i `div` 6) (9 + fromIntegral j)
            f = 0.75 + 0.3 * noise i (11 + fromIntegral j)
            whole = r * sqrt f
            piece = r * sqrt (max 0.05 (0.85 - f))
            gap = 0.35 * r + whole + piece + 2
         in [((u + 0.35 * r * cos a, v + 0.35 * r * sin a), whole), ((u - gap * cos a, v - gap * sin a), piece)]
    | otherwise = [((u, v), r)]
  where
    u = u0 + 0.2 * noise i (fromIntegral j)
    v = v0 + 0.2 * noise i (fromIntegral j + 0.5)

-- | A lit fire's discs in the i-th frame, around its core's place: the
-- core, lighting 600 to 870 pixels, its centroid jittering by a pixel or
-- so, and up to four flames of 6 to 100 pixels flickering within 20
-- pixels of it, each there in about half the frames.
fireDiscs :: (Double, Double) -> Int -> [Disc]
fireDiscs (u, v) i =
    ((u + 2 * flicker 0 - 1, v + 2 * flicker 1 - 1), sqrt ((600 + 270 * flicker 2) / pi))
        : [ ((u + d * cos a, v + d * sin a), sqrt ((6 + 94 * flicker (k + 0.4)) / pi))
          | k <- [3 .. 6]
          , flicker (k + 0.1) > 0.5
          , let a = negate pi * flicker (k + 0.2)
                d = 18 + 2 * flicker (k + 0.3)
          ]
  where
    flicker = unitNoise i

-- | The discs of a light not hidden by the head the given reflectors are
-- on: none within 70 pixels of one.
occluded :: [Disc] -> [Disc] -> [Disc]
occluded heads = filter (\(p, _) -> all (\(q, _) -> distance p q > 70) heads)
  where
    distance (a, b) (c, d) = sqrt ((a - c) ^ (2 :: Int) + (b - d) ^ (2 :: Int))

-- | Noise from 0 to 1, the same for the same frame and key.
unitNoise :: Int -> Double -> Double
unitNoise i k = let x = sin (fromIntegral i * 12.9898 + k * 78.233) * 43758.5453 in x - fromIntegral (floor x :: Int)

-- | Noise from -0.5 to 0.5, the same for the same frame and key.
noise :: Int -> Double -> Double
noise i k = let x = sin (fromIntegral i * 12.9898 + k * 78.233) * 43758.5453 in x - fromIntegral (floor x :: Int) - 0.5

-- | About normally distributed noise, with a standard deviation of 1.
gaussian :: Int -> Double -> Double
gaussian i k = 2 * sum [noise i (k + 0.13 * n) | n <- [1, 2, 3]]
