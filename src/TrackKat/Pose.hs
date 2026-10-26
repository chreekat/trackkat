-- | Head pose from the three reflectors of a TrackClip.
--
-- Frames: the camera frame has x right, y down (as in the image) and z out
-- of the lens. The model frame is the clip's own, chosen so that a head
-- squarely facing the camera has the identity rotation. A pose maps model to
-- camera: @p_cam = R p_model + t@.
module TrackKat.Pose
  ( Intrinsics (..)
  , defaultIntrinsics
  , Model (..)
  , trackClip
  , triangle
  , Pose (..)
  , project
  , identify
  , solvePose
  , HeadPose (..)
  , relativePose
  , eulerYXZ
  ) where

import Data.List (sortOn)

import TrackKat.Linear

-- | Pinhole camera.
data Intrinsics = Intrinsics
  { focal :: Double  -- ^ pixels
  , cx :: Double
  , cy :: Double
  }
  deriving (Show)

-- | TrackIR 5: 640x480, 51.7 degree horizontal field of view.
defaultIntrinsics :: Intrinsics
defaultIntrinsics = Intrinsics {focal = 320 / tan (51.7 / 2 * pi / 180), cx = 320, cy = 240}

data Model = Model
  { mLeft :: V3    -- ^ lower reflector that appears on the left of the image
  , mRight :: V3   -- ^ lower reflector that appears on the right
  , mTop :: V3     -- ^ the raised reflector
  , mPivot :: V3   -- ^ point the head turns about, for reporting translation
  }
  deriving (Show)

-- | NaturalPoint's reflective TrackClip on a cap brim, in mm. Dimensions are
-- linuxtrack's defaults (Cap-X 67, Cap-Y 54, Cap-Z 96, Head-Y 160, Head-Z 50).
trackClip :: Model
trackClip =
  Model
    { mLeft = V3 (-67) 0 0
    , mRight = V3 67 0 0
    , mTop = V3 0 (-54) 96
    , mPivot = V3 0 160 50
    }

-- | A headset clip from the three distances between reflectors, in mm: left
-- to right, left to top, right to top. The top reflector is on a post that
-- leans back (away from the camera) by @lean@ degrees from vertical.
--
-- Only the triangle's shape affects the angles, and its size only scales
-- the distance estimate. The lean doesn't need to be accurate: it picks which
-- of the two mirror-image solutions the solver starts near, and recentering
-- absorbs the rest. The pivot is put 170 mm below the headband.
triangle :: Double -> Double -> Double -> Double -> Maybe Model
triangle lean lr lt rt
  | lr <= 0 || h2 <= 0 = Nothing
  | otherwise =
      Just
        Model
          { mLeft = V3 (-a) 0 0
          , mRight = V3 a 0 0
          , mTop = V3 x (negate (h * cos theta)) (h * sin theta)
          , mPivot = V3 0 170 0
          }
  where
    a = lr / 2
    x = (lt * lt - rt * rt) / (2 * lr)
    h2 = lt * lt - (x + a) * (x + a)
    h = sqrt h2
    theta = lean * pi / 180

data Pose = Pose {poseR :: M33, poseT :: V3}
  deriving (Show)

project :: Intrinsics -> Pose -> V3 -> (Double, Double)
project k (Pose r t) p =
  let V3 x y z = mulMV r p ^+^ t
   in (cx k + focal k * x / z, cy k + focal k * y / z)

-- | Match three blob centroids to (left, right, top): the highest in the
-- image is the top reflector, the other two go by x.
identify :: [(Double, Double)] -> Maybe ((Double, Double), (Double, Double), (Double, Double))
identify pts = case sortOn snd pts of
  [top, a, b]
    | fst a <= fst b -> Just (a, b, top)
    | otherwise -> Just (b, a, top)
  _ -> Nothing

-- | Find the pose that projects the model onto the observed points
-- (left, right, top), refining from the given guess, or from a guess that
-- the head squarely faces the camera if there is none.
--
-- Returns the pose and the remaining RMS reprojection error in pixels.
solvePose
  :: Intrinsics
  -> Model
  -> Maybe Pose
  -> ((Double, Double), (Double, Double), (Double, Double))
  -> Maybe (Pose, Double)
solvePose k m guess obs@(l, r, _) =
  case guess of
    Just g | Just res@(_, err) <- refine g, err < 1 -> Just res
    _ -> refine (frontal k m l r)
  where
    pts = [mLeft m, mRight m, mTop m]
    observed = let (a, b, c) = obs in [a, b, c]
    residuals pose =
      concat (zipWith (\p (u, v) -> let (pu, pv) = project k pose p in [pu - u, pv - v]) pts observed)
    cost = sum . map (^ (2 :: Int)) . residuals
    rms pose = sqrt (cost pose / 3)
    refine p0 = do
      p <- levenbergMarquardt residuals p0
      let V3 _ _ z = poseT p
      if z > 0 then Just (p, rms p) else Nothing

-- | A head squarely facing the camera, at the distance that makes the two
-- lower reflectors as far apart as observed.
frontal :: Intrinsics -> Model -> (Double, Double) -> (Double, Double) -> Pose
frontal k m (lu, lv) (ru, rv) = Pose identity (V3 ((mu - cx k) * z / focal k) ((mv - cy k) * z / focal k) z ^-^ mid)
  where
    width = norm (mRight m ^-^ mLeft m)
    pixels = max 1 (sqrt ((ru - lu) ^ (2 :: Int) + (rv - lv) ^ (2 :: Int)))
    z = focal k * width / pixels
    mu = (lu + ru) / 2
    mv = (lv + rv) / 2
    mid = 0.5 *^ (mLeft m ^+^ mRight m)

-- | Damped Gauss-Newton on the six pose parameters, with a numerical
-- Jacobian. Rotation updates are applied as small axis-angle rotations.
levenbergMarquardt :: (Pose -> [Double]) -> Pose -> Maybe Pose
levenbergMarquardt residuals = go (50 :: Int) 1e-3
  where
    cost = sum . map (^ (2 :: Int)) . residuals
    step (Pose r t) [a, b, c, x, y, z] = Pose (mulMM (rodrigues (V3 a b c)) r) (t ^+^ V3 x y z)
    step p _ = p
    deltas = [(i, if i < 3 then 1e-6 else 1e-3) | i <- [0 .. 5 :: Int]]
    unit i h = [if j == i then h else 0 | j <- [0 .. 5]]
    go 0 _ p = Just p
    go n lambda p
      | c0 < 1e-12 = Just p
      | otherwise =
          let r0 = residuals p
              cols = [map (/ h) (zipWith (-) (residuals (step p (unit i h))) r0) | (i, h) <- deltas]
              jtj = [[sum (zipWith (*) ci cj) | cj <- cols] | ci <- cols]
              jtr = [sum (zipWith (*) ci r0) | ci <- cols]
              damped = [[if i == j then v * (1 + lambda) + 1e-9 else v | (j, v) <- zip [0 :: Int ..] row] | (i, row) <- zip [0 ..] jtj]
           in case solveLinear damped (map negate jtr) of
                Nothing -> Nothing
                Just d ->
                  let p' = step p d
                      c1 = cost p'
                   in if c1 < c0
                        then if c0 - c1 < 1e-10 * c0 then Just p' else go (n - 1) (lambda / 10) p'
                        else if lambda > 1e8 then Just p else go (n - 1) (lambda * 10) p
      where
        c0 = cost p

-- | Head pose relative to a reference pose, about the camera's axes. Angles
-- in degrees: yaw about the vertical, pitch about left-right, roll about
-- the line of sight. Translation in mm, of the model's pivot point.
--
-- Using the camera's axes (rather than the clip's) means the result doesn't
-- depend on how the clip is oriented on the head, only on the shape of its
-- triangle, provided the user faces the camera when recentering.
data HeadPose = HeadPose
  { yaw, pitch, roll :: Double
  , tx, ty, tz :: Double
  }
  deriving (Show)

relativePose :: Model -> Pose -> Pose -> HeadPose
relativePose m (Pose r0 t0) (Pose r t) =
  HeadPose (deg a) (deg b) (deg c) x y z
  where
    (a, b, c) = eulerYXZ (mulMM r (transpose r0))
    pivot rot trans = mulMV rot (mPivot m) ^+^ trans
    V3 x y z = pivot r t ^-^ pivot r0 t0
    deg = (* (180 / pi))

-- | Decompose @R = Ry(yaw) Rx(pitch) Rz(roll)@, in radians.
eulerYXZ :: M33 -> (Double, Double, Double)
eulerYXZ (M33 (V3 _ _ r02) (V3 r10 r11 r12) (V3 _ _ r22)) =
  ( atan2 r02 r22
  , asin (max (-1) (min 1 (negate r12)))
  , atan2 r10 r11
  )
