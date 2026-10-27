-- | Head pose from the three reflectors of a TrackClip.
--
-- Frames: the camera frame has x right, y down (as in the image) and z out
-- of the lens. The model frame is the clip's own, chosen so that a head
-- squarely facing the camera has the identity rotation. A pose maps model to
-- camera: @p_cam = R p_model + t@.
module TrackKat.Pose (
    Intrinsics (..),
    defaultIntrinsics,
    Model (..),
    trackClip,
    triangle,
    Pose (..),
    project,
    identify,
    solvePose,
    Observation (..),
    solveNear,
    poseChange,
    Level,
    cameraLevel,
    levelFromUp,
    levelUp,
    levelFromYaws,
    cameraTilt,
    inLevelAxes,
    HeadPose (..),
    relativePose,
    eulerYXZ,
) where

import Data.List (sortOn)

import TrackKat.Linear

-- | Pinhole camera.
data Intrinsics = Intrinsics
    { focal :: Double
    -- ^ pixels
    , cx :: Double
    , cy :: Double
    }
    deriving (Show)

-- | TrackIR 5: 640x480, 51.7 degree horizontal field of view.
defaultIntrinsics :: Intrinsics
defaultIntrinsics = Intrinsics{focal = 320 / tan (51.7 / 2 * pi / 180), cx = 320, cy = 240}

data Model = Model
    { mLeft :: V3
    -- ^ lower reflector that appears on the left of the image
    , mRight :: V3
    -- ^ lower reflector that appears on the right
    , mTop :: V3
    -- ^ the raised reflector
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
        }

-- | A headset clip from the three distances between reflectors, in mm: left
-- to right, left to top, right to top. The top reflector is on a post that
-- leans back (away from the camera) by @lean@ degrees from vertical.
--
-- Only the triangle's shape affects the angles, and its size only scales
-- the distance estimate. The lean doesn't need to be accurate: it picks which
-- of the two mirror-image solutions the solver starts near, and recentering
-- absorbs the rest.
triangle :: Double -> Double -> Double -> Double -> Maybe Model
triangle lean lr lt rt
    | lr <= 0 || h2 <= 0 = Nothing
    | otherwise =
        Just
            Model
                { mLeft = V3 (-a) 0 0
                , mRight = V3 a 0 0
                , mTop = V3 x (negate (h * cos theta)) (h * sin theta)
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

-- | Match three blobs, given their centroids, to (left, right, top): the
-- highest in the image is the top reflector, the other two go by x.
--
-- This is only a guess for when there is no recent pose to predict from:
-- with the camera above the head, the top reflector can appear level with
-- or below a lower one.
identify :: (a -> (Double, Double)) -> [a] -> Maybe (a, a, a)
identify at blobs = case sortOn (snd . at) blobs of
    [top, a, b]
        | fst (at a) <= fst (at b) -> Just (a, b, top)
        | otherwise -> Just (b, a, top)
    _ -> Nothing

-- | Find the pose that projects the model onto the observed points
-- (left, right, top), refining from the given guess, or from a guess that
-- the head squarely faces the camera if there is none.
--
-- Returns the pose and the remaining RMS reprojection error in pixels.
solvePose ::
    Intrinsics ->
    Model ->
    Maybe Pose ->
    ((Double, Double), (Double, Double), (Double, Double)) ->
    Maybe (Pose, Double)
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

-- | A reflector seen in the image: its place on the model, how much its
-- position counts (1 for a clearly seen reflector, less for a dim or
-- uncertain one), and where it was seen.
data Observation = Observation
    { obPoint :: V3
    , obWeight :: Double
    , obSeen :: (Double, Double)
    }
    deriving (Show)

-- | Refine a pose to fit what was seen, starting from a recent pose and held
-- gently toward it, so that what the observations leave free (with one
-- reflector unseen, turning about the line through the other two) stays
-- where it was. The hold is on rotation and on the movement of the given
-- pivot, a point in the model's axes that the head turns about, so that
-- turning the head is preferred to sliding it.
--
-- Returns the pose and the weighted RMS reprojection error in pixels.
solveNear :: Intrinsics -> V3 -> Pose -> [Observation] -> Maybe (Pose, Double)
solveNear k pivot recent@(Pose r0 _) obs
    | totalWeight <= 0 = Nothing
    | otherwise = do
        p <- levenbergMarquardt residuals recent
        let V3 _ _ z = poseT p
        if z > 0 then Just (p, fit p) else Nothing
  where
    totalWeight = sum [w * w | Observation _ w _ <- obs]
    reprojection pose =
        concat [let (pu, pv) = project k pose p in [w * (pu - u), w * (pv - v)] | Observation p w (u, v) <- obs]
    residuals pose = reprojection pose ++ hold pose
    hold pose@(Pose r _) =
        let V3 a b c = turnBetween r0 r
            V3 x y z = pivotAt pose ^-^ pivotAt recent
         in map (holdTurn *) [a, b, c] ++ map (holdMove *) [x, y, z]
    pivotAt (Pose r t) = mulMV r pivot ^+^ t
    fit pose = sqrt (sum (map (^ (2 :: Int)) (reprojection pose)) / totalWeight)

-- | How strongly 'solveNear' holds a pose where it was: pixels of
-- reprojection error worth one radian of turning, and one mm of movement.
-- About a twentieth of a pixel per degree or mm, far weaker than what a
-- reflector in view says about any direction it can see.
holdTurn, holdMove :: Double
holdTurn = 3
holdMove = 0.05

-- | The turn from one rotation to another, as sin(angle) times the axis:
-- smooth near no turn at all.
turnBetween :: M33 -> M33 -> V3
turnBetween from to =
    let M33 (V3 _ a01 a02) (V3 a10 _ a12) (V3 a20 a21 _) = to `mulMM` transpose from
     in 0.5 *^ V3 (a21 - a12) (a02 - a20) (a10 - a01)

-- | How far apart two poses are: degrees of turning, and mm the given pivot
-- (a point in the model's axes) moved.
poseChange :: V3 -> Pose -> Pose -> (Double, Double)
poseChange pivot (Pose r0 t0) (Pose r t) =
    ( norm (rotationVector (r `mulMM` transpose r0)) * 180 / pi
    , norm ((mulMV r pivot ^+^ t) ^-^ (mulMV r0 pivot ^+^ t0))
    )

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
                                then if c0 - c1 < 1e-10 * c0 || c0 - c1 < 1e-4 then Just p' else go (n - 1) (lambda / 10) p'
                                else if lambda > 1e8 then Just p else go (n - 1) (lambda * 10) p
      where
        c0 = cost p

-- | Rotation from the camera's axes to level axes facing the camera.
newtype Level = Level M33
    deriving (Show)

-- | Level axes for a camera that is itself level.
cameraLevel :: Level
cameraLevel = Level identity

-- | Level axes with the given vertical, in camera axes, pointing up; their
-- forward is as close to the camera's line of sight as level allows.
levelFromUp :: V3 -> Level
levelFromUp up = Level (M33 right down forward)
  where
    down = (-1) *^ normalize up
    ahead = V3 0 0 1 ^-^ (dot (V3 0 0 1) down *^ down)
    forward = normalize ahead
    right = cross down forward

-- | The vertical of level axes, in camera axes, pointing up.
levelUp :: Level -> V3
levelUp (Level (M33 _ down _)) = (-1) *^ down

-- | Level axes whose vertical is the axis of the given head rotations
-- (relative to a reference, in camera axes), as from turning the head left
-- and right. Small rotations count for little; the result is Nothing if
-- there was too little turning to tell.
levelFromYaws :: [M33] -> Maybe Level
levelFromYaws rots
    | norm total < 0.5 = Nothing
    | otherwise = Just (levelFromUp total)
  where
    -- 2 sin(angle) times the axis of each rotation, pointing up.
    axis (M33 (V3 _ r01 r02) (V3 r10 _ r12) (V3 r20 r21 _)) =
        let v = V3 (r21 - r12) (r02 - r20) (r10 - r01)
         in if dot v cameraUp < 0 then (-1) *^ v else v
    total = foldr ((^+^) . axis) (V3 0 0 0) rots
    cameraUp = V3 0 (-1) 0

-- | How far the camera is tipped down, and to its right, from the given
-- level, in degrees.
cameraTilt :: Level -> (Double, Double)
cameraTilt (Level (M33 _ (V3 dx dy dz) _)) =
    (atan2 dz (sqrt (dx * dx + dy * dy)) * 180 / pi, atan2 (negate dx) dy * 180 / pi)

-- | A direction given in the clip's axes, in level axes while the clip is in
-- the given pose.
inLevelAxes :: Level -> Pose -> V3 -> V3
inLevelAxes (Level toLevel) (Pose r _) v = mulMV toLevel (mulMV r v)

-- | A head's rotation and movement from a reference pose, in a level frame:
-- x to the camera's right, y down, z away from the camera.
data HeadPose = HeadPose
    { yaw :: Double
    -- ^ degrees about the vertical
    , pitch :: Double
    -- ^ degrees about the left-right axis
    , roll :: Double
    -- ^ degrees about the front-back axis
    , tx :: Double
    -- ^ mm
    , ty :: Double
    -- ^ mm
    , tz :: Double
    -- ^ mm
    }
    deriving (Show)

-- | The current pose relative to a reference pose, measured about the given
-- level axes, with translation taken at the given pivot: the point, in the
-- model's axes, that the head turns about, so that turning alone doesn't
-- count as movement.
--
-- Using axes fixed to the camera rather than to the clip means the result
-- depends only on the clip triangle's shape, not on how the clip sits on the
-- head, provided the user faces the camera when the reference is taken.
relativePose :: Level -> V3 -> Pose -> Pose -> HeadPose
relativePose (Level toLevel) pivotPoint (Pose r0 t0) (Pose r t) =
    HeadPose (deg a) (deg b) (deg c) x y z
  where
    toCamera = transpose toLevel
    (a, b, c) = eulerYXZ (toLevel `mulMM` mulMM r (transpose r0) `mulMM` toCamera)
    pivot rot trans = mulMV rot pivotPoint ^+^ trans
    V3 x y z = mulMV toLevel (pivot r t ^-^ pivot r0 t0)
    deg = (* (180 / pi))

-- | Decompose @R = Ry(yaw) Rx(pitch) Rz(roll)@, in radians.
eulerYXZ :: M33 -> (Double, Double, Double)
eulerYXZ (M33 (V3 _ _ r02) (V3 r10 r11 r12) (V3 _ _ r22)) =
    ( atan2 r02 r22
    , asin (max (-1) (min 1 (negate r12)))
    , atan2 r10 r11
    )
