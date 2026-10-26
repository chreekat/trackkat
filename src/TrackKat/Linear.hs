-- | Just enough 3D linear algebra for pose estimation.
module TrackKat.Linear (
    V3 (..),
    (^+^),
    (^-^),
    (*^),
    dot,
    cross,
    norm,
    normalize,
    angleBetween,
    M33 (..),
    identity,
    transpose,
    mulMV,
    mulMM,
    rodrigues,
    rotationVector,
    rotateToward,
    solveLinear,
) where

import Data.List (unsnoc)

data V3 = V3 !Double !Double !Double
    deriving (Eq, Show)

infixl 6 ^+^, ^-^
infixl 7 *^

(^+^), (^-^) :: V3 -> V3 -> V3
V3 a b c ^+^ V3 x y z = V3 (a + x) (b + y) (c + z)
V3 a b c ^-^ V3 x y z = V3 (a - x) (b - y) (c - z)

(*^) :: Double -> V3 -> V3
k *^ V3 x y z = V3 (k * x) (k * y) (k * z)

dot :: V3 -> V3 -> Double
dot (V3 a b c) (V3 x y z) = a * x + b * y + c * z

cross :: V3 -> V3 -> V3
cross (V3 a b c) (V3 x y z) = V3 (b * z - c * y) (c * x - a * z) (a * y - b * x)

norm :: V3 -> Double
norm v = sqrt (dot v v)

-- | The unit vector in the same direction; the zero vector stays zero.
normalize :: V3 -> V3
normalize v = let n = norm v in if n == 0 then v else (1 / n) *^ v

-- | Radians between two non-zero vectors.
angleBetween :: V3 -> V3 -> Double
angleBetween a b = atan2 (norm (cross a b)) (dot a b)

-- | Rows.
data M33 = M33 !V3 !V3 !V3
    deriving (Eq, Show)

identity :: M33
identity = M33 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)

transpose :: M33 -> M33
transpose (M33 (V3 a b c) (V3 d e f) (V3 g h i)) = M33 (V3 a d g) (V3 b e h) (V3 c f i)

mulMV :: M33 -> V3 -> V3
mulMV (M33 r0 r1 r2) v = V3 (dot r0 v) (dot r1 v) (dot r2 v)

mulMM :: M33 -> M33 -> M33
mulMM a b = M33 (row r0) (row r1) (row r2)
  where
    M33 r0 r1 r2 = a
    M33 c0 c1 c2 = transpose b
    row r = V3 (dot r c0) (dot r c1) (dot r c2)

-- | Rotation by the axis-angle vector @w@ (angle = |w| radians).
rodrigues :: V3 -> M33
rodrigues w
    | theta < 1e-12 = identity
    | otherwise =
        M33
            (V3 (c + x * x * k) (x * y * k - z * s) (x * z * k + y * s))
            (V3 (y * x * k + z * s) (c + y * y * k) (y * z * k - x * s))
            (V3 (z * x * k - y * s) (z * y * k + x * s) (c + z * z * k))
  where
    theta = norm w
    V3 x y z = (1 / theta) *^ w
    c = cos theta
    s = sin theta
    k = 1 - c

-- | The axis-angle vector of a rotation, the inverse of 'rodrigues' for
-- rotations short of half a turn.
rotationVector :: M33 -> V3
rotationVector (M33 (V3 r00 r01 r02) (V3 r10 r11 r12) (V3 r20 r21 r22))
    | s < 1e-12 = V3 0 0 0
    | otherwise = (theta / s) *^ v
  where
    -- 2 sin(angle) times the axis.
    v = V3 (r21 - r12) (r02 - r20) (r10 - r01)
    s = norm v
    theta = acos (max (-1) (min 1 ((r00 + r11 + r22 - 1) / 2)))

-- | Turn a unit vector toward another, by at most the given radians.
rotateToward :: Double -> V3 -> V3 -> V3
rotateToward limit from to
    | angle <= limit = to
    | otherwise = mulMV (rodrigues (limit *^ normalize (cross from to))) from
  where
    angle = angleBetween from to

-- | Solve the square system @a x = b@ by Gaussian elimination with partial
-- pivoting. Nothing if (numerically) singular.
solveLinear :: [[Double]] -> [Double] -> Maybe [Double]
solveLinear a b = backSubst <$> eliminate (zipWith (\row v -> row ++ [v]) a b)
  where
    -- Rows are their coefficients followed by their right-hand side.
    eliminate [] = Just []
    eliminate rows = case [(abs x, n) | (n, x : _) <- zip [0 :: Int ..] rows] of
        [] -> Nothing
        leads ->
            let (size, i) = maximum leads
             in case splitAt i rows of
                    (before, (p0 : prest) : after)
                        | size >= 1e-12 ->
                            ((p0 : prest) :) <$> eliminate (map (reduce p0 prest) (before ++ after))
                    _ -> Nothing
    reduce p0 prest (x : xs) = zipWith (\c pc -> c - x / p0 * pc) xs prest
    reduce _ _ [] = []
    -- Each eliminated row is [pivot, coefficients of later unknowns..., rhs].
    backSubst = foldr step []
    step (p : rest) xs
        | Just (coeffs, rhs) <- unsnoc rest = (rhs - sum (zipWith (*) coeffs xs)) / p : xs
    step _ xs = xs
