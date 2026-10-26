-- | Just enough 3D linear algebra for pose estimation.
module TrackKat.Linear
  ( V3 (..)
  , (^+^)
  , (^-^)
  , (*^)
  , dot
  , cross
  , norm
  , M33 (..)
  , identity
  , transpose
  , mulMV
  , mulMM
  , rodrigues
  , solveLinear
  ) where

import Data.List (foldl')

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

-- | Solve the square system @a x = b@ by Gaussian elimination with partial
-- pivoting. Nothing if (numerically) singular.
solveLinear :: [[Double]] -> [Double] -> Maybe [Double]
solveLinear a b = backSubst <$> eliminate (zipWith (\row v -> row ++ [v]) a b)
  where
    eliminate [] = Just []
    eliminate rows =
      let pivot = foldl' (\p r -> if abs (head r) > abs (head p) then r else p) (head rows) rows
          others = deleteFirst pivot rows
          p0 = head pivot
          reduce r = let k = head r / p0 in zipWith (\x y -> x - k * y) (tail r) (tail pivot)
       in if abs p0 < 1e-12
            then Nothing
            else (pivot :) <$> eliminate (map reduce others)
    deleteFirst x (y : ys) | x == y = ys | otherwise = y : deleteFirst x ys
    deleteFirst _ [] = []
    -- Each eliminated row is [pivot, coefficients of later unknowns..., rhs].
    backSubst = foldr step []
      where
        step row xs =
          let coeffs = init (tail row)
              rhs = last row
           in (rhs - sum (zipWith (*) coeffs xs)) / head row : xs
