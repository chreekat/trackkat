-- | Estimating the true shape of the clip's triangle from how the head
-- moves, to report, experimentally; nothing applies it.
--
-- See Note [Clip shape from movement].
module TrackKat.Shape (
    ShapeLearner,
    newShapeLearner,
    watchShape,
    shapeLostSight,
    shapeWindows,
    ShapeEstimate (..),
    shapeEstimate,
    sureEstimate,
    Confidence (..),
    confidence,
    Difference (..),
    difference,
    describeShape,
    describeShapeBriefly,
    describeShapeSoFar,
) where

import Text.Printf (printf)

import TrackKat.Calibration (neckCanBeAt)
import TrackKat.Config (wornDistances)
import TrackKat.Linear
import TrackKat.Pose
import TrackKat.Term (Bound (..), Finding (..), Measure (..), Name (..), Tally, Term, Verdict (..), about, accepts, atMost, gate, impossible, noTally, possible, rejections, tally)
import TrackKat.World (World (..), inMillimetres, times, world)

-- Note [Clip shape from movement]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Three reflectors seen in a frame are six numbers, and a pose is six
-- unknowns: any triangle fits any three points in the image, so no frame
-- says anything about the triangle's shape. Movement does. A head turns
-- about its neck, so over a second of turning the poses keep one point
-- still, a little sway aside (see 'turnFit'). Poses solved with the wrong
-- triangle are distorted, by how the clip is seen, so that they don't:
-- moving the top reflector by a millimetre turns and moves each pose a
-- little, differently at each view. So the shape is what makes turning
-- look most like turning about a fixed point.
--
-- Only the shape shows, not the size: the whole scene twice the size,
-- seen from twice as far, looks the same. (The tabs' curve doesn't scale,
-- but the slide of their light is far too slight to tell.) So the left to
-- right distance is held as configured, and the top reflector moved in the
-- triangle's plane, along the crossbar and square to it: two numbers,
-- which say both distances to the top. Nor do the reflectors' sizes help:
-- a blob's area goes as one over the distance squared, so the top
-- reflector's depth behind the others shows as some 15% in area; but a
-- few mm of shape is under 1%, while tape's brightness, and so the area
-- above the threshold, changes by tens of % with the angle it is seen at.
--
-- The estimate is linearised about the configured shape. Each frame's
-- pose is a solution of where the reflectors appear; how it would move
-- with the top reflector moved follows from the derivatives of where they
-- appear with the pose and with the shape. A second of poses, with that,
-- fits a still point p and its place c with the top moved by d:
-- R_i p + t_i + B_i d = c, B_i the movement of the point at p per mm of d.
-- Eliminating p and c leaves a quadratic in d, the evidence of the
-- window; the evidence adds up over windows, and the estimate is where
-- their sum is least. Windows that turned too little to locate a still
-- point, or didn't turn about one at all, or put it where no neck can be,
-- are left out, as when learning the pivot.
--
-- Noise in the image biases such a fit when the derivatives come from the
-- same noisy pose they are applied to, and when each pose is noisy beside
-- how far the window turned. So poses are taken as means of a few frames,
-- and each mean's derivatives are taken at the mean of its neighbours.
-- Windows are spread over a few groups in turn, each with its own
-- estimate; their scatter says how well the whole is known.
--
-- In simulation (the head glancing 30 degrees left and right and 15 up and
-- down, swaying, 0.2 px of noise, curved tabs), a minute of tracking puts
-- the distances to the top within about a mm, and five minutes within
-- half that. Being linearised, the estimate falls a little short of a far
-- different shape, by about a tenth of the difference.
--
-- Which windows are taken is decided by terms (see "TrackKat.Term"), and
-- the learner counts them by what took or rejected them ('shapeWindows').

-- | Learning the shape of a clip, configured as the given model.
data ShapeLearner = ShapeLearner
    { slModel :: !Model
    , slBlock :: !(Maybe Block)
    , slBefore :: !(Maybe Pose)
    -- ^ the mean pose of the block before the last
    , slLast :: !(Maybe (Double, Pose))
    -- ^ the last block's end and mean pose, which becomes a sample when
    -- the next block ends
    , slWindow :: !(Maybe Window)
    , slGroups :: ![Evidence]
    -- ^ 'groups' of them, all evaluated
    , slTaken :: !Int
    -- ^ windows taken, which picks the group for the next
    , slTold :: !(Maybe (Double, Double))
    -- ^ the distances to the top last reported
    , slWindows :: !Tally
    -- ^ windows of turning taken, and those not, by what rejected them
    }

-- | Consecutive poses, to be taken as their mean.
data Block = Block
    { bCount :: !Int
    , bFirst :: !Pose
    , bTurns :: !V3
    -- ^ sum of the turns from the first pose
    , bMoves :: !V3
    -- ^ sum of the translations
    }

-- | A mean pose, and how it would turn and move with the top reflector
-- moved by a mm along the crossbar and square to it: rotation vectors and
-- translations in the camera's axes.
data Sample = Sample !Pose !V3 !V3 !V3 !V3

-- | The samples of a window of turning so far, the latest first, and when
-- it started.
data Window = Window !Double ![Sample]

-- | What windows say about the top reflector's move d: the quadratic
-- d' H d - 2 g' d, as H's three entries (uu, uv, vv) and g's two; and how
-- many windows it is from, older ones faded.
data Evidence = Evidence !Double !Double !Double !Double !Double !Double

eWindows :: Evidence -> Double
eWindows (Evidence _ _ _ _ _ w) = w

noEvidence :: Evidence
noEvidence = Evidence 0 0 0 0 0 0

addEvidence :: Evidence -> Evidence -> Evidence
addEvidence (Evidence a b c d e f) (Evidence a' b' c' d' e' f') = Evidence (a + a') (b + b') (c + c') (d + d') (e + e') (f + f')

scaleEvidence :: Double -> Evidence -> Evidence
scaleEvidence k (Evidence a b c d e f) = Evidence (k * a) (k * b) (k * c) (k * d) (k * e) (k * f)

-- | Groups windows are spread over, to tell how well the estimate is known.
groups :: Int
groups = 8

-- | Frames in a block.
blockFrames :: Int
blockFrames = 8

-- | Seconds in a window of turning.
windowSeconds :: Double
windowSeconds = 1

-- | Degrees a window must turn through.
windowSpan :: Double
windowSpan = 12

-- | RMS distance, mm, by which a window's best still point may move, with
-- the configured shape; more, and the body moved too. Looser than for
-- learning the pivot, as the shape may be some way off: four times the
-- world's 'neckWander', twice what learning the pivot allows.
windowFit :: Double
windowFit = inMillimetres (times 4 (neckWander world))

-- | Windows remembered: about ten minutes of turning.
memory :: Double
memory = 600

-- | Nothing learnt yet about a clip configured as the given model.
newShapeLearner :: Model -> ShapeLearner
newShapeLearner m = ShapeLearner m Nothing Nothing Nothing Nothing (replicate groups noEvidence) 0 Nothing noTally

-- | How many windows of turning the shape has been estimated from since
-- the learner was made, and how many were left out, by the term that
-- rejected them, with what the last one left out was rejected for.
shapeWindows :: ShapeLearner -> Tally
shapeWindows = slWindows

-- | Learn from a pose seen clearly at the given time, surely the head's,
-- given the pivot it turns about, in the model's axes. The estimate is
-- returned too when it is newly sure, or has moved since it was.
watchShape :: Intrinsics -> Double -> V3 -> Pose -> ShapeLearner -> (ShapeLearner, Maybe ShapeEstimate)
watchShape k now pivot pose sl = case maybe (Block 1 pose (V3 0 0 0) (poseT pose)) (addToBlock pose) (slBlock sl) of
    b
        | bCount b < blockFrames -> (sl{slBlock = Just b}, Nothing)
        | otherwise ->
            let mean = blockMean b
                sample = case (slBefore sl, slLast sl) of
                    (Just before, Just (at, mid)) -> (,) at <$> sampleAt k (slModel sl) mid (meanPose [before, mean])
                    _ -> Nothing
                moved = sl{slBlock = Nothing, slBefore = snd <$> slLast sl, slLast = Just (now, mean)}
             in maybe (moved, Nothing) (\(at, s) -> addSample pivot at s moved) sample

addToBlock :: Pose -> Block -> Block
addToBlock (Pose r t) b =
    b
        { bCount = bCount b + 1
        , bTurns = bTurns b ^+^ rotationVector (r `mulMM` transpose (poseR (bFirst b)))
        , bMoves = bMoves b ^+^ t
        }

blockMean :: Block -> Pose
blockMean b = Pose (rodrigues (k *^ bTurns b) `mulMM` poseR (bFirst b)) (k *^ bMoves b)
  where
    k = 1 / fromIntegral (bCount b)

-- | The mean of some poses, whose turns from the first are small.
meanPose :: [Pose] -> Pose
meanPose [] = Pose identity (V3 0 0 0)
meanPose (p : ps) = blockMean (foldl' (flip addToBlock) (Block 1 p (V3 0 0 0) (poseT p)) ps)

-- | Take a sample, ending the window if it is long enough.
addSample :: V3 -> Double -> Sample -> ShapeLearner -> (ShapeLearner, Maybe ShapeEstimate)
addSample pivot at s sl = case slWindow sl of
    Nothing -> (sl{slWindow = Just (Window at [s])}, Nothing)
    Just (Window start samples)
        | at - start < windowSeconds -> (sl{slWindow = Just (Window start (s : samples))}, Nothing)
        | otherwise -> case windowEvidence (slModel sl) pivot (s : samples) of
            Left failed -> (sl{slWindow = Nothing, slWindows = tally (Rejected failed) (slWindows sl)}, Nothing)
            Right e -> news (taken sl{slWindow = Nothing, slWindows = tally (Accepted possible) (slWindows sl)} e)

-- | Add a window's evidence to its group, fading the older.
taken :: ShapeLearner -> Evidence -> ShapeLearner
taken sl e = sl{slGroups = foldr seq added added, slTaken = slTaken sl + 1}
  where
    total = sum (map eWindows (slGroups sl))
    fade = if total + 1 > memory then (memory - 1) / total else 1
    added = [scaleEvidence fade g `addEvidence` (if i == slTaken sl `mod` groups then e else noEvidence) | (i, g) <- zip [0 ..] (slGroups sl)]

-- | The estimate, if it is newly sure or has moved by 'newsMM' since it
-- was told, marked as told.
news :: ShapeLearner -> (ShapeLearner, Maybe ShapeEstimate)
news sl = case shapeEstimate sl of
    Just est
        | confidence est == Sure
        , maybe True (\(a, b) -> max (abs (a - lt)) (abs (b - rt)) >= newsMM) (slTold sl) ->
            (sl{slTold = Just (lt, rt)}, Just est)
      where
        (_, lt, rt) = wornDistances (estimatedModel est)
    _ -> (sl, Nothing)

-- | How far, in mm, the estimate moves before it is told again.
newsMM :: Double
newsMM = 1.5

-- | The clip went out of sight, or its pose is in doubt: blocks and
-- windows in progress end.
shapeLostSight :: ShapeLearner -> ShapeLearner
shapeLostSight sl = sl{slBlock = Nothing, slBefore = Nothing, slLast = Nothing, slWindow = Nothing}

-- | The directions, in the model's axes, the top reflector is moved in:
-- along the crossbar, and square to it in the triangle's plane, away from
-- the crossbar.
moveAxes :: Model -> (V3, V3)
moveAxes m = (along, normalize (up ^-^ (dot up along *^ along)))
  where
    along = normalize (mRight m ^-^ mLeft m)
    up = mTop m ^-^ (0.5 *^ (mLeft m ^+^ mRight m))

-- | The model with its top reflector moved by so many mm along each of
-- 'moveAxes'.
moveTop :: Model -> (Double, Double) -> Model
moveTop m (du, dv) = m{mTop = mTop m ^+^ (du *^ u) ^+^ (dv *^ v)}
  where
    (u, v) = moveAxes m

-- | A sample of a pose, its derivatives taken at the given pose nearby:
-- how the pose would turn and move, with the reflectors appearing where
-- they do, if the top reflector were moved.
sampleAt :: Intrinsics -> Model -> Pose -> Pose -> Maybe Sample
sampleAt k m pose near = do
    let appear model p = concat [let (u, v) = seenAt k model p which in [u, v] | which <- [LeftReflector, RightReflector, TopReflector]]
        base = appear m near
        derivative f h = map (/ h) (zipWith (-) (appear' f) base)
          where
            appear' (model, p) = appear model p
        steps = [(i, if i < 3 then 1e-6 else 1e-3) | i <- [0 .. 5 :: Int]]
        byPose = [derivative (m, perturbed near [if j == i then h else 0 | j <- [0 .. 5]]) h | (i, h) <- steps]
        jacobian = foldr (zipWith (:)) (replicate 6 []) byPose
        byShape d = derivative (moveTop m d, near) 1e-3
    du <- solveLinear jacobian (map negate (byShape (1e-3, 0)))
    dv <- solveLinear jacobian (map negate (byShape (0, 1e-3)))
    case (du, dv) of
        ([a, b, c, x, y, z], [a', b', c', x', y', z']) -> Just (Sample pose (V3 a b c) (V3 x y z) (V3 a' b' c') (V3 x' y' z'))
        _ -> Nothing

-- | A pose turned by a rotation vector and moved, as the solver steps.
perturbed :: Pose -> [Double] -> Pose
perturbed (Pose r t) [a, b, c, x, y, z] = Pose (rodrigues (V3 a b c) `mulMM` r) (t ^+^ V3 x y z)
perturbed p _ = p

-- | What a window's samples, the latest first, say about the top
-- reflector's move, given the pivot as learnt; or, if they turned too
-- little, or no still point explains them, or it is where no neck can be,
-- what the terms that say so found.
--
-- The unknowns are the still point p, its place c, and the move d; each
-- sample gives three equations R p - c + B d = -t (see Note [Clip shape
-- from movement]), with B linearised about the window's still point with
-- the configured shape, and p held very weakly toward the pivot, which
-- only settles where along the axis of turning it is.
windowEvidence :: Model -> V3 -> [Sample] -> Either [Finding] Evidence
windowEvidence m pivot samples
    | not (accepts turned span') = Left (rejections turned span')
    | otherwise = maybe (Left [Finding unsolved impossible Nothing]) id $ do
        still <- leastSquares (stillRows (const (replicate 3 [0, 0])))
        let p = vec (take 3 still)
        normal@(_, _, fit) <- reduced (stillRows (moveColumns p))
        let found = (sqrt (fit / n), p)
        pure (if accepts explained found then Right (toEvidence normal) else Left (rejections explained found))
  where
    turned :: Term Double
    turned =
        gate
            (Name "turn" "the head turned too little")
            (\turn -> not (turn < windowSpan * pi / 180))
            (\turn -> Measure (turn * 180 / pi) AtLeast windowSpan "degrees")
    unsolved = Name "no still point" "no point of the head stayed still"
    -- The window's still point, and the RMS distance, mm, it moved: within
    -- 'windowFit', and where a neck can be.
    explained :: Term (Double, V3)
    explained =
        atMost (Name "fit" "the body moved while the head turned") "mm" windowFit fst
            <> about snd (neckCanBeAt "window's pivot" m)
    n = fromIntegral (length samples)
    span' = case reverse samples of
        Sample first _ _ _ _ : _ -> maximum [norm (rotationVector (r `mulMM` transpose (poseR first))) | Sample (Pose r _) _ _ _ _ <- samples]
        [] -> 0
    hold = 1e-4
    components (V3 x y z) = [x, y, z]
    vec xs = case xs of
        [x, y, z] -> V3 x y z
        _ -> V3 0 0 0
    rowsOf (M33 a b c) = map components [a, b, c]
    -- Rows of the equations, the unknowns p, c, d, with the right-hand
    -- side; and the hold on p.
    stillRows columns =
        concat
            [ zipWith3 (\rotation unit (move, target) -> (rotation ++ unit ++ move, negate target)) (rowsOf r) [[-1, 0, 0], [0, -1, 0], [0, 0, -1]] (zip (columns s) (components t))
            | s@(Sample (Pose r t) _ _ _ _) <- samples
            ]
            ++ [ (map (sqrt hold *) unit ++ [0, 0, 0, 0, 0], sqrt hold * x)
               | (unit, x) <- zip [[1, 0, 0], [0, 1, 0], [0, 0, 1]] (components pivot)
               ]
    -- How the camera position of p moves per mm of each move: turning it
    -- about the camera's origin, and moving it.
    moveColumns p (Sample (Pose r _) wu tu wv tv) =
        let q = mulMV r p
         in zipWith (\a b -> [a, b]) (components (cross wu q ^+^ tu)) (components (cross wv q ^+^ tv))
    normalOf rows =
        let columns = transpose' (map fst rows)
            targets = map snd rows
         in ( [[sum (zipWith (*) a b) | b <- columns] | a <- columns]
            , [sum (zipWith (*) a targets) | a <- columns]
            , sum (map (^ (2 :: Int)) targets)
            )
    transpose' = foldr (zipWith (:)) (repeat [])
    -- p and c with no move.
    leastSquares rows = let (a, b, _) = normalOf rows in solveLinear (map (take 6) (take 6 a)) (take 6 b)
    -- The normal equations for d, p and c eliminated: H d = g, and the
    -- sum of squares with no move.
    reduced rows = do
        let (a, b, yy) = normalOf rows
            axx = map (take 6) (take 6 a)
            axd = map (drop 6) (take 6 a)
            add = map (drop 6) (drop 6 a)
            bx = take 6 b
            bd = drop 6 b
            column j = map (!! j) axd
        s0 <- solveLinear axx (column 0)
        s1 <- solveLinear axx (column 1)
        sx <- solveLinear axx bx
        let dot' xs ys = sum (zipWith (*) xs ys)
            h i j = add !! i !! j - dot' (column i) (if j == 0 then s0 else s1)
            g i = bd !! i - dot' (column i) sx
        pure ((h 0 0, h 0 1, h 1 1), (g 0, g 1), yy - dot' bx sx)
    toEvidence ((huu, huv, hvv), (gu, gv), _) = Evidence huu huv hvv gu gv 1

-- | The move the evidence says, if it says one.
solveEvidence :: Evidence -> Maybe (Double, Double)
solveEvidence (Evidence a b c u v _)
    | det <= 1e-9 * max 1 (a * c) = Nothing
    | otherwise = Just ((c * u - b * v) / det, (a * v - b * u) / det)
  where
    det = a * c - b * b

-- | The clip's shape as estimated so far.
data ShapeEstimate = ShapeEstimate
    { estimatedModel :: Model
    , configuredModel :: Model
    , estimateSpread :: Double
    -- ^ mm: the standard error of the distances to the top, the larger
    , estimateMinutes :: Double
    -- ^ minutes of turning it is from, older ones faded
    }
    deriving (Show)

-- | The estimate so far, if the evidence makes one.
shapeEstimate :: ShapeLearner -> Maybe ShapeEstimate
shapeEstimate sl = do
    let total = foldl' addEvidence noEvidence (slGroups sl)
        distancesOf e = (\(_, lt, rt) -> (lt, rt)) . wornDistances . moveTop m <$> solveEvidence e
        m = slModel sl
        spread = case traverse distancesOf (slGroups sl) of
            Just ds ->
                let se f = let xs = map f ds; mu = sum xs / g in sqrt (sum [(x - mu) ^ (2 :: Int) | x <- xs] / (g - 1) / g)
                 in max (se fst) (se snd)
            Nothing -> 1 / 0
        g = fromIntegral groups
    move <- solveEvidence total
    pure (ShapeEstimate (moveTop m move) m spread (eWindows total * windowSeconds / 60))

-- | The estimate so far, if it is sure.
sureEstimate :: ShapeLearner -> Maybe ShapeEstimate
sureEstimate sl = case shapeEstimate sl of
    Just est | confidence est == Sure -> Just est
    _ -> Nothing

-- | Whether an estimate is sure enough to tell.
data Confidence = Sure | Unsure
    deriving (Eq, Show)

-- | Sure when it is from a minute of turning or more and the distances to
-- the top are known within half a mm.
confidence :: ShapeEstimate -> Confidence
confidence est
    | estimateMinutes est >= 1 && estimateSpread est <= 0.5 = Sure
    | otherwise = Unsure

-- | Whether an estimate is meaningfully different from the configured
-- shape: by 2 mm in a distance to the top, and by more than it may be
-- out.
data Difference = AsConfigured | Different
    deriving (Eq, Show)

difference :: ShapeEstimate -> Difference
difference est
    | off >= max 2 (4 * estimateSpread est) = Different
    | otherwise = AsConfigured
  where
    (_, lt, rt) = wornDistances (estimatedModel est)
    (_, lt0, rt0) = wornDistances (configuredModel est)
    off = max (abs (lt - lt0)) (abs (rt - rt0))

-- | Distances as the clip setting takes them, to the mm.
renderDistances :: Model -> String
renderDistances m = let (lr, lt, rt) = wornDistances m in printf "%.0f %.0f %.0f" lr lt rt

-- | Minutes, roughly, as words.
renderMinutes :: Double -> String
renderMinutes m
    | round m == (1 :: Int) = "a minute"
    | otherwise = printf "%.0f minutes" m

-- | A line for the log on a sure estimate.
describeShape :: ShapeEstimate -> String
describeShape est = case difference est of
    Different ->
        printf
            "Clip shape estimate (experimental): %s mm, from %s of turning, keeping %.0f mm left to right (movement shows the shape, not the size); configured %s. Try clip = %s if tracking seems off."
            (renderDistances (estimatedModel est))
            (renderMinutes (estimateMinutes est))
            lr
            (renderDistances (configuredModel est))
            (renderDistances (estimatedModel est))
    AsConfigured ->
        printf
            "Clip shape estimate (experimental): %s mm, from %s of turning: as configured."
            (renderDistances (estimatedModel est))
            (renderMinutes (estimateMinutes est))
  where
    (lr, _, _) = wornDistances (configuredModel est)

-- | The estimate as the window's calibration area shows it, on one line.
describeShapeBriefly :: ShapeEstimate -> String
describeShapeBriefly est = case difference est of
    Different -> printf "Clip shape (experimental): %s mm, not the %s set." (renderDistances (estimatedModel est)) (renderDistances (configuredModel est))
    AsConfigured -> printf "Clip shape (experimental): %s mm, as set." (renderDistances (configuredModel est))

-- | How the estimate stands, sure or not, for a replay's end.
describeShapeSoFar :: ShapeLearner -> String
describeShapeSoFar sl = case shapeEstimate sl of
    Just est
        | not (isInfinite (estimateSpread est)) ->
            let (lr, lt, rt) = wornDistances (estimatedModel est)
             in printf
                    "Clip shape estimate (experimental): %.1f %.1f %.1f mm, give or take %.1f, from %.1f minutes of turning (%s; configured %s, its left to right kept)."
                    lr
                    lt
                    rt
                    (estimateSpread est)
                    (estimateMinutes est)
                    ( case (confidence est, difference est) of
                        (Unsure, _) -> "not sure yet" :: String
                        (Sure, Different) -> "different"
                        (Sure, AsConfigured) -> "as configured"
                    )
                    (renderDistances (configuredModel est))
    _ -> "Clip shape estimate (experimental): too little turning to tell."
