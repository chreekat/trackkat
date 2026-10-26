-- | Learning the setup from everyday head movement: which way is up as the
-- camera sees it, and the point the neck turns the head about.
--
-- Up is the axis of the head's left and right turns. Each brisk turn whose
-- axis is near the current estimate adds that axis, weighted by the angle
-- turned; old turns fade, so a camera that has been moved is learnt again.
--
-- The pivot is the point in the head that stays still while the head turns
-- and the body doesn't. Each half second of tracking with enough turning
-- gives a least-squares fit of such a point; if the fit is good (the body
-- was still) it joins the estimate, again with old windows fading.
--
-- What the tracker applies follows the estimates slowly enough that the
-- view doesn't visibly move when they change.
module TrackKat.Calibration (
    -- * What has been learnt
    Calibration,
    defaultCalibration,
    estimatedLevel,
    estimatedPivot,
    levelTurning,
    pivotTurns,
    describeLevel,
    describePivot,

    -- * Learning
    Learner,
    newLearner,
    learnerCalibration,
    appliedLevel,
    appliedPivot,
    Refinement (..),
    learn,
    lostSight,
    calibrateLevel,

    -- * Saving
    renderCalibration,
    parseCalibration,
) where

import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import Text.Printf (printf)

import TrackKat.Linear
import TrackKat.Number
import TrackKat.Pose

-- | Estimates of up and of the neck pivot, with the evidence for each.
data Calibration = Calibration
    { upSum :: !V3
    -- ^ turns' axes in camera axes, pointing up, each times its angle in
    -- degrees, older turns faded
    , upWeight :: !Double
    -- ^ degrees of turning in 'upSum'
    , pivotInfo :: !M33
    -- ^ information about the pivot: the normal matrix of the least-squares
    -- fits so far, older ones faded, plus the prior's
    , pivotMoment :: !V3
    -- ^ the right-hand side to 'pivotInfo''s normal equations
    , pivotWindows :: !Double
    -- ^ windows of turning in 'pivotInfo', faded alike
    }
    deriving (Show)

-- | Nothing learnt yet: the camera is taken to be level, and the pivot is
-- where it is on most heads.
defaultCalibration :: Calibration
defaultCalibration =
    Calibration
        { upSum = V3 0 0 0
        , upWeight = 0
        , pivotInfo = scaleM priorStrength identity
        , pivotMoment = priorStrength *^ priorPivot
        , pivotWindows = 0
        }

-- | The camera's own up, used until a turn is seen.
cameraUp :: V3
cameraUp = V3 0 (-1) 0

-- | Up, in camera axes.
estimatedUp :: Calibration -> V3
estimatedUp c
    | norm (upSum c) > 0 = normalize (upSum c)
    | otherwise = cameraUp

estimatedLevel :: Calibration -> Level
estimatedLevel = levelFromUp . estimatedUp

-- | The point the head turns about, in the clip's axes (x to the wearer's
-- left, y down, z back, from the middle of the two lower reflectors), mm.
estimatedPivot :: Calibration -> V3
estimatedPivot c = clampPivot (fromMaybe priorPivot (solve3 (pivotInfo c) (pivotMoment c)))

-- | Degrees of head turning up was learnt from, with older turns faded.
levelTurning :: Calibration -> Double
levelTurning = upWeight

-- | Windows of turning the pivot was learnt from, with older ones faded.
pivotTurns :: Calibration -> Double
pivotTurns = pivotWindows

-- | A line for the console about the learnt level.
describeLevel :: Calibration -> String
describeLevel c =
    let (down, right) = cameraTilt (estimatedLevel c)
     in printf
            "Level learnt: camera tipped %.1f degrees down and %.1f to its right, from %.0f degrees of head turning."
            down
            right
            (levelTurning c)

-- | A line for the console about the learnt pivot.
describePivot :: Calibration -> String
describePivot c =
    printf
        "Neck pivot learnt: %.0f mm %s the clip, %.0f %s it and %.0f to its %s, from %.0f head turns."
        (abs y)
        (if y >= 0 then "below" else "above" :: String)
        (abs z)
        (if z >= 0 then "behind" else "in front of" :: String)
        (abs x)
        (if x >= 0 then "left" else "right" :: String)
        (pivotTurns c)
  where
    V3 x y z = estimatedPivot c

-- * Up

-- | Turns must reach this many degrees within 'turnSeconds' to count.
turnDegrees, turnSeconds :: Double
turnDegrees = 10
turnSeconds = 0.6

-- | Turns whose axis is further than this many degrees from the current up
-- are nods, tilts or a mix, and don't count. A camera moved by more needs
-- an explicit calibration.
upCone :: Double
upCone = 30

-- | Degrees of turning remembered: about a hundred and fifty glances.
upMemory :: Double
upMemory = 3600

-- | Degrees of turning before the learnt up is used.
upConfident :: Double
upConfident = 180

-- | Add a turn, a rotation of the head in camera axes, unless it isn't
-- a left or right turn.
addTurn :: M33 -> Calibration -> Maybe Calibration
addTurn rot c
    | degrees < turnDegrees || angleBetween axis up > upCone * pi / 180 = Nothing
    | otherwise =
        let fade = if upWeight c + degrees > upMemory then (upMemory - degrees) / upWeight c else 1
         in Just
                c
                    { upSum = fade *^ upSum c ^+^ degrees *^ axis
                    , upWeight = fade * upWeight c + degrees
                    }
  where
    v = rotationVector rot
    degrees = norm v * 180 / pi
    up = estimatedUp c
    axis = normalize (if dot v up < 0 then (-1) *^ v else v)

-- | Take an explicitly calibrated level as half of what is remembered, so
-- that everyday turning goes on refining it.
withLevel :: Level -> Calibration -> Calibration
withLevel l c = c{upSum = (upMemory / 2) *^ levelUp l, upWeight = upMemory / 2}

-- * Pivot

-- | Where the pivot is on most heads, in the clip's axes, mm. A headset clip
-- sits on the side of the head at about the temple, its reflectors a few
-- cm forward of the ear. Turning left and right happens mostly between the
-- top two vertebrae, which lie on the midline about level with the ear
-- lobe; nodding uses the rest of the neck too, so its centre is lower
-- still. That puts the pivot about 100 mm below the clip and 100 behind,
-- and up to 100 to one side, depending on the side the clip is worn.
priorPivot :: V3
priorPivot = V3 0 100 100

-- | Where the pivot may be: a box well beyond the range of heads and clip
-- placements, so that a misleading fit can't put it anywhere absurd.
clampPivot :: V3 -> V3
clampPivot (V3 x y z) = V3 (clamp 150 x) (max 0 (min 200 y)) (max (-50) (min 200 z))
  where
    clamp m = max (-m) . min m

-- | How much the prior counts, against about 5 for each window of turning.
-- Mostly it pins the part of the pivot that turning left and right can't
-- show: how far up or down the axis of turning it lies.
priorStrength :: Double
priorStrength = 1

-- | Length of a window, in seconds.
windowSeconds :: Double
windowSeconds = 0.5

-- | Degrees a window must turn through to say anything.
windowSpan :: Double
windowSpan = 8

-- | RMS distance, mm, by which a window's best fixed point may move; more,
-- and the body moved too.
windowFit :: Double
windowFit = 4

-- | Information remembered, as the trace of the normal matrix: about two
-- hundred windows of turning.
pivotMemory :: Double
pivotMemory = 1000

-- | Sums over a window's poses, enough to fit the point that stays still.
data Window = Window
    { wStart :: !Double
    -- ^ time of the first pose
    , wFirst :: !M33
    -- ^ rotation of the first pose
    , wCount :: !Int
    , wRot :: !M33
    -- ^ sum of rotations
    , wRotT :: !V3
    -- ^ sum of transposed rotations times translations
    , wTrans :: !V3
    -- ^ sum of translations
    , wSquares :: !Double
    -- ^ sum of squared translations
    , wSpan :: !Double
    -- ^ furthest the head turned from the first pose, radians
    }

startWindow :: Double -> Pose -> Window
startWindow now (Pose r t) = Window now r 1 r (mulMV (transpose r) t) t (dot t t) 0

addPose :: Pose -> Window -> Window
addPose (Pose r t) w =
    w
        { wCount = wCount w + 1
        , wRot = addM (wRot w) r
        , wRotT = wRotT w ^+^ mulMV (transpose r) t
        , wTrans = wTrans w ^+^ t
        , wSquares = wSquares w + dot t t
        , wSpan = max (wSpan w) (norm (rotationVector (r `mulMM` transpose (wFirst w))))
        }

-- | Add a window if the head turned enough during it and a fixed point
-- explains its movement.
--
-- With rotations R_i and translations t_i, a point p in the clip's axes is
-- at R_i p + t_i. Fitting p and the still position c to R_i p + t_i = c by
-- least squares, and eliminating c, leaves normal equations A p = b, with
-- A = n I - S^T S / n, b = S^T T / n - sum R_i^T t_i, S and T the sums of
-- R_i and t_i. A fit of the window alone, held near the current estimate
-- along the axis it can't see, must explain it within 'windowFit' and lie
-- in the plausible box.
addWindow :: Window -> Calibration -> Maybe Calibration
addWindow w c
    | wSpan w < windowSpan * pi / 180 || fit > windowFit || clampPivot own /= own = Nothing
    | otherwise =
        let dataTrace = traceM (pivotInfo c) - 3 * priorStrength
            fade = if dataTrace + traceM a > pivotMemory then max 0 ((pivotMemory - traceM a) / dataTrace) else 1
            prior = scaleM priorStrength identity
         in Just
                c
                    { pivotInfo = prior `addM` scaleM fade (pivotInfo c `addM` scaleM (-1) prior) `addM` a
                    , pivotMoment = priorStrength *^ priorPivot ^+^ fade *^ (pivotMoment c ^-^ priorStrength *^ priorPivot) ^+^ b
                    , pivotWindows = fade * pivotWindows c + 1
                    }
  where
    n = fromIntegral (wCount w)
    s = wRot w
    a = scaleM n identity `addM` scaleM (-1 / n) (transpose s `mulMM` s)
    b = (1 / n) *^ mulMV (transpose s) (wTrans w) ^-^ wRotT w
    squares = wSquares w - dot (wTrans w) (wTrans w) / n
    current = estimatedPivot c
    own = case solve3 (a `addM` scaleM priorStrength identity) (b ^+^ priorStrength *^ current) of
        Just p -> p
        Nothing -> current
    fit = sqrt (max 0 (dot own (mulMV a own) - 2 * dot b own + squares) / n)

solve3 :: M33 -> V3 -> Maybe V3
solve3 (M33 (V3 a b d) (V3 e f g) (V3 h i j)) (V3 x y z) =
    case solveLinear [[a, b, d], [e, f, g], [h, i, j]] [x, y, z] of
        Just [p, q, r] -> Just (V3 p q r)
        _ -> Nothing

addM :: M33 -> M33 -> M33
addM (M33 a b c) (M33 d e f) = M33 (a ^+^ d) (b ^+^ e) (c ^+^ f)

scaleM :: Double -> M33 -> M33
scaleM k (M33 a b c) = M33 (k *^ a) (k *^ b) (k *^ c)

traceM :: M33 -> Double
traceM (M33 (V3 a _ _) (V3 _ b _) (V3 _ _ c)) = a + b + c

-- * Learning

-- | Learning in progress, and the calibration the tracker applies, which
-- follows the estimates slowly.
data Learner = Learner
    { lCalibration :: !Calibration
    , lTurn :: !(Maybe (Double, M33))
    -- ^ time and rotation at the start of the turn being watched
    , lWindow :: !(Maybe Window)
    , lLast :: !(Maybe Double)
    -- ^ time of the last pose seen
    , lUp :: !V3
    -- ^ up as applied
    , lPivot :: !V3
    -- ^ pivot as applied
    , lToldUp :: !V3
    -- ^ up as last reported
    , lToldPivot :: !V3
    -- ^ pivot as last reported
    }

-- | Start learning from a calibration, applying it at once.
newLearner :: Calibration -> Learner
newLearner c = Learner c Nothing Nothing Nothing up pivot up pivot
  where
    up = estimatedUp c
    pivot = estimatedPivot c

learnerCalibration :: Learner -> Calibration
learnerCalibration = lCalibration

appliedLevel :: Learner -> Level
appliedLevel = levelFromUp . lUp

-- | The pivot applied, in the clip's axes, mm.
appliedPivot :: Learner -> V3
appliedPivot = lPivot

-- | Something learning changed enough to mention.
data Refinement = LevelRefined | PivotRefined
    deriving (Eq, Show)

-- | How fast what is applied follows the estimates: slow enough that a
-- turned head's view doesn't visibly drift, in degrees and mm a second.
upRate, pivotRate :: Double
upRate = 0.25
pivotRate = 2

-- | Seconds between poses beyond which the applied values don't move
-- further, so that they never jump.
longestStep :: Double
longestStep = 0.1

-- | Changes worth mentioning, in degrees and mm.
upNews, pivotNews :: Double
upNews = 0.5
pivotNews = 5

-- | Windows before the learnt pivot is mentioned.
pivotConfident :: Double
pivotConfident = 10

-- | Learn from the pose seen at the given time in seconds, and move what is
-- applied toward the estimates.
learn :: Double -> Pose -> Learner -> (Learner, [Refinement])
learn now pose l =
    ( l
        { lCalibration = c2
        , lTurn = turn'
        , lWindow = window'
        , lLast = Just now
        , lUp = if upWeight c2 >= upConfident then rotateToward (upRate * pi / 180 * dt) (lUp l) upEst else lUp l
        , lPivot = towards (pivotRate * dt) (lPivot l) pivotEst
        , lToldUp = if levelNews then upEst else lToldUp l
        , lToldPivot = if pivotNewsNow then pivotEst else lToldPivot l
        }
    , [LevelRefined | levelNews] ++ [PivotRefined | pivotNewsNow]
    )
  where
    r = poseR pose
    dt = maybe 0 (\t -> max 0 (min longestStep (now - t))) (lLast l)
    -- A turn ends when it has turned far enough, or taken too long.
    (c1, turn') = case lTurn l of
        Just (t0, r0)
            | now - t0 > turnSeconds -> (lCalibration l, Just (now, r))
            | norm (rotationVector turned) >= turnDegrees * pi / 180 ->
                (fromMaybe (lCalibration l) (addTurn turned (lCalibration l)), Just (now, r))
            | otherwise -> (lCalibration l, lTurn l)
          where
            turned = r `mulMM` transpose r0
        Nothing -> (lCalibration l, Just (now, r))
    (c2, window') = case maybe (startWindow now pose) (addPose pose) (lWindow l) of
        w
            | now - wStart w >= windowSeconds -> (fromMaybe c1 (addWindow w c1), Nothing)
            | otherwise -> (c1, Just w)
    upEst = estimatedUp c2
    pivotEst = estimatedPivot c2
    levelNews = upWeight c2 >= upConfident && angleBetween upEst (lToldUp l) >= upNews * pi / 180
    pivotNewsNow = pivotWindows c2 >= pivotConfident && norm (pivotEst ^-^ lToldPivot l) >= pivotNews

-- | Move a point toward another, by at most the given distance.
towards :: Double -> V3 -> V3 -> V3
towards limit from to
    | d <= limit = to
    | otherwise = from ^+^ (limit / d) *^ (to ^-^ from)
  where
    d = norm (to ^-^ from)

-- | The clip went out of sight: turns and windows in progress end.
lostSight :: Learner -> Learner
lostSight l = l{lTurn = Nothing, lWindow = Nothing, lLast = Nothing}

-- | Take an explicitly calibrated level, applying it at once.
calibrateLevel :: Level -> Learner -> Learner
calibrateLevel lvl l =
    let c = withLevel lvl (lCalibration l)
        up = estimatedUp c
     in l{lCalibration = c, lUp = up, lToldUp = up}

-- * Saving

-- | The calibration as a text file of @name = value@ lines that explains
-- itself.
renderCalibration :: Calibration -> String
renderCalibration c =
    unlines
        [ "# What trackkat has learnt about how you sit and the camera sits, from"
        , "# how your head moves. trackkat keeps it up to date while it tracks;"
        , "# delete this file to start afresh."
        , "#"
        , "# up: which way is up, as a direction in the camera's axes (x to its"
        , "# right, y down, z out of the lens), and the degrees of head turning it"
        , "# was learnt from, with older turns counting less."
        , "up = " ++ vector (estimatedUp c)
        , "turning = " ++ renderNumber (upWeight c)
        , "#"
        , "# pivot: the point your neck turns your head about, in mm from the"
        , "# middle of the clip's two lower reflectors, along the clip's axes (x to"
        , "# your left, y down, z back); how many half-second windows of turning it"
        , "# was learnt from; and how sure of it trackkat is, as the information"
        , "# matrix of its least-squares fit: xx xy xz yy yz zz."
        , "pivot = " ++ vector pivot
        , "pivot windows = " ++ renderNumber (pivotWindows c)
        , "pivot information = " ++ unwords (map renderNumber [a, b, d, f, g, j])
        ]
  where
    vector (V3 x y z) = unwords (map renderNumber [x, y, z])
    pivot = estimatedPivot c
    M33 (V3 a b d) (V3 _ f g) (V3 _ _ j) = pivotInfo c

-- | Read a calibration file. Settings it lacks start from scratch; the
-- error names the offending line.
parseCalibration :: String -> Either String Calibration
parseCalibration str = do
    s <- foldl' line (Right emptyFile) (zip [1 :: Int ..] (lines str))
    let up = maybe cameraUp normalize (fUp s)
        weight = fromMaybe 0 (fTurning s)
        info = fromMaybe (pivotInfo defaultCalibration) (fInfo s)
        pivot = fromMaybe (estimatedPivot defaultCalibration) (fPivot s)
    pure
        Calibration
            { upSum = weight *^ up
            , upWeight = weight
            , pivotInfo = info
            , pivotMoment = mulMV info pivot
            , pivotWindows = fromMaybe 0 (fWindows s)
            }
  where
    line acc (n, l) =
        acc >>= \s -> case break (== '=') (takeWhile (/= '#') l) of
            (name, '=' : value) -> either (Left . (where_ n ++)) Right (setting s (trim name) (words value))
            (blank, _)
                | all isSpace blank -> Right s
                | otherwise -> Left (where_ n ++ "expected name = value")
    where_ n = "line " ++ show n ++ ": "
    setting s name value = case (name, traverse parseNumber value) of
        ("up", Just [x, y, z]) | norm (V3 x y z) > 0 -> Right s{fUp = Just (V3 x y z)}
        ("up", _) -> Left "up is three numbers, not all zero"
        ("turning", Just [w]) | w >= 0 -> Right s{fTurning = Just w}
        ("turning", _) -> Left "turning is a number of degrees, at least 0"
        ("pivot", Just [x, y, z]) -> Right s{fPivot = Just (V3 x y z)}
        ("pivot", _) -> Left "pivot is three numbers"
        ("pivot windows", Just [w]) | w >= 0 -> Right s{fWindows = Just w}
        ("pivot windows", _) -> Left "pivot windows is a number, at least 0"
        ("pivot information", Just [a, b, d, f, g, j])
            | a > 0, f > 0, j > 0 -> Right s{fInfo = Just (M33 (V3 a b d) (V3 b f g) (V3 d g j))}
        ("pivot information", _) -> Left "pivot information is six numbers, the first, fourth and sixth positive"
        _ -> Left ("unknown setting " ++ show name)
    trim = f . f where f = reverse . dropWhile isSpace

-- | A calibration file's settings as read so far.
data File = File
    { fUp :: Maybe V3
    , fTurning :: Maybe Double
    , fPivot :: Maybe V3
    , fWindows :: Maybe Double
    , fInfo :: Maybe M33
    }

emptyFile :: File
emptyFile = File Nothing Nothing Nothing Nothing Nothing
