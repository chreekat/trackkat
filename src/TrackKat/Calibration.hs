-- | Learning the setup from everyday head movement: which way is up as the
-- camera sees it, and the point the neck turns the head about.
--
-- Up is the axis of the head's left and right turns: of the main axes of
-- all its brisk turns, nods and tilts alike, the one nearest the camera's
-- own up, with old turns fading, so a camera that has been moved is learnt
-- again (see Note [Learning up]).
--
-- The pivot is the point in the head that stays still while the head turns
-- and the body doesn't. Each half second of tracking with enough turning
-- gives a least-squares fit of such a point; if the fit is good (the body
-- was still) and the point is where a neck can be, behind the reflectors,
-- it joins the estimate, again with old windows fading. Learning waits
-- while the pose may be the mirror image of the head's (see
-- "TrackKat.Branch").
--
-- What the tracker applies follows the estimates slowly enough that the
-- view doesn't visibly move when they change.
module TrackKat.Calibration (
    -- * What has been learnt
    Calibration,
    defaultCalibration,
    mountedCalibration,
    estimatedLevel,
    estimatedPivot,
    pivotSpreadAlong,
    levelTurning,
    pivotTurns,
    describeLevel,
    describePivot,
    describeLevelBriefly,
    describePivotBriefly,
    learntClip,
    plausiblePivot,
    plausibleCalibration,
    screenCalibration,

    -- * Learning
    Learner,
    newLearner,
    learnerCalibration,
    appliedLevel,
    appliedPivot,
    Refinement (..),
    learn,
    lostSight,
    changeClip,
    calibrateLevel,

    -- * Windows of turning
    TurnWindow,
    windowSeconds,
    startWindow,
    addPose,
    windowStart,
    windowTurn,
    turnFit,

    -- * Saving
    renderCalibration,
    parseCalibration,
) where

import Data.Char (isSpace)
import Data.List (maximumBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Text.Printf (printf)

import TrackKat.Linear
import TrackKat.Number
import TrackKat.Pose
import TrackKat.World (World (..), inMillimetres, inPerSecond, times, world)

-- | Estimates of up and of the neck pivot, with the evidence for each.
data Calibration = Calibration
    { upScatter :: !M33
    -- ^ the sum of turns' axes, in camera axes, each times itself and the
    -- degrees it counts for, older turns faded (see Note [Learning up])
    , upWeight :: !Double
    -- ^ degrees of turning in 'upScatter'
    , pivotInfo :: !M33
    -- ^ information about the pivot from windows of turning, per mm
    -- squared, faded as in Note [Learning the pivot]; the prior's isn't in it
    , pivotMoment :: !V3
    -- ^ the right-hand side to 'pivotInfo''s normal equations
    , pivotWindows :: !Double
    -- ^ windows of turning learnt from, older ones faded
    , pivotPrior :: !V3
    -- ^ where the pivot is on most heads, for the clip as it is mounted
    -- (see 'mountPivot'), in the head's axes
    }
    deriving (Show)

-- | Nothing learnt yet: the camera is taken to be level, and the pivot is
-- where it is on most heads, for a clip mounted the standard way (see
-- 'standardMounting').
defaultCalibration :: Calibration
defaultCalibration =
    Calibration
        { upScatter = zeroM
        , upWeight = 0
        , pivotInfo = zeroM
        , pivotMoment = V3 0 0 0
        , pivotWindows = 0
        , pivotPrior = prior
        }
  where
    prior = mountPivot standardMounting

-- | A calibration for a clip mounted as given: what was learnt kept, the
-- pivot held near where it is on most heads for that mounting along what
-- turning hasn't shown (see Note [Learning the pivot]).
mountedCalibration :: Mounting -> Calibration -> Calibration
mountedCalibration mount c = c{pivotPrior = mountPivot mount}

-- | The camera's own up, used until a turn is seen.
cameraUp :: V3
cameraUp = V3 0 (-1) 0

-- | Up, in camera axes: of the main axes of turning, the one nearest the
-- camera's own up, pointing up (see Note [Learning up]).
estimatedUp :: Calibration -> V3
estimatedUp c
    | upWeight c > 0 = if dot nearest cameraUp < 0 then (-1) *^ nearest else nearest
    | otherwise = cameraUp
  where
    nearest = snd (maximumBy (comparing (abs . dot cameraUp . snd)) (eigenSymmetric (upScatter c)))

estimatedLevel :: Calibration -> Level
estimatedLevel = levelFromUp . estimatedUp

-- | The point the head turns about, in the head's axes (x to the wearer's
-- left, y down, z back, from the middle of the clip's two lower
-- reflectors; see Note [Mounting] in "TrackKat.Pose"), mm.
estimatedPivot :: Calibration -> V3
estimatedPivot c = fromMaybe (pivotPrior c) (solve3 (pivotPrecision c) (priorInfo `mulMV` pivotPrior c ^+^ pivotMoment c))

-- | All that is known of the pivot: the prior's information and the
-- windows', per mm squared.
pivotPrecision :: Calibration -> M33
pivotPrecision c = priorInfo `addM` pivotInfo c

-- | The pivot's standard error, mm, along a direction in the head's axes.
pivotSpreadAlong :: Calibration -> V3 -> Double
pivotSpreadAlong c u = maybe priorSpread (sqrt . max 0 . dot u) (solve3 (pivotPrecision c) u)

-- | Degrees of head turning up was learnt from, with older turns faded.
levelTurning :: Calibration -> Double
levelTurning = upWeight

-- | Windows of turning the pivot was learnt from, with older ones faded.
pivotTurns :: Calibration -> Double
pivotTurns = pivotWindows

-- | What to do to have something learnt, ending a line saying that it
-- hasn't been yet.
notLearnt :: String
notLearnt = "not learnt yet; turn your head left and right."

-- | A line for the console about the learnt level, or that none is yet.
describeLevel :: Calibration -> String
describeLevel c
    | levelTurning c <= 0 = "Level: " ++ notLearnt
    | otherwise =
        let (down, right) = cameraTilt (estimatedLevel c)
         in printf
                "Level learnt: camera tipped %.1f degrees down and %.1f to its right, from %.0f degrees of head turning."
                down
                right
                (levelTurning c)

-- | A line for the console about the learnt pivot, or that none is yet,
-- given the clip's pose at the centre. Directions are along the level axes
-- with the head at the centre, facing the camera, however the clip sits on
-- the head; without a centre, only the distance is given. Each comes with
-- its standard error, which stays large for a direction little shown yet
-- (see Note [Learning the pivot]).
describePivot :: Maybe Pose -> Calibration -> String
describePivot _ c | pivotTurns c <= 0 = "Neck pivot: " ++ notLearnt
describePivot Nothing c =
    printf
        "Neck pivot learnt: %.0f mm from the clip, give or take %.0f, from %.0f head turns."
        (norm (estimatedPivot c))
        (pivotSpreadAlong c (normalize (estimatedPivot c)))
        (pivotTurns c)
describePivot (Just centre) c =
    printf
        "Neck pivot learnt: %.0f mm %s the clip, %.0f %s it and %.0f to your %s, give or take %.0f, %.0f and %.0f mm, from %.0f head turns."
        (abs y)
        (if y >= 0 then "below" else "above" :: String)
        (abs z)
        (if z >= 0 then "behind" else "in front of" :: String)
        (abs x)
        (if x >= 0 then "left" else "right" :: String)
        sy
        sz
        sx
        (pivotTurns c)
  where
    (V3 x y z, V3 sx sy sz) = pivotInLevelAxes centre c

-- | The learnt level as the window shows it, shorter than 'describeLevel'.
describeLevelBriefly :: Calibration -> String
describeLevelBriefly c
    | levelTurning c <= 0 = "Level: " ++ notLearnt
    | otherwise =
        let (down, right) = cameraTilt (estimatedLevel c)
         in printf "Camera tipped %.1f° down and %.1f° to its right, from %.0f° of head turning." down right (levelTurning c)

-- | The learnt pivot as the window shows it, shorter than 'describePivot'.
describePivotBriefly :: Maybe Pose -> Calibration -> String
describePivotBriefly _ c | pivotTurns c <= 0 = "Neck pivot: " ++ notLearnt
describePivotBriefly Nothing c =
    printf
        "Neck pivot %.0f ± %.0f mm from the clip, from %.0f head turns."
        (norm (estimatedPivot c))
        (pivotSpreadAlong c (normalize (estimatedPivot c)))
        (pivotTurns c)
describePivotBriefly (Just centre) c =
    printf
        "Neck pivot %.0f ± %.0f mm %s the clip, %.0f ± %.0f %s, %.0f ± %.0f to your %s, from %.0f turns."
        (abs y)
        sy
        (if y >= 0 then "below" else "above" :: String)
        (abs z)
        sz
        (if z >= 0 then "behind" else "in front" :: String)
        (abs x)
        sx
        (if x >= 0 then "left" else "right" :: String)
        (pivotTurns c)
  where
    (V3 x y z, V3 sx sy sz) = pivotInLevelAxes centre c

-- | The learnt pivot along the level axes with the head at the given
-- centre, and its standard error along each, mm.
pivotInLevelAxes :: Pose -> Calibration -> (V3, V3)
pivotInLevelAxes centre c = (inLevelAxes level centre (estimatedPivot c), V3 (along (V3 1 0 0)) (along (V3 0 1 0)) (along (V3 0 0 1)))
  where
    level = estimatedLevel c
    -- The head's direction that is the given level direction at the
    -- centre, the turn from one set of axes to the other being orthogonal.
    along e = pivotSpreadAlong c (V3 (dot e (toLevel (V3 1 0 0))) (dot e (toLevel (V3 0 1 0))) (dot e (toLevel (V3 0 0 1))))
    toLevel = inLevelAxes level centre

-- | Where the clip's reflectors (left, right, top) sit on the head once
-- both the level and the pivot have been learnt, given the head's pose at
-- the centre: in mm from the neck pivot, along the head's axes as the
-- window draws it (see 'inHeadAxes').
learntClip :: Model -> Maybe Pose -> Calibration -> Maybe (V3, V3, V3)
learntClip m (Just centre) c
    | levelTurning c > 0 && pivotTurns c > 0 = Just (onHead (mLeft m), onHead (mRight m), onHead (mTop m))
  where
    onHead p = inHeadAxes (estimatedLevel c) centre (p ^-^ estimatedPivot c)
learntClip _ _ _ = Nothing

-- * Up

-- | Turns must reach this many degrees within 'turnSeconds' to count.
turnDegrees, turnSeconds :: Double
turnDegrees = 10
turnSeconds = 0.6

-- | A turn counts for at most this many degrees, so that a pose the
-- tracker got wrong for a moment, which looks like a big quick turn, counts
-- for little.
turnMost :: Double
turnMost = 2 * turnDegrees

-- | Degrees of turning remembered: a couple of minutes of looking about.
upMemory :: Double
upMemory = 1800

-- | Degrees of turning before the learnt up is used.
upConfident :: Double
upConfident = 180

-- Note [Learning up]
-- ~~~~~~~~~~~~~~~~~~
-- Up is the axis the head turns about when it looks left and right. Every
-- brisk turn, of at least 'turnDegrees' within 'turnSeconds', adds its axis
-- to a scatter matrix, the sum of each axis times itself, weighted by the
-- degrees turned (up to 'turnMost'), so that which way a turn went doesn't
-- matter. Its eigenvectors are the main axes of the head's turning: left
-- and right, nodding and tilting. A head nods about an axis across it and
-- tilts about one through its face, both square to the axis it turns left
-- and right about however far it has turned, so nods and tilts that go as
-- often one way as the other don't pull that axis aside: they make the
-- other two. Up is the one of the three nearest the camera's own up, which
-- is the left and right turning's for any camera tipped or rolled less than
-- 45 degrees, however much more the head nods or tilts than it turns.
--
-- Every turn counts, wherever its axis is, so no estimate, a wrong one
-- from a few odd turns or one from before the camera moved, can keep out
-- the evidence against it: beyond 'upMemory' degrees older turns fade as
-- new ones come, and the main axes follow. (Counting only turns near the
-- current up, as was done before, held on to a wrong up for good.) An
-- explicit calibration is taken as half of what is remembered, so that
-- everyday turning goes on refining it.
--
-- A head that rolls a little as it turns, the same way each time, turns
-- about an axis leaning toward the face or away from it, as does one whose
-- neck's turning axis leans back. That axis is up all the same: it is what
-- the head turns about, so a glance left or right is yaw alone to a game,
-- with no roll or pitch from how the neck happens to turn. So up isn't
-- quite the way gravity pulls, which turning can't show, and the camera's
-- tip told is from that axis.
--
-- The pivot lies on this axis (see Note [Learning the pivot]), so it is
-- described along it: with up wrong by some angle about the line of sight,
-- a pivot below the clip would be told as off to one side by its height
-- times the sine of that angle.

-- | Add a turn, a rotation of the head in camera axes, if it is brisk enough
-- to show an axis. See Note [Learning up].
addTurn :: M33 -> Calibration -> Maybe Calibration
addTurn rot c
    | turned < turnDegrees = Nothing
    | otherwise =
        let fade = if upWeight c + degrees > upMemory then (upMemory - degrees) / upWeight c else 1
         in Just
                c
                    { upScatter = scaleM fade (upScatter c) `addM` scaleM degrees (outer axis axis)
                    , upWeight = fade * upWeight c + degrees
                    }
  where
    v = rotationVector rot
    turned = norm v * 180 / pi
    degrees = min turnMost turned
    axis = normalize v

-- | Take an explicitly calibrated level as half of what is remembered, so
-- that everyday turning goes on refining it.
withLevel :: Level -> Calibration -> Calibration
withLevel l = turnedAbout (upMemory / 2) (levelUp l)

-- | A calibration whose turning, so many degrees of it, was all about the
-- given up.
turnedAbout :: Double -> V3 -> Calibration -> Calibration
turnedAbout weight up c = c{upScatter = scaleM weight (outer u u), upWeight = weight}
  where
    u = normalize up

-- * Pivot

-- | Whether a pivot, in the head's axes, is where a neck can be: behind the
-- reflectors, on the side away from where they face, by at least the
-- world's 'neckBehindClip', and within its 'neckWithin' of them. However the clip sits on the head, and however its post
-- is bent, the head is behind the reflectors; a pivot in front of them was
-- learnt from mirror-image poses (see 'mirrorPose'), on which the head
-- seems to turn about a point in front of the clip.
plausiblePivot :: Model -> V3 -> Bool
plausiblePivot m p = dot p (towardHead m) >= inMillimetres (neckBehindClip world) && norm p <= inMillimetres (neckWithin world)

-- | Whether what was learnt of the pivot is plausible for the given clip.
-- A calibration whose pivot isn't came from mirror-image poses, and its
-- level likely did too.
plausibleCalibration :: Model -> Calibration -> Bool
plausibleCalibration m = plausiblePivot m . estimatedPivot

-- | A saved calibration as it is, or, if it isn't plausible for the given
-- clip, why it should be forgotten.
screenCalibration :: Model -> Calibration -> Either String Calibration
screenCalibration m c
    | plausibleCalibration m c = Right c
    | otherwise =
        Left
            "its neck pivot is in front of the clip, where no neck is: it was learnt from mirror-image poses, with yaw and pitch reversed"

-- Note [Learning the pivot]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
-- Each window of turning is a least-squares problem: the point p, in the
-- model's axes, that stayed most still (see 'addWindow'). Its normal
-- equations A p = b say how well it shows each direction of p. Turning
-- left and right about a vertical axis leaves every point on that axis
-- still, so it shows where the axis is, behind the clip and to one side,
-- but nothing of how high on it the pivot is; only nods and tilts show
-- that, and they are rarer and smaller.
--
-- So what is kept is the windows' information matrix, each window
-- weighted as the evidence it is: A over how far its poses stray from
-- turning about one point (its own fit, and at least 'windowNoise', for
-- how much no one point explains a neck), counted as 'windowPoses'
-- independent poses, since a window's poses err alike. Its inverse is the
-- estimate's covariance, so the uncertainty is known direction by
-- direction, and is what the descriptions give.
--
-- A prior, the pivot of most heads give or take 'priorSpread' in each
-- direction, holds the directions little shown: until nods and tilts are
-- seen, the height stays near the usual one rather than following what a
-- few turns happen to hint. Directions well shown leave it behind within
-- a few windows.
--
-- Old evidence fades only in the directions new evidence shows, and only
-- once the estimate is known along them to within 'pivotMemorySpread':
-- turning left and right keeps where the axis of turning is up to date
-- without wearing away what nods showed of the height, and nodding later
-- moves the height however much the head turned before. So a clip moved on
-- the headset is learnt again in about as many windows as it took first.
--
-- A neck has no one pivot: it turns left and right between the top two
-- vertebrae, nods quickly at the base of the skull above them, and bends
-- lower down for big slow nods and tilts. One point is kept nonetheless,
-- since what it is for wants one point: so that turning doesn't move the
-- view (see 'relativePose'), the point that moves least over the head's
-- turning is best, and that is the least-squares point of all its windows
-- together, weighted as here, where the axis of turning is coming from the
-- turns and its height from the nods and tilts, each as often as they
-- happen. The mirror image's poses (see "TrackKat.Branch") turn about a
-- point in front of the clip, far from all of these. So the height depends
-- on how the head nods: quick small nods put it high, at the skull's base,
-- and slow deep ones low, in the neck; while it is little shown, the prior
-- holds it between.

-- | How far, mm, the pivot is from 'pivotPrior' on most heads, in any
-- direction: the standard deviation of the prior, the world's
-- 'pivotSpread'.
priorSpread :: Double
priorSpread = inMillimetres (pivotSpread world)

priorInfo :: M33
priorInfo = scaleM (1 / (priorSpread * priorSpread)) identity

-- | How far, mm, a window's poses stray from turning about one point at
-- the least, however well they fit: how far a neck is from a single pivot
-- over half a second, the world's 'neckWander'.
windowNoise :: Double
windowNoise = inMillimetres (neckWander world)

-- | How many independent poses a window's are worth.
windowPoses :: Double
windowPoses = 1

-- | Evidence is remembered along each direction until it gives the pivot
-- to within this many mm; beyond that, older evidence fades as new comes.
pivotMemorySpread :: Double
pivotMemorySpread = 3

-- | Windows remembered in the count told, 'pivotTurns'.
countMemory :: Double
countMemory = 1000

-- | How strongly a window's own fixed point is held toward the estimate
-- along what the window can't show, against 1 for each pose.
holdStrength :: Double
holdStrength = 1

-- | Length of a window, in seconds.
windowSeconds :: Double
windowSeconds = 0.5

-- | Degrees a window must turn through to say anything.
windowSpan :: Double
windowSpan = 8

-- | RMS distance, mm, by which a window's best fixed point may move; more,
-- and the body moved too: twice the world's 'neckWander'.
windowFit :: Double
windowFit = inMillimetres (times 2 (neckWander world))

-- | Sums over a window's poses, enough to fit the point that stays still.
data TurnWindow = TurnWindow
    { windowStart :: !Double
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

startWindow :: Double -> Pose -> TurnWindow
startWindow now (Pose r t) = TurnWindow now r 1 r (mulMV (transpose r) t) t (dot t t) 0

addPose :: Pose -> TurnWindow -> TurnWindow
addPose (Pose r t) w =
    w
        { wCount = wCount w + 1
        , wRot = addM (wRot w) r
        , wRotT = wRotT w ^+^ mulMV (transpose r) t
        , wTrans = wTrans w ^+^ t
        , wSquares = wSquares w + dot t t
        , wSpan = max (wSpan w) (norm (rotationVector (r `mulMM` transpose (wFirst w))))
        }

-- | Add a window if the head turned enough during it, a fixed point
-- explains its movement, and that point and the estimate with it are where
-- a neck can be on the given clip. See Note [Learning the pivot].
--
-- With rotations R_i and translations t_i, a point p in the model's axes is
-- at R_i p + t_i. Fitting p and the still position c to R_i p + t_i = c by
-- least squares, and eliminating c, leaves normal equations A p = b, with
-- A = n I - S^T S / n, b = S^T T / n - sum R_i^T t_i, S and T the sums of
-- R_i and t_i. A fit of the window alone, held near the current estimate
-- along the axis it can't see, must explain it within 'windowFit'.
addWindow :: Model -> TurnWindow -> Calibration -> Maybe Calibration
addWindow m w c
    | wSpan w < windowSpan * pi / 180 || fit > windowFit || not (plausiblePivot m own) = Nothing
    | not (plausiblePivot m (estimatedPivot added)) = Nothing
    | otherwise = Just added
  where
    (a, b) = normalEquations w
    (own, fit) = fixedPoint holdStrength (estimatedPivot c) w
    weight = windowPoses / (fromIntegral (wCount w) * (fit * fit + windowNoise * windowNoise))
    info = scaleM weight a
    (remembered, rememberedMoment) = forgetBeyondMemory (pivotInfo c `addM` info, pivotMoment c ^+^ weight *^ b)
    added =
        c
            { pivotInfo = remembered
            , pivotMoment = rememberedMoment
            , pivotWindows = pivotWindows c * (1 - 1 / countMemory) + 1
            }

-- | Information about the pivot from windows, and its moment, with
-- evidence taken away along any direction where there is more than
-- 'pivotMemorySpread' allows, down to that. Directions that new windows
-- add little to keep what they have. What is taken away is a share of all
-- the evidence along that direction, so what remains points to the same
-- estimate.
forgetBeyondMemory :: (M33, V3) -> (M33, V3)
forgetBeyondMemory (info0, moment0) = foldl' forget (info0, moment0) (eigenSymmetric info0)
  where
    most = 1 / (pivotMemorySpread * pivotMemorySpread)
    forget (info, moment) (e, u)
        | e <= most = (info, moment)
        | otherwise = (info `addM` scaleM (most - e) (outer u u), moment ^-^ ((1 - most / e) * dot u moment) *^ u)

-- | The normal equations A p = b of a window's fit of a fixed point.
normalEquations :: TurnWindow -> (M33, V3)
normalEquations w = (a, b)
  where
    n = fromIntegral (wCount w)
    s = wRot w
    a = scaleM n identity `addM` scaleM (-1 / n) (transpose s `mulMM` s)
    b = (1 / n) *^ mulMV (transpose s) (wTrans w) ^-^ wRotT w

-- | The point, in the model's axes, that moved least during a window, held
-- toward the given point with the given strength (against 1 for each pose
-- and squared mm moved); and the RMS distance, mm, it moved.
fixedPoint :: Double -> V3 -> TurnWindow -> (V3, Double)
fixedPoint strength near w = (p, sqrt (max 0 (dot p (mulMV a p) - 2 * dot b p + squares) / n))
  where
    (a, b) = normalEquations w
    n = fromIntegral (wCount w)
    squares = wSquares w - dot (wTrans w) (wTrans w) / n
    p = fromMaybe near (solve3 (a `addM` scaleM strength identity) (b ^+^ strength *^ near))

-- | The furthest, in degrees, the head turned during a window from where
-- it was at its start.
windowTurn :: TurnWindow -> Double
windowTurn w = wSpan w * 180 / pi

-- | How badly a window's poses fit turning about a point near the given
-- one, in the model's axes: the mean square, mm squared, of how far the best
-- such point moved, plus the given weight times its squared distance from
-- the given point.
turnFit :: Double -> V3 -> TurnWindow -> Double
turnFit weight near w = moved * moved + weight * dot off off
  where
    (p, moved) = fixedPoint (weight * fromIntegral (wCount w)) near w
    off = p ^-^ near

solve3 :: M33 -> V3 -> Maybe V3
solve3 (M33 (V3 a b d) (V3 e f g) (V3 h i j)) (V3 x y z) =
    case solveLinear [[a, b, d], [e, f, g], [h, i, j]] [x, y, z] of
        Just [p, q, r] -> Just (V3 p q r)
        _ -> Nothing

addM :: M33 -> M33 -> M33
addM (M33 a b c) (M33 d e f) = M33 (a ^+^ d) (b ^+^ e) (c ^+^ f)

scaleM :: Double -> M33 -> M33
scaleM k (M33 a b c) = M33 (k *^ a) (k *^ b) (k *^ c)

zeroM :: M33
zeroM = M33 (V3 0 0 0) (V3 0 0 0) (V3 0 0 0)

outer :: V3 -> V3 -> M33
outer (V3 a b d) v = M33 (a *^ v) (b *^ v) (d *^ v)

-- | The eigenvalues and unit eigenvectors of a symmetric matrix, by Jacobi
-- rotations.
eigenSymmetric :: M33 -> [(Double, V3)]
eigenSymmetric m0 = [(at diagonal i i, column vectors i) | i <- [0, 1, 2]]
  where
    rows (M33 (V3 a b c) (V3 d e f) (V3 g h i)) = [[a, b, c], [d, e, f], [g, h, i]]
    at x i j = x !! i !! j
    column x j = V3 (at x 0 j) (at x 1 j) (at x 2 j)
    (diagonal, vectors) = iterate sweep (rows m0, rows identity) !! 8
    sweep acc = foldl' rotate acc [(0, 1), (0, 2), (1, 2)]
    -- Zero the (p, q) element with a rotation J: A to J^T A J, V to V J.
    rotate (am, vm) (p, q)
        | abs apq < 1e-300 = (am, vm)
        | otherwise = (mul (transposeL j) (mul am j), mul vm j)
      where
        apq = at am p q
        theta = (at am q q - at am p p) / (2 * apq)
        t = signum theta / (abs theta + sqrt (theta * theta + 1)) + (if theta == 0 then 1 else 0)
        cs = 1 / sqrt (t * t + 1)
        sn = t * cs
        j = [[entry r k | k <- [0, 1, 2]] | r <- [0, 1, 2]]
        entry r k
            | (r, k) == (p, p) || (r, k) == (q, q) = cs
            | (r, k) == (p, q) = sn
            | (r, k) == (q, p) = negate sn
            | r == k = 1
            | otherwise = 0
    mul x y = [[sum (zipWith (*) row col) | col <- transposeL y] | row <- x]
    transposeL [r0, r1, r2] = zipWith3 (\x y z -> [x, y, z]) r0 r1 r2
    transposeL _ = []

-- * Learning

-- | Learning in progress, and the calibration the tracker applies, which
-- follows the estimates slowly.
data Learner = Learner
    { lModel :: !Model
    -- ^ the clip, which says where a neck can be
    , lCalibration :: !Calibration
    , lTurn :: !(Maybe (Double, M33))
    -- ^ time and rotation at the start of the turn being watched
    , lWindow :: !(Maybe TurnWindow)
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

-- | Start learning about a head wearing the given clip from a calibration,
-- applying it at once.
newLearner :: Model -> Calibration -> Learner
newLearner m c = Learner m mc Nothing Nothing Nothing up pivot up pivot
  where
    mc = mountedCalibration (mMount m) c
    up = estimatedUp mc
    pivot = estimatedPivot mc

learnerCalibration :: Learner -> Calibration
learnerCalibration = lCalibration

appliedLevel :: Learner -> Level
appliedLevel = levelFromUp . lUp

-- | The pivot applied, in the model's axes, mm.
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
            | now - windowStart w >= windowSeconds -> (fromMaybe c1 (addWindow (lModel l) w c1), Nothing)
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

-- | The clip went out of sight, or its pose is in doubt: turns and windows
-- in progress end.
lostSight :: Learner -> Learner
lostSight l = l{lTurn = Nothing, lWindow = Nothing, lLast = Nothing}

-- | Learn about a head wearing a different clip from now on.
changeClip :: Model -> Learner -> Learner
changeClip m l = l{lModel = m, lCalibration = mountedCalibration (mMount m) (lCalibration l)}

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
        , "# matrix of its least-squares fit, the inverse of its covariance, per"
        , "# square cm: xx xy xz yy yz zz."
        , "pivot = " ++ vector pivot
        , "pivot windows = " ++ renderNumber (pivotWindows c)
        , "pivot information = " ++ unwords (map renderNumber [a, b, d, f, g, j])
        ]
  where
    vector (V3 x y z) = unwords (map renderNumber [x, y, z])
    pivot = estimatedPivot c
    M33 (V3 a b d) (V3 _ f g) (V3 _ _ j) = scaleM perSquareCm (pivotPrecision c)

-- | Information per square mm in square cm.
perSquareCm :: Double
perSquareCm = 100

-- | Information about the pivot as saved, per square cm with the prior's,
-- as information from windows alone, per square mm, no more than is
-- remembered (see Note [Learning the pivot]). Builds before this one saved
-- the normal matrices of the windows' fits, with 1 for the prior, which
-- come to more than is now remembered. Their windows count as they would
-- now with their poses straying 'windowNoise' and a little, but for no more
-- than a tenth of what is remembered along any direction: they were taken
-- for surer than they were, the height especially, so what is learnt now
-- soon outweighs them.
windowInfo :: M33 -> M33
windowInfo saved = foldr add zeroM [(min limit (max 0 e), u) | (e, u) <- eigenSymmetric fromWindows]
  where
    most = 1 / (pivotMemorySpread * pivotMemorySpread)
    precision = scaleM (1 / perSquareCm) saved
    olderBuild = maximum (map fst (eigenSymmetric precision)) > 1.01 * (most + 1 / (priorSpread * priorSpread))
    (fromWindows, limit)
        | olderBuild = (scaleM (windowPoses / (fromIntegral olderWindowPoses * (1 + windowNoise * windowNoise))) (saved `addM` scaleM (-1) identity), most / 10)
        | otherwise = (precision `addM` scaleM (-1) priorInfo, most)
    add (e, u) acc = acc `addM` scaleM e (outer u u)

-- | Poses in a window of earlier builds: a window's at the world's
-- 'frameRate', 60.
olderWindowPoses :: Int
olderWindowPoses = round (windowSeconds * inPerSecond (frameRate world))

-- | Read a calibration file. Settings it lacks start from scratch; the
-- error names the offending line.
parseCalibration :: String -> Either String Calibration
parseCalibration str = do
    s <- foldl' line (Right emptyFile) (zip [1 :: Int ..] (lines str))
    let up = maybe cameraUp normalize (fUp s)
        -- Earlier builds remembered twice as much turning.
        weight = min upMemory (fromMaybe 0 (fTurning s))
        info = maybe zeroM windowInfo (fInfo s)
        pivot = fromMaybe (estimatedPivot defaultCalibration) (fPivot s)
        prior = pivotPrior defaultCalibration
    pure
        ( turnedAbout
            weight
            up
            Calibration
                { upScatter = zeroM
                , upWeight = 0
                , pivotInfo = info
                , pivotMoment = mulMV (priorInfo `addM` info) pivot ^-^ mulMV priorInfo prior
                , pivotWindows = fromMaybe 0 (fWindows s)
                , pivotPrior = prior
                }
        )
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
