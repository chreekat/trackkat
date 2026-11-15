-- | Simulated heads and clips in front of the camera, tracked: what the
-- simulations of several subjects share.
module Test.Simulation (
    -- * Clips and heads
    deg,
    headset,
    clipStraight,
    clipBent,
    headSeen,
    glanceYaw,
    glancePitch,
    glanceRoll,
    Subject (..),
    subjectClip,
    userTabs,
    unbentTabs,
    scaleneTabs,
    framesAt,
    clipFrames,
    rendered,

    -- * Tracking
    Row,
    trackFrames,
    trackFramesEvents,
    trackFramesEventsWith,
    trackPath,
    trackNoisy,
    runLearnerWith,
    shapeSession,

    -- * What tracking came to
    corrections,
    isMirrored,
    mirroredOn,
    lastMirrored,
    lastMirroredOn,
    outputError,
    poseError,
    turnError,
    lostCount,
    isLost,
    worstError,
    seens,
    unmerged,
    after,
    distance,
    summed,

    -- * Scenes
    at120,
    at30,
    straightWorn,
    bentWorn,
    glance,
    fireAt,
    raised,
    fidgeting,
    slipping,
    slipTurn,
) where

import Data.List (mapAccumL)

import TrackKat.Calibration
import TrackKat.Config
import TrackKat.Frame
import TrackKat.Linear
import TrackKat.Pose
import TrackKat.Shape
import TrackKat.Synth
import TrackKat.Tracker
import TrackKat.World (clockAt)

-- | A frame's time, the head's true pose, what tracking made of the frame,
-- and the pose tracked.
type Row = (Double, Pose, Outcome, Maybe Pose)

-- | Radians, from degrees.
deg :: Double -> Double
deg = (* (pi / 180))

-- | A headset clip with its post leaning back 45 degrees.
headset :: Model
headset = maybe (error "bad test triangle") id (triangle 45 120 150 150)

-- | Glancing about, as in simulations of learning the clip's shape: degrees
-- of yaw, pitch and roll at a time.
glanceYaw, glancePitch, glanceRoll :: Double -> Double
glanceYaw t = 22 * sin (2 * pi * t / 3.3) + 8 * sin (2 * pi * t / 1.7 + 1) + 4 * sin (2 * pi * t / 0.9 + 2)
glancePitch t = 10 * sin (2 * pi * t / 5.3 + 0.5) + 5 * sin (2 * pi * t / 2.1 + 1.3)
glanceRoll t = 5 * sin (2 * pi * t / 7.7 + 0.2) + 2 * sin (2 * pi * t / 1.3 + 0.9)

-- | Learn the shape of a clip configured as one model, worn as another,
-- from the given minutes of glancing at 120 frames a second, swaying, the
-- reflectors seen with 0.2 px of noise and the poses solved with the
-- configured model: what was learnt, and the estimates told.
shapeSession :: Model -> Model -> Double -> (ShapeLearner, [ShapeEstimate])
shapeSession truth configured minutes = go (0 :: Int) (seenPose 0) (newShapeLearner configured) []
  where
    frames = round (minutes * 60 * 120)
    seenPose t =
        let Pose r p = headSeen (-23) (glanceYaw t) (glancePitch t) (glanceRoll t)
         in Pose r (p ^+^ V3 (6 * sin (2 * pi * t / 11) + 2 * sin (2 * pi * t / 2.7)) (4 * sin (2 * pi * t / 13)) (5 * sin (2 * pi * t / 9)))
    go i prev sl told
        | i >= frames = (sl, reverse told)
        | otherwise =
            let t = fromIntegral i / 120
                noisy j (u, v) = (u + 0.2 * gaussian i j, v + 0.2 * gaussian i (j + 0.5))
                seen which j = noisy j (seenAt defaultIntrinsics truth (seenPose t) which)
             in case solvePose defaultIntrinsics configured (Just prev) (seen LeftReflector 0, seen RightReflector 1, seen TopReflector 2) of
                    Just (pose, _) ->
                        let (sl', new) = watchShape defaultIntrinsics (clockAt t) (V3 0 100 100) pose sl
                         in sl' `seq` go (i + 1) pose sl' (maybe told (: told) new)
                    Nothing -> go (i + 1) prev (shapeLostSight sl) told

-- | The headset clip as first made, and with its post bent.
clipStraight, clipBent :: Model
clipStraight = maybe (error "clipStraight") id (triangle 45 70 116 116)
clipBent = maybe (error "clipBent") id (triangle 45 70 110 110)

-- | The clip seen by the user's camera (see 'userCamera'), worn pitched by
-- the given degrees (see 'wornPitched'), the head turned by the given yaw,
-- pitch and roll in degrees.
headSeen :: Double -> Double -> Double -> Double -> Pose
headSeen worn = headPose userCamera (wornPitched worn)

-- | Track frames of (time, true pose, stripes) from a fresh tracker: each
-- frame's time, true pose, outcome, and the pose tracked.
trackFrames :: Model -> [(Double, Pose, [Stripe])] -> [Row]
trackFrames m = map fst . snd . trackFramesEvents m

-- | Track frames as 'trackFrames' does, with each frame's events, and the
-- tracker at the end.
trackFramesEvents :: Model -> [(Double, Pose, [Stripe])] -> (Tracker, [(Row, [Event])])
trackFramesEvents = trackFramesEventsWith (lightMasking defaultConfig)

-- | Track frames as 'trackFramesEvents' does, still lights masked or not.
trackFramesEventsWith :: Masking -> Model -> [(Double, Pose, [Stripe])] -> (Tracker, [(Row, [Event])])
trackFramesEventsWith masking m = mapAccumL frame (newTracker defaultIntrinsics m masking defaultCalibration)
  where
    frame t (time, truth, stripes) = let (t', o, evs) = step (clockAt time) stripes t in (t', ((time, truth, o, trackerPose t'), evs))

distance :: (Double, Double) -> (Double, Double) -> Double
distance (a, b) (c, d) = sqrt ((a - c) ^ (2 :: Int) + (b - d) ^ (2 :: Int))

-- | Track a head along a path of (time, true pose, how lit each reflector is).
trackPath :: Model -> [(Double, Pose, (Double, Double, Double))] -> [Row]
trackPath m path = trackFrames m (rendered m path)

-- | Frames of a head along a path, as 'trackPath' sees them.
rendered :: Model -> [(Double, Pose, (Double, Double, Double))] -> [(Double, Pose, [Stripe])]
rendered m path = [(t, pose, renderDiscs (pointDiscs defaultIntrinsics m lit pose)) | (t, pose, lit) <- path]

-- | Track a head along a path as 'trackPath' does, with each reflector's
-- centre moved by up to half the given pixels, differently each frame, as
-- noise moves a blob's centroid.
trackNoisy :: Model -> Double -> [(Double, Pose, (Double, Double, Double))] -> (Tracker, [(Row, [Event])])
trackNoisy m amp path =
    trackFramesEvents m [(t, pose, renderDiscs (jitter amp i (pointDiscs defaultIntrinsics m lit pose))) | (i, (t, pose, lit)) <- zip [0 :: Int ..] path]

-- | Times of the frames where a pose was corrected.
corrections :: [(Row, [Event])] -> [Double]
corrections rows = [t | ((t, _, _, _), evs) <- rows, any isCorrected evs]
  where
    isCorrected Corrected{} = True
    isCorrected _ = False

-- | Whether the pose tracked is the mirror image of the true pose rather
-- than the true pose.
isMirrored :: Row -> Bool
isMirrored = mirroredOn clipBent

-- | Whether the pose tracked is the mirror image of the true pose of a
-- clip of the given shape rather than the true pose.
mirroredOn :: Model -> Row -> Bool
mirroredOn m (_, truth, _, tracked) = case (mirrorPose defaultIntrinsics m truth, tracked) of
    (Just other, Just p) -> fst (poseError other p) < fst (poseError truth p) && fst (poseError truth p) > 3 && fst (poseError truth other) > 10
    _ -> False

-- | The time of the last frame tracked on the mirror image, or 0.
lastMirrored :: [Row] -> Double
lastMirrored = lastMirroredOn clipBent

-- | The time of the last frame tracked on the mirror image of a clip of
-- the given shape, or 0.
lastMirroredOn :: Model -> [Row] -> Double
lastMirroredOn m rows = maximum (0 : [t | r@(t, _, _, _) <- rows, mirroredOn m r])

-- | The worst error, in degrees, of the yaw and pitch reported, against
-- the head's true yaw and pitch at each time.
outputError :: (Double -> (Double, Double)) -> [Row] -> Double
outputError turned rows = maximum (0 : [max (abs (yaw hp - y)) (abs (pitch hp - p)) | (t, _, Tracked{outPose = hp}, _) <- rows, let (y, p) = turned t])

-- | Learn from (time, pose) pairs from scratch, for a head wearing the
-- given clip: the learner, and what it reported.
runLearnerWith :: Model -> [(Double, Pose)] -> (Learner, [Refinement])
runLearnerWith m = foldl (\(l, rs) (time, pose) -> let (l', new) = learn (clockAt time) pose l in (l', rs ++ new)) (newLearner m defaultCalibration, [])

-- | Degrees of turning and mm of movement between two poses.
poseError :: Pose -> Pose -> (Double, Double)
poseError a b = (norm (rotationVector (poseR a `mulMM` transpose (poseR b))) * 180 / pi, norm (poseT a ^-^ poseT b))

lostCount :: [Row] -> Int
lostCount rows = length [() | (_, _, Lost{}, _) <- rows]

isLost :: Outcome -> Bool
isLost Lost{} = True
isLost Tracked{} = False

-- | The worst error of the poses tracked: degrees, mm.
worstError :: [Row] -> (Double, Double)
worstError rows = (maximum (0 : map fst errors), maximum (0 : map snd errors))
  where
    errors = [poseError truth p | (_, truth, Tracked{}, Just p) <- rows]

seens :: [Row] -> [Seen]
seens rows = [s | (_, _, Tracked{outSeen = s}, _) <- rows]

-- | The frames where no reflectors shared a blob.
unmerged :: [Row] -> [Row]
unmerged rows = [r | r@(_, _, Tracked{outSeen = Seen{seenMerged = 0}}, _) <- rows]

-- | The frames from the given time on.
after :: Double -> [Row] -> [Row]
after t0 = filter (\(t, _, _, _) -> t >= t0)

-- | The mean and worst errors of the turning and movement tracked from
-- the first frame, against the head's: degrees, and mm at the clip's
-- middle.
turnError :: [Row] -> ((Double, Double), (Double, Double))
turnError rows = case [(truth, p) | (_, truth, Tracked{}, Just p) <- rows] of
    (truth0, p0) : rest ->
        let errors = [(fst (poseError turned seen), norm (middle turned ^-^ middle seen)) | (truth, p) <- rest, let turned = moved truth0 truth; seen = moved p0 p]
            mean xs = sum xs / fromIntegral (max 1 (length xs))
         in ((mean (map fst errors), mean (map snd errors)), (maximum (0 : map fst errors), maximum (0 : map snd errors)))
    [] -> ((0, 0), (0, 0))
  where
    -- The rigid movement from one pose to another.
    moved (Pose r0 t0) (Pose r t) = let r' = r `mulMM` transpose r0 in Pose r' (t ^-^ mulMV r' t0)
    middle pose = poseAt pose (V3 0 0 900)

-- * Clips of other shapes and looks

-- | A clip to simulate: a name for it, the model the tracker is given,
-- the clip as it truly is (the same model) and how its reflectors show,
-- worn pitched 23 degrees back as the user's is (see 'wornPitched').
data Subject = Subject String Model Look

subjectClip :: Subject -> Clip
subjectClip (Subject _ m look) = Clip m look (wornPitched (-23))

-- | The clips the key simulations run with: the user's, and others that
-- differ from it in the ways clips do. (Other simulations use points.)
userTabs, unbentTabs, scaleneTabs :: Subject
userTabs = Subject "the user's clip, 70 110 110 on tabs, its top tab bent" (clipModel (clip defaultConfig)) AsTape
unbentTabs = Subject "a clip 70 116 116 on tabs, its post and top tab unbent" (tabbed 0 70 116 116) AsTape
scaleneTabs = Subject "a scalene clip, 70 130 95 on tabs" (tabbed 37.5 70 130 95) AsTape

-- | A clip as 'clipShape' makes one, with its top tab bent by the given
-- degrees.
tabbed :: Double -> Double -> Double -> Double -> Model
tabbed bend lr lt rt = maybe (error "tabbed") (\m -> m{mTabs = (mTabs m){postBend = bend}}) (clipModel <$> clipShape lr lt rt)

-- | Frames of a clip along a path of (time, true pose, how lit each
-- reflector is), each reflector's centre moved by up to half the given
-- pixels.
clipFrames :: Clip -> Double -> [(Double, Pose, Lit)] -> [(Double, Pose, [Stripe])]
clipFrames c amp path = [(t, pose, renderDiscs (jitter amp i (clipDiscs defaultIntrinsics c lit pose))) | (i, (t, pose, lit)) <- zip [0 :: Int ..] path]

-- | Times of frames at so many a second, for so many seconds.
framesAt :: Double -> Double -> [Double]
framesAt rate seconds = [fromIntegral i / rate | i <- [0 .. round (seconds * rate) :: Int]]

-- * Scenes

-- | Times of frames at 120 and at 30 a second, for so many seconds.
at120, at30 :: Double -> [Double]
at120 = framesAt 120
at30 = framesAt 30

-- | The pitch the clip is worn at (see 'wornPitched') for its top reflector
-- to show 15 px above the lower pair as first made, and, with its post
-- bent, 40 px above.
straightWorn, bentWorn :: Double
straightWorn = -49
bentWorn = -23

-- | Glancing left and right from the given time on: degrees of yaw at a
-- time.
glance :: Double -> Double -> Double
glance from t = if t < from then 0 else 25 * sin (2 * pi * (t - from) / 2.5)

-- | How many frames were lost, and the worst error of the poses tracked.
summed :: [Row] -> (Int, (Double, Double))
summed rows = (lostCount rows, worstError rows)

-- | Where a fire behind the user shows, as the user's camera saw one.
fireAt :: (Double, Double)
fireAt = (156, 297)

-- | A pose moved so that the clip is high in the image, its lower pair about
-- 60 pixels apart around (300, 100), as on the user's camera.
raised :: Pose -> Pose
raised (Pose r p) = Pose r (p ^+^ V3 (-26) (-165) 0)

-- | A head fidgeting, raised, at a time.
fidgeting :: Double -> Pose
fidgeting t = raised (headSeen bentWorn (3 * sin (2 * pi * t / 3)) (2 * sin (2 * pi * t / 4.3)) 0)

-- | A head nodding through where the clip's triangle squarely faces the
-- camera, the clip out of sight for a moment meanwhile, then glancing about:
-- a path of (time, true pose, how lit each reflector is).
slipping :: [(Double, Pose, (Double, Double, Double))]
slipping = [(t, uncurry (headSeen 30) (slipTurn t) 0, if t > 1.5 && t < 2.2 then (0, 0, 0) else allLit) | t <- at120 12]

-- | The yaw and pitch of the head of 'slipping' at a time.
slipTurn :: Double -> (Double, Double)
slipTurn t = (glance 3 t, max 0 (min 20 (20 * (t - 1) / 1.5)))
