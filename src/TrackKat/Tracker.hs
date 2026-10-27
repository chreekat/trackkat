-- | Turning camera frames into head poses, frame by frame.
module TrackKat.Tracker (
    Tracker,
    newTracker,
    trackerCalibration,
    trackerCentre,
    trackerPose,
    Request (..),
    request,
    setModel,
    Event (..),
    Outcome (..),
    Seen (..),
    describeSeen,
    seenNotes,
    findBlobs,
    step,
) where

import Data.Foldable (toList)
import Data.List (intercalate, sortOn)
import Data.Maybe (catMaybes, isJust, isNothing, listToMaybe, mapMaybe)

import TrackKat.Calibration
import TrackKat.Frame (Blob (..), Stripe, stripesToBlobs)
import TrackKat.Linear (M33, mulMM, norm, transpose)
import TrackKat.Match
import TrackKat.Pose

-- | Something the user asked for, carried out on the next tracked frame.
data Request = Recenter | Calibrate
    deriving (Eq, Show)

-- | Something that happened while handling a frame.
data Event
    = Recentered
    | CalibrationStarted
    | -- | The level was calibrated on request and applied at once; the
      -- calibration with it.
      Calibrated Calibration
    | CalibrationFailed
    | -- | Everyday movement changed the calibration noticeably; what is
      -- applied follows it slowly.
      Refined Refinement Calibration
    deriving (Show)

-- | What a frame showed.
data Outcome
    = Tracked
        { outPose :: HeadPose
        , outDistance :: Double
        -- ^ mm from the camera to the clip
        , outFit :: Double
        -- ^ RMS reprojection error, pixels
        , outSeen :: Seen
        }
    | -- | The clip couldn't be made out; how many blobs there were.
      Lost Int
    deriving (Show)

-- | How the reflectors appeared in a tracked frame.
data Seen = Seen
    { seenMissing :: Int
    -- ^ reflectors out of sight, held where recent frames had them
    , seenMerged :: Int
    -- ^ reflectors sharing a blob with another
    , seenStrays :: Int
    -- ^ blobs that weren't reflectors
    }
    deriving (Eq, Show)

-- | A few words to end a status line with on anything unusual about how
-- the reflectors appeared, like "   2 of 3 reflectors, +1 stray"; empty if
-- they were three blobs and nothing else.
describeSeen :: Seen -> String
describeSeen seen = case seenNotes seen of
    [] -> ""
    notes -> "   " ++ intercalate ", " notes

-- | What was unusual about how the reflectors appeared, like
-- @["2 of 3 reflectors", "+1 stray"]@.
seenNotes :: Seen -> [String]
seenNotes (Seen missing merged strays) =
    [show (3 - missing) ++ " of 3 reflectors" | missing > 0]
        ++ ["merged" | merged > 0]
        ++ ["+" ++ show strays ++ (if strays == 1 then " stray" else " strays") | strays > 0]

-- | A requested calibration of level in progress.
data Sweep
    = Sweep
        -- | deadline, in seconds on the clock given to 'step'
        Double
        -- | rotations relative to the reference, newest first
        [M33]

-- | The last pose tracked.
data Recent = Recent
    { recentPose :: Pose
    , recentTime :: Double
    -- ^ in seconds on the clock given to 'step'
    , recentAllSeen :: Double
    -- ^ when all three reflectors were last seen
    }

data Tracker = Tracker
    { trIntrinsics :: Intrinsics
    , trModel :: Model
    , trRecent :: Maybe Recent
    -- ^ predicts the next frame, and is the solver's starting point
    , trSizes :: Three Size
    , trReference :: Maybe Pose
    , trLearner :: Learner
    , trSweep :: Maybe Sweep
    , trPending :: [Request]
    -- ^ oldest first
    }

-- | A tracker that starts from the given calibration and recenters on the
-- first frame it tracks.
newTracker :: Intrinsics -> Model -> Calibration -> Tracker
newTracker k m c = Tracker k m Nothing (pure noSize) Nothing (newLearner c) Nothing [Recenter]

-- | What the tracker has learnt so far.
trackerCalibration :: Tracker -> Calibration
trackerCalibration = learnerCalibration . trLearner

-- | The clip's pose at the last recenter.
trackerCentre :: Tracker -> Maybe Pose
trackerCentre = trReference

-- | The clip's pose in the camera's axes, as last tracked.
trackerPose :: Tracker -> Maybe Pose
trackerPose = fmap recentPose . trRecent

request :: Request -> Tracker -> Tracker
request r t = t{trPending = trPending t ++ [r]}

-- | Track a differently shaped clip from the next frame, recentering then,
-- since the clip's axes move with its shape.
setModel :: Model -> Tracker -> Tracker
setModel m t = request Recenter t{trModel = m, trRecent = Nothing}

-- | Seconds of head turning to calibrate level from.
calibrationSeconds :: Double
calibrationSeconds = 5

-- | Blobs smaller or larger than this many pixels are not reflectors, when
-- finding the clip afresh.
minBlobPixels, maxBlobPixels :: Int
minBlobPixels = 15
maxBlobPixels = 2500

-- | The smallest blob taken for a reflector predicted nearby: one fading
-- from view, or part of one.
minFaintPixels :: Int
minFaintPixels = 6

-- | The blobs in a frame that could be reflectors, heaviest first.
findBlobs :: [Stripe] -> [Blob]
findBlobs = stripesToBlobs minFaintPixels maxBlobPixels

-- | For how long, in seconds, a pose predicts where the reflectors are. After
-- longer out of sight, the clip is found afresh.
predictionSeconds :: Double
predictionSeconds = 0.5

-- | For how long, in seconds since all three were last seen, two reflectors
-- are enough to track.
twoReflectorSeconds :: Double
twoReflectorSeconds = 1

-- | The most a head turns (degrees) and moves (mm, at its pivot) between
-- frames the given number of seconds apart: from one frame to the next,
-- about what the shaping for games allows, and more the longer apart. A
-- bigger jump means the blobs were matched to the wrong reflectors.
maxTurn, maxMove :: Double -> Double
maxTurn dt = 15 + 100 * dt
maxMove dt = 40 + 200 * dt

-- | The worst RMS reprojection error, in pixels, of a pose matched from a
-- prediction.
maxFit :: Double
maxFit = 2

-- | Seconds over which a reflector's usual size is forgotten.
sizeMemory :: Double
sizeMemory = 10

-- | What a frame showed, when the clip was made out.
data Sighting
    = Sighting
        Pose
        -- | RMS reprojection error, pixels
        Double
        (Three (Maybe Sight))
        -- | blobs that weren't reflectors
        Int

-- | Handle one frame's stripes, seen at the given time in seconds. Pending
-- requests wait for a frame where the clip is tracked.
step :: Double -> [Stripe] -> Tracker -> (Tracker, Outcome, [Event])
step now stripes t =
    case sighting of
        Nothing -> (t{trLearner = lostSight (trLearner t)}, Lost (length blobs), [])
        Just (Sighting pose fit sights unused) ->
            let allSeen = all isJust sights
                recent = Recent pose now (if allSeen then now else maybe now recentAllSeen (trRecent t))
                sizes = resize (metres pose) <$> trSizes t <*> sights
                (t1, requested) = foldl (handle pose) (t{trRecent = Just recent, trSizes = sizes, trPending = []}, []) (trPending t)
                ref = maybe pose id (trReference t1)
                (learner, refinements) = learn now pose (trLearner t1)
                (t2, calibrated) = calibrate pose ref t1{trLearner = learner}
                applied = trLearner t2
                seen =
                    Seen
                        (length (filter isNothing (toList sights)))
                        (length [() | Just s <- toList sights, sightKind s == Merged])
                        unused
             in ( t2
                , Tracked (relativePose (appliedLevel applied) (appliedPivot applied) ref pose) (norm (poseT pose)) fit seen
                , reverse requested ++ [Refined r (learnerCalibration learner) | r <- refinements] ++ calibrated
                )
  where
    -- Big enough to be a reflector: of a fair size, and not tiny beside the
    -- biggest blob, as other reflections often are.
    big b = blobPoints b >= minBlobPixels && blobPoints b * 25 >= biggest * 3
    biggest = maximum (0 : map blobPoints blobs)
    k = trIntrinsics t
    m = trModel t
    blobs = findBlobs stripes
    pivot = appliedPivot (trLearner t)
    metres pose = norm (poseT pose) / 1000
    sighting = case trRecent t of
        Just r | now - recentTime r <= predictionSeconds -> follow r
        _ -> findAfresh
    -- With nothing to predict from, three big blobs by their layout,
    -- solved from where the clip was last seen if anywhere.
    findAfresh = do
        (l, r, top) <- identify (\b -> (blobX b, blobY b)) (filter big blobs)
        let at b = (blobX b, blobY b)
            whole b = Just (Sight (at b) (fromIntegral (blobPoints b)) Whole)
        (pose, fit) <- solvePose k m (recentPose <$> trRecent t) (at l, at r, at top)
        Just (Sighting pose fit (Three (whole l) (whole r) (whole top)) (length blobs - 3))
    -- The best match to the prediction, unless its pose is an impossible
    -- jump; then, of the next few that see as many reflectors, the one whose
    -- pose is nearest. (Fewer reflectors can always be fitted by a pose
    -- near the last, so they would hide a real jump.)
    follow r =
        let dt = now - recentTime r
            twoAllowed = now - recentAllSeen r <= twoReflectorSeconds
            candidates = [c | c <- matches k m (recentPose r) (trSizes t) blobs, twoAllowed || all isJust (matchSights c)]
            attempt c = do
                (pose, fit) <- solveNear k pivot (recentPose r) (observations (metres (recentPose r)) c)
                let (turn, move) = poseChange pivot (recentPose r) pose
                    jump = max (turn / maxTurn dt) (move / maxMove dt)
                if fit <= maxFit && jump <= 1
                    then Just (jump, Sighting pose fit (matchSights c) (matchUnused c))
                    else Nothing
         in case candidates of
                [] -> Nothing
                c : cs -> case attempt c of
                    Just (_, s) -> Just s
                    Nothing ->
                        let alike = [a | a <- cs, seenCount a == seenCount c]
                         in fmap snd (listToMaybe (sortOn fst (mapMaybe attempt (take 5 alike))))
    seenCount c = length (catMaybes (toList (matchSights c)))
    observations z c = catMaybes (toList (observe z <$> modelPoints m <*> trSizes t <*> matchSights c))
    -- A dim reflector, smaller than usual, counts for less: part of it may
    -- be too dim to see, moving its centroid. One sharing a blob counts for
    -- half, its place being an estimate.
    observe z p size = fmap $ \s ->
        let usual = sizeUsual size / (z * z)
            dim = if usual > 0 then min 1 (sightPixels s / usual) else 1
            shared = if sightKind s == Merged then 0.5 else 1
         in Observation p (dim * shared) (sightAt s)
    -- Reflectors' sizes, at 1 m, after a frame at the given distance.
    resize z size seen =
        let since = maybe 0 (\r -> now - recentTime r) (trRecent t)
            usual = sizeUsual size * exp (negate since / sizeMemory)
         in case seen of
                Nothing -> size{sizeUsual = usual}
                Just s -> let atOneMetre = sightPixels s * z * z in Size atOneMetre (max atOneMetre usual)
    handle pose (acc, evs) Recenter = (acc{trReference = Just pose}, Recentered : evs)
    handle _ (acc, evs) Calibrate =
        (acc{trSweep = Just (Sweep (now + calibrationSeconds) [])}, CalibrationStarted : evs)
    calibrate pose ref acc = case trSweep acc of
        Nothing -> (acc, [])
        Just (Sweep deadline rots)
            | now < deadline ->
                (acc{trSweep = Just (Sweep deadline (poseR pose `mulMM` transpose (poseR ref) : rots))}, [])
            | otherwise -> case levelFromYaws rots of
                Nothing -> (acc{trSweep = Nothing}, [CalibrationFailed])
                Just l ->
                    let acc' = acc{trSweep = Nothing, trLearner = calibrateLevel l (trLearner acc)}
                     in (acc', [Calibrated (trackerCalibration acc')])
