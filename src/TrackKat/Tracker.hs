-- | Turning camera frames into head poses, frame by frame.
module TrackKat.Tracker (
    Tracker,
    newTracker,
    Masking (..),
    trackerCalibration,
    trackerCentre,
    trackerPose,
    BlobRole (..),
    trackerBlobs,
    Request (..),
    request,
    setModel,
    setMasking,
    Event (..),
    Outcome (..),
    Seen (..),
    describeSeen,
    describeMasked,
    describeLightIgnored,
    describeLightForgotten,
    findBlobs,
    step,
) where

import Data.Foldable (toList)
import Data.List (intercalate, partition, sortOn)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)

import TrackKat.Branch
import TrackKat.Calibration
import TrackKat.Frame (Blob (..), Stripe, stripesToBlobs)
import TrackKat.Lights
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
    | -- | The pose tracked, or the one recentered on, was found to be the
      -- mirror image of the head's (see "TrackKat.Branch") and was
      -- switched for the head's.
      Corrected
    | -- | A bright light that stays put, at this place in the image, is
      -- ignored from now on (see "TrackKat.Lights").
      LightIgnored (Double, Double)
    | -- | The light ignored at this place hasn't been seen for a while,
      -- and isn't ignored any more.
      LightForgotten (Double, Double)
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
    | -- | The clip couldn't be made out.
      Lost
        { outBlobs :: Int
        -- ^ blobs there were, besides those masked
        , outMasked :: Int
        -- ^ blobs ignored as part of a still light
        }
    deriving (Show)

-- | How the reflectors appeared in a tracked frame.
data Seen = Seen
    { seenMissing :: Int
    -- ^ reflectors out of sight, held where recent frames had them
    , seenMerged :: Int
    -- ^ reflectors sharing a blob with another
    , seenStrays :: Int
    -- ^ blobs that weren't reflectors
    , seenMasked :: Int
    -- ^ blobs ignored as part of a still light
    }
    deriving (Eq, Show)

-- | A few words to end a status line with on anything unusual about how
-- the reflectors appeared, like "   2 of 3 reflectors, +1 stray"; empty if
-- they were three blobs and nothing else.
describeSeen :: Seen -> String
describeSeen (Seen missing merged strays masked) = case notes of
    [] -> ""
    _ -> "   " ++ intercalate ", " notes
  where
    notes =
        [show (3 - missing) ++ " of 3 reflectors" | missing > 0]
            ++ ["merged" | merged > 0]
            ++ ["+" ++ show strays ++ (if strays == 1 then " stray" else " strays") | strays > 0]
            ++ [describeMasked masked | masked > 0]

-- | Blobs ignored as part of a still light, as notes on the status say.
describeMasked :: Int -> String
describeMasked n = "+" ++ show n ++ " masked"

-- | Lines for the log on a still light being ignored, and forgotten, at a
-- place in the image.
describeLightIgnored, describeLightForgotten :: (Double, Double) -> String
describeLightIgnored p = "Ignoring a bright light at " ++ describePlace p ++ "."
describeLightForgotten p = "No longer ignoring the light at " ++ describePlace p ++ ": it has been gone a while."

describePlace :: (Double, Double) -> String
describePlace (u, v) = "(" ++ show (round u :: Int) ++ ", " ++ show (round v :: Int) ++ ")"

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
    , recentSince :: !Double
    -- ^ since when the clip has been tracked in every frame
    }

data Tracker = Tracker
    { trIntrinsics :: Intrinsics
    , trModel :: Model
    , trRecent :: Maybe Recent
    -- ^ predicts the next frame, and is the solver's starting point
    , trSizes :: Three Size
    , trReference :: Maybe Pose
    , trLearner :: Learner
    , trBranch :: Branch
    -- ^ whether the poses tracked and recentered on are the head's
    , trSweep :: Maybe Sweep
    , trPending :: [Request]
    -- ^ oldest first
    , trMasking :: Masking
    , trLights :: !Lights
    , trBlobs :: ![(Blob, BlobRole)]
    -- ^ the last frame's, all evaluated, so as not to hold on to the frames
    -- before
    }

-- | A tracker that starts from the given calibration and recenters on the
-- first frame it tracks.
newTracker :: Intrinsics -> Model -> Masking -> Calibration -> Tracker
newTracker k m masking c = Tracker k m Nothing (pure noSize) Nothing (newLearner m c) (newBranch m) Nothing [Recenter] masking noLights []

-- | What the tracker has learnt so far.
trackerCalibration :: Tracker -> Calibration
trackerCalibration = learnerCalibration . trLearner

-- | The clip's pose at the last recenter.
trackerCentre :: Tracker -> Maybe Pose
trackerCentre = trReference

-- | The clip's pose in the camera's axes, as last tracked.
trackerPose :: Tracker -> Maybe Pose
trackerPose = fmap recentPose . trRecent

-- | The last frame's blobs, heaviest first, and what each was taken for.
trackerBlobs :: Tracker -> [(Blob, BlobRole)]
trackerBlobs = trBlobs

-- | What a blob was taken for.
data BlobRole
    = -- | one of the clip's reflectors, or part of one
      ReflectorBlob
    | -- | something else, ignored
      StrayBlob
    | -- | part of a still light, masked
      MaskedBlob
    deriving (Eq, Show)

-- | Learn and mask still lights, or not, from the next frame; turned off,
-- what was learnt is forgotten.
setMasking :: Masking -> Tracker -> Tracker
setMasking masking t
    | masking == trMasking t = t
    | otherwise = t{trMasking = masking, trLights = noLights}

request :: Request -> Tracker -> Tracker
request r t = t{trPending = trPending t ++ [r]}

-- | Track a differently shaped clip from the next frame, recentering then,
-- since the clip's axes move with its shape.
setModel :: Model -> Tracker -> Tracker
setModel m t = request Recenter t{trModel = m, trRecent = Nothing, trLearner = changeClip m (trLearner t), trBranch = newBranch m}

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

-- | For how long, in seconds, the clip must have been tracked in every
-- frame for still lights to be learnt clear of it.
confirmSeconds :: Double
confirmSeconds = 0.5

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

-- | The farthest a head turns (degrees) and moves (mm, at its pivot) from
-- where it was recentered. Beyond that the blobs aren't the clip, but
-- something like a phone's light.
maxReachTurn, maxReachMove :: Double
maxReachTurn = 120
maxReachMove = 400

-- | The nearest and farthest, in mm, the clip can be from the camera.
nearest, farthest :: Double
nearest = 250
farthest = 2500

-- | The worst RMS reprojection error, in pixels, of a pose matched from a
-- prediction.
maxFit :: Double
maxFit = 2

-- | Seconds over which a reflector's usual size is forgotten.
sizeMemory :: Double
sizeMemory = 10

-- | How the clip was made out.
data Finding = Followed | FoundAfresh

-- | What a frame showed, when the clip was made out.
data Sighting
    = Sighting
        Finding
        Pose
        -- | RMS reprojection error, pixels
        Double
        (Three (Maybe Sight))
        -- | blobs that weren't reflectors
        Int
        -- | blobs ignored as part of a still light
        Int

-- | Handle one frame's stripes, seen at the given time in seconds. Pending
-- requests wait for a frame where the clip is tracked.
step :: Double -> [Stripe] -> Tracker -> (Tracker, Outcome, [Event])
step now stripes t =
    case sighting of
        Nothing ->
            let (learntLights, learntNews) = learnt []
                -- Lights just learnt that the clip would be found in are
                -- the clip, held still: they are forgotten unannounced.
                newly = [p | LightIgnored p <- learntNews]
                learntMask = activeMask [] learntLights
                (lights, unlearnt)
                    | null newly = (learntLights, [])
                    | otherwise = case findAfresh (learntMask, partition (inMask learntMask) blobs) of
                        Just (_, restored) -> unlearn newly (map at restored) learntLights
                        Nothing -> (learntLights, [])
                news = [e | e <- learntNews, not (isUnlearnt e)]
                isUnlearnt (LightIgnored p) = p `elem` unlearnt
                isUnlearnt _ = False
                -- Something tracked a moment ago in a light just learnt
                -- was the light, and isn't followed.
                untracked = case trRecent t of
                    Just r
                        | not (holding [p | LightIgnored p <- news] (toList (project k (recentPose r) <$> modelPoints m)) lights) ->
                            Just r{recentSince = now}
                    _ -> Nothing
             in ( t{trLearner = lostSight (trLearner t), trLights = lights, trRecent = untracked, trBlobs = roles []}
                , Lost (length visible) (length masked)
                , news
                )
        Just (Sighting finding seenPose fit sights unused hidden) ->
            let allSeen = all isJust sights
                since = case finding of
                    Followed -> maybe now recentSince (trRecent t)
                    FoundAfresh -> now
                -- Lights are learnt clear of the clip once it has been
                -- tracked a while: something tracked for a moment can be
                -- lights that look like the clip. If it was, and the light
                -- is learnt, it is no longer followed.
                confirmed = now - since >= confirmSeconds
                (lights, news) = learnt (if confirmed then reflectorDiscs seenPose sights else [])
                wasLight = not confirmed && holding [p | LightIgnored p <- news] (toList (project k seenPose <$> modelPoints m)) lights
                -- See Note [Which pose is the head's] in TrackKat.Branch.
                others = if allSeen then Just (alternatives k m seenPose) else Nothing
                watched = watch now pivot seenPose others $ case finding of
                    Followed -> trBranch t
                    FoundAfresh -> foundAfresh (trBranch t)
                (pose, fix) = case (correction watched, followedMirror watched) of
                    (c, Just other) | c `elem` [MirrorTracked, MirrorBoth] -> (other, c)
                    (MirrorReference, _) -> (seenPose, MirrorReference)
                    _ -> (seenPose, MirrorNeither)
                reference
                    | fix `elem` [MirrorReference, MirrorBoth] = (\old -> fromMaybe old (mirrorPose k m old)) <$> trReference t
                    | otherwise = trReference t
                correcting = fix /= MirrorNeither
                recent = Recent pose now (if allSeen then now else maybe now recentAllSeen (trRecent t)) since
                sizes = resize (metres pose) <$> trSizes t <*> sights
                -- Learning in progress from before a correction was of
                -- the mirror image, and so was a requested calibration's
                -- turning.
                restarted =
                    t
                        { trRecent = Just recent
                        , trSizes = sizes
                        , trPending = []
                        , trReference = reference
                        , trBranch = corrected fix seenPose watched
                        , trLearner = if correcting then lostSight (trLearner t) else trLearner t
                        , trSweep = if correcting then fmap (\(Sweep deadline _) -> Sweep deadline []) (trSweep t) else trSweep t
                        , trLights = lights
                        , trBlobs = roles [s | Just s <- toList sights]
                        }
                (t1, requested) = foldl (handle pose) (restarted, []) (trPending t)
                ref = maybe pose id (trReference t1)
                -- Only a pose surely the head's is learnt from.
                (learner, refinements)
                    | settled (trBranch t1) = learn now pose (trLearner t1)
                    | otherwise = (lostSight (trLearner t1), [])
                (t2, calibrated) = calibrate pose ref t1{trLearner = learner}
                applied = trLearner t2
                seen =
                    Seen
                        (length (filter isNothing (toList sights)))
                        (length [() | Just s <- toList sights, sightKind s == Merged])
                        unused
                        hidden
             in ( if wasLight then t2{trRecent = Nothing} else t2
                , Tracked (relativePose (appliedLevel applied) (appliedPivot applied) ref pose) (norm (poseT pose)) fit seen
                , [Corrected | correcting]
                    ++ reverse requested
                    ++ [Refined r (learnerCalibration learner) | r <- refinements]
                    ++ calibrated
                    ++ news
                )
  where
    k = trIntrinsics t
    m = trModel t
    blobs = findBlobs stripes
    -- Blobs in a still light's area are ignored, but for any where the
    -- clip is predicted to be.
    following = case trRecent t of
        Just r | now - recentTime r <= predictionSeconds -> Just r
        _ -> Nothing
    mask = case trMasking t of
        NoMasking -> noMask
        MaskLights -> activeMask (maybe [] (\r -> toList (project k (recentPose r) <$> modelPoints m)) following) (trLights t)
    (masked, visible) = partition (inMask mask) blobs
    -- Learning still lights, given discs around where the reflectors are:
    -- the lights learnt, and the events saying what changed.
    learnt discs = case trMasking t of
        NoMasking -> (trLights t, [])
        MaskLights -> map toEvent <$> learnLights now discs blobs (trLights t)
    -- Each blob's role, given where the reflectors were seen: a blob near
    -- one, for its size, is one, or a piece of one.
    roles seen = foldr (\(_, r) rest -> r `seq` rest) assigned assigned
      where
        assigned = [(b, role b) | b <- blobs]
        role b
            | any (\s -> distance2 (sightAt s) (at b) <= reflectorReach b ^ (2 :: Int)) seen = ReflectorBlob
            | inMask mask b = MaskedBlob
            | otherwise = StrayBlob
        reflectorReach b = 3 + 2 * sqrt (fromIntegral (blobPoints b) / pi)
        distance2 (u, v) (x, y) = (u - x) ^ (2 :: Int) + (v - y) ^ (2 :: Int)
    toEvent (Learnt p) = LightIgnored p
    toEvent (Forgotten p) = LightForgotten p
    -- Where each reflector is, or would be, in the image at a pose, out to
    -- a margin that takes in the pieces of the biggest seen, such as a
    -- reflector broken in two.
    reflectorDiscs pose sights =
        let biggestSeen = maximum (0 : [sightPixels s | Just s <- toList sights])
         in [(project k pose p, 15 + 2 * sqrt (biggestSeen / pi)) | p <- toList (modelPoints m)]
    pivot = appliedPivot (trLearner t)
    metres pose = norm (poseT pose) / 1000
    sighting = maybe (fst <$> findAfresh (mask, (masked, visible))) follow following
    -- With nothing to predict from, three big blobs by their layout,
    -- solved from where the clip was last seen if anywhere, given a mask
    -- and the blobs it masks and doesn't; and the masked blobs taken as
    -- reflectors. With fewer than three big blobs unmasked, the heaviest
    -- in each of a few masked areas are tried too, those that make the
    -- three most alike in size first: a clip held still can be learnt as
    -- lights (see Note [Still lights] in TrackKat.Lights).
    findAfresh (areas, (hidden, shown)) = listToMaybe (mapMaybe attempt choices)
      where
        -- Big enough to be a reflector: of a fair size, and not tiny
        -- beside the biggest blob, as other reflections often are.
        big b = blobPoints b >= minBlobPixels && blobPoints b * 25 >= biggest * 3
        biggest = maximum (0 : map blobPoints shown)
        bigShown = filter big shown
        choices = case compare (length bigShown) 3 of
            EQ -> [[]]
            LT -> sortOn alike (choose (3 - length bigShown) (take 4 (filter big (heaviestIn areas hidden))))
            GT -> []
        alike extra = let ps = map blobPoints (bigShown ++ extra) in fromIntegral (maximum ps) / fromIntegral (max 1 (minimum ps)) :: Double
        whole b = Just (Sight (at b) (fromIntegral (blobPoints b)) Whole)
        attempt extra = do
            (l, r, top) <- identify at (bigShown ++ extra)
            (pose, fit) <- solvePose k m (recentPose <$> trRecent t) (at l, at r, at top)
            if plausible pose
                then Just (Sighting FoundAfresh pose fit (Three (whole l) (whole r) (whole top)) (length shown + length extra - 3) (length hidden - length extra), extra)
                else Nothing
    -- The best match to the prediction, unless its pose is an impossible
    -- jump; then, of the next few that see as many reflectors, the one whose
    -- pose is nearest. (Fewer reflectors can always be fitted by a pose
    -- near the last, so they would hide a real jump.)
    follow r =
        let dt = now - recentTime r
            twoAllowed = now - recentAllSeen r <= twoReflectorSeconds
            candidates = [c | c <- matches k m (recentPose r) (trSizes t) visible, twoAllowed || all isJust (matchSights c)]
            attempt c = do
                (pose, fit) <- solveNear k pivot (recentPose r) (observations (metres (recentPose r)) c)
                let (turn, move) = poseChange pivot (recentPose r) pose
                    jump = max (turn / maxTurn dt) (move / maxMove dt)
                if fit <= maxFit && jump <= 1 && plausible pose
                    then Just (jump, Sighting Followed pose fit (matchSights c) (matchUnused c) (length masked))
                    else Nothing
         in case candidates of
                [] -> Nothing
                c : cs -> case attempt c of
                    Just (_, s) -> Just s
                    Nothing ->
                        let alike = [a | a <- cs, seenCount a == seenCount c]
                         in fmap snd (listToMaybe (sortOn fst (mapMaybe attempt (take 5 alike))))
    -- Somewhere a head can be: at a sane distance from the camera, and not
    -- too far from where it was recentered.
    plausible pose =
        let distance = norm (poseT pose)
            reach ref = let (turn, move) = poseChange pivot ref pose in turn <= maxReachTurn && move <= maxReachMove
         in distance >= nearest && distance <= farthest && maybe True reach (trReference t)
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
    at b = (blobX b, blobY b)
    handle pose (acc, evs) Recenter = (acc{trReference = Just pose, trBranch = recentered (trBranch acc)}, Recentered : evs)
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

-- | The ways to choose so many of a list, in order.
choose :: Int -> [a] -> [[a]]
choose 0 _ = [[]]
choose _ [] = []
choose n (x : xs) = map (x :) (choose (n - 1) xs) ++ choose n xs
