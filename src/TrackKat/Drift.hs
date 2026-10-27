-- | Letting the centre follow where the head rests, so that shifting in the
-- chair doesn't call for a recenter.
--
-- The head is watched a second at a time. A second in which it kept still
-- near the centre is a moment of rest. Rests at about the same place add
-- up, and once they have added up for long enough the centre moves toward
-- that place, slowly enough not to be seen. Translation, as from sitting
-- differently, follows after less rest and faster than rotation, as from a
-- turned chair or a slouch. Moving, and looking or leaning away, however
-- long, count for nothing; rest somewhere else near the centre, as during
-- a lean held still, starts adding up afresh, so a lean held for a while
-- doesn't move the centre either.
module TrackKat.Drift (
    Drift (..),
    Drifter,
    newDrifter,
    driftOffset,
    driftCentre,
    interrupted,
    describeDrift,
) where

import Text.Printf (printf)

import TrackKat.Pose (HeadPose (..))

-- | Whether the centre follows where the head rests.
data Drift = Drift | NoDrift
    deriving (Eq, Show)

-- | The centre as it has drifted, and the rest it is drifting toward.
data Drifter = Drifter
    { dOffset :: !HeadPose
    -- ^ the centre, relative to the one recentered on
    , dRest :: !(Maybe Rest)
    , dWindow :: !(Maybe Window)
    , dLast :: !(Maybe Double)
    -- ^ time of the last pose seen
    , dTold :: !HeadPose
    -- ^ the centre as last reported
    }

-- | Where the head has been resting, relative to the centre recentered on,
-- and for how many seconds in all.
data Rest = Rest !HeadPose !Double

-- | A second of poses: its start time, and per axis the sum, least and
-- greatest of the poses.
data Window = Window !Double !Int ![Double] ![Double] ![Double]

-- | The centre as recentered, with no rest seen.
newDrifter :: Drifter
newDrifter = Drifter zero Nothing Nothing Nothing zero

-- | How far the centre has moved since the recenter.
driftOffset :: Drifter -> HeadPose
driftOffset = dOffset

zero :: HeadPose
zero = HeadPose 0 0 0 0 0 0

-- | Seconds in a window.
windowSeconds :: Double
windowSeconds = 1

-- | How far a still head may move during a window, from its least to its
-- greatest on each axis, in degrees and mm: breathing and settling, but
-- not looking or leaning.
stillDegrees, stillMm :: Double
stillDegrees = 1.5
stillMm = 6

-- | How far from the centre a rest may be, in degrees on each axis and mm
-- in all. The rotations, times the curves, already look well away from
-- centre: 4 degrees of yaw is 24 in the game by default. A held look
-- further away never moves the centre.
nearDegrees, nearMm :: Double
nearDegrees = 4
nearMm = 40

-- | How far apart two rests may be and still count as the same place.
sameDegrees, sameMm :: Double
sameDegrees = 2
sameMm = 10

-- | Seconds over which the place of a rest settles, as more of it is seen.
restBlend :: Double
restBlend = 10

-- | Seconds of rest at a place before the centre moves toward it: longer
-- than a look or lean is usually held.
translationAfter, rotationAfter :: Double
translationAfter = 20
rotationAfter = 60

-- | Seconds in which the centre makes up most of the way to a rest (the
-- time constant), and the most it moves a second, in mm and degrees: with
-- the default curves, no more than 3 mm and 0.2 degrees a second in the
-- game.
translationTime, rotationTime, translationRate, rotationRate :: Double
translationTime = 30
rotationTime = 60
translationRate = 0.75
rotationRate = 0.03

-- | Seconds between poses beyond which the centre doesn't move further, so
-- that it never jumps.
longestStep :: Double
longestStep = 0.1

-- | Moves of the centre worth mentioning, in degrees and mm.
newsDegrees, newsMm :: Double
newsDegrees = 1
newsMm = 10

-- | Take the pose seen at the given time in seconds, relative to the centre
-- recentered on, and give it relative to the drifted centre; also the new
-- centre, if it has moved enough since last mentioned to mention. With
-- 'NoDrift' the centre stays where it is, as while paused.
driftCentre :: Drift -> Double -> HeadPose -> Drifter -> (HeadPose, Drifter, Maybe HeadPose)
driftCentre NoDrift _ hp d = (hp `minus` dOffset d, interrupted d, Nothing)
driftCentre Drift now hp d =
    ( hp `minus` offset
    , d{dOffset = offset, dRest = rest, dWindow = window, dLast = Just now, dTold = if news then offset else dTold d}
    , if news then Just offset else Nothing
    )
  where
    dt = maybe 0 (\t -> max 0 (min longestStep (now - t))) (dLast d)
    current = maybe (startWindow now hp) (addPose hp) (dWindow d)
    (rest, window)
        | now - start current >= windowSeconds = (ended current, Nothing)
        | otherwise = (dRest d, Just current)
    ended w
        | still w && near (mean w `minus` dOffset d) = Just $ case dRest d of
            Just (Rest at seconds)
                | same (mean w) at ->
                    let k = elapsed / min restBlend (seconds + elapsed)
                     in Rest (fromAxes (zipWith (\a m -> a + k * (m - a)) (axes at) (axes (mean w)))) (seconds + elapsed)
            _ -> Rest (mean w) elapsed
        | otherwise = dRest d
      where
        elapsed = now - start w
    -- The centre moves only while the head rests where the rest is.
    offset = case rest of
        Just (Rest at seconds)
            | still current && same hp at ->
                let follow after time rate part
                        | seconds < after = part (dOffset d)
                        | otherwise = towards (min (rate * dt) (gap * dt / time)) (part (dOffset d)) (part at)
                      where
                        gap = magnitude (zipWith (-) (part at) (part (dOffset d)))
                 in fromAxes
                        ( follow rotationAfter rotationTime rotationRate rotations
                            ++ follow translationAfter translationTime translationRate translations
                        )
        _ -> dOffset d
    news =
        magnitude (zipWith (-) (rotations offset) (rotations (dTold d))) >= newsDegrees
            || magnitude (zipWith (-) (translations offset) (translations (dTold d))) >= newsMm

-- | The clip went out of sight, or the head is not to be watched for a
-- while: the second in progress ends unseen.
interrupted :: Drifter -> Drifter
interrupted d = d{dWindow = Nothing, dLast = Nothing}

-- | A line for the console about the centre.
describeDrift :: HeadPose -> String
describeDrift o
    | all ((< 0.05) . abs) (axes o) = "Centre: where you recentered."
    | otherwise =
        printf
            "Centre has followed you since recentering: yaw %+.1f  pitch %+.1f  roll %+.1f   x %+.0f  y %+.0f  z %+.0f mm"
            (yaw o)
            (pitch o)
            (roll o)
            (tx o)
            (ty o)
            (tz o)

startWindow :: Double -> HeadPose -> Window
startWindow now hp = Window now 1 (axes hp) (axes hp) (axes hp)

addPose :: HeadPose -> Window -> Window
addPose hp (Window t n s lo hi) = Window t (n + 1) (zipWith (+) s v) (zipWith min lo v) (zipWith max hi v)
  where
    v = axes hp

start :: Window -> Double
start (Window t _ _ _ _) = t

mean :: Window -> HeadPose
mean (Window _ n s _ _) = fromAxes (map (/ fromIntegral n) s)

still :: Window -> Bool
still (Window _ _ _ lo hi) = and (zipWith3 (\limit l h -> h - l <= limit) [stillDegrees, stillDegrees, stillDegrees, stillMm, stillMm, stillMm] lo hi)

near :: HeadPose -> Bool
near p = all ((<= nearDegrees) . abs) (rotations p) && magnitude (translations p) <= nearMm

same :: HeadPose -> HeadPose -> Bool
same a b = all ((<= sameDegrees) . abs) (rotations g) && magnitude (translations g) <= sameMm
  where
    g = a `minus` b

-- | A pose less an offset, axis by axis. Yaw is the outermost of the
-- angles, so subtracting it undoes a turned chair exactly; subtracting a few
-- degrees of pitch or roll is close enough.
minus :: HeadPose -> HeadPose -> HeadPose
minus a b = fromAxes (zipWith (-) (axes a) (axes b))

axes :: HeadPose -> [Double]
axes p = [yaw p, pitch p, roll p, tx p, ty p, tz p]

fromAxes :: [Double] -> HeadPose
fromAxes [y, p, r, x, yy, z] = HeadPose y p r x yy z
fromAxes _ = zero

rotations, translations :: HeadPose -> [Double]
rotations = take 3 . axes
translations = drop 3 . axes

-- | Move a point toward another, by at most the given distance.
towards :: Double -> [Double] -> [Double] -> [Double]
towards limit from to
    | d <= limit = to
    | otherwise = zipWith (\f t -> f + (t - f) * limit / d) from to
  where
    d = magnitude (zipWith (-) to from)

magnitude :: [Double] -> Double
magnitude = sqrt . sum . map (^ (2 :: Int))
