import Data.List (sortOn, mapAccumL)
import System.Environment
import Text.Printf
import TrackKat.Calibration
import TrackKat.Frame
import TrackKat.Linear
import TrackKat.Pose
import TrackKat.Tracker
import TrackKat.Match

deg = (* (pi / 180))
rotYXZ y p r = rodrigues (V3 0 (deg y) 0) `mulMM` rodrigues (V3 (deg p) 0 0) `mulMM` rodrigues (V3 0 0 (deg r))
Just bent = triangle 45 70 110 110
Just straight = triangle 45 70 116 116
k = defaultIntrinsics

-- Discs (centre, radius) to stripes, merging runs that touch on a line.
render :: [((Double, Double), Double)] -> [Stripe]
render discs = concatMap line [0 .. frameHeight - 1]
  where
    line y = map (stripe y) (joinRuns (sortOn fst (concatMap (run y) discs)))
    run y ((u, v), r) =
        let dy = fromIntegral y - v
            h2 = r * r - dy * dy
         in if h2 < 0 then [] else let hw = sqrt h2; a = max 0 (ceiling (u - hw)); b = min (frameWidth - 1) (floor (u + hw)) in [(a, b) | a <= b]
    joinRuns ((a, b) : (c, d) : rest) | c <= b + 1 = joinRuns ((a, max b d) : rest)
    joinRuns (x : rest) = x : joinRuns rest
    joinRuns [] = []
    stripe y (a, b) = let n = b - a + 1 in Stripe y a n (100 * n * (n - 1) `div` 2) (100 * n)

tilt = rodrigues (V3 (deg 12) 0 0)
place = V3 0 180 845
-- The head turns about its neck, 130 mm below and 90 behind the clip; the clip sits pitched by worn.
neckOffset = V3 0 130 90
headAt worn y p r = let h = rotYXZ y p r in Pose (tilt `mulMM` h `mulMM` rotYXZ 0 worn 0) (mulMV tilt (place ^+^ neckOffset ^-^ mulMV h neckOffset))

-- reflector radii at 1 m in pixels: lower ~350px at 830mm, top ~265
radii = [8.8, 8.8, 7.6]

discsAt :: Model -> [Double] -> Pose -> [((Double, Double), Double)]
discsAt m fades pose =
    [ (project k pose p, f * r * 1 / (z / 1000))
    | (p, r, f) <- zip3 [mLeft m, mRight m, mTop m] radii fades
    , let V3 _ _ z = mulMV (poseR pose) p ^+^ poseT pose
    , f > 0
    ]

errorOf :: Pose -> Pose -> (Double, Double)
errorOf a b = (norm (rotationVector (poseR a `mulMM` transpose (poseR b))) * 180 / pi, norm (poseT a ^-^ poseT b))

runPath :: Model -> [(Double, Pose, [Stripe])] -> [(Double, Pose, Outcome, Maybe Pose)]
runPath m frames = snd (mapAccumL go (newTracker k m defaultCalibration) frames)
  where
    go t (time, truth, ss) = let (t', o, _) = step time ss t in (t', (time, truth, o, trackerPose t'))

report :: [(Double, Pose, Outcome, Maybe Pose)] -> IO ()
report rows = mapM_ line rows
  where
    line (time, truth, o, mp) = case o of
        Lost n -> printf "%6.3f LOST %d\n" time n
        Tracked hp d fit seen -> do
            let (ea, et) = maybe (0, 0) (errorOf truth) mp
            printf "%6.3f y %6.1f p %6.1f r %6.1f d %5.0f fit %.2f err %.2f deg %.1f mm %s\n" time (yaw hp) (pitch hp) (roll hp) d fit ea et (describeSeen seen)

summarise :: [(Double, Pose, Outcome, Maybe Pose)] -> IO ()
summarise rows = do
    let errs = [errorOf truth p | (_, truth, Tracked{}, Just p) <- rows]
        lost = length [() | (_, _, Lost _, _) <- rows]
    printf "frames %d lost %d max err %.2f deg %.1f mm\n" (length rows) lost (maximum (0 : map fst errs)) (maximum (0 : map snd errs))

every n rows = [r | r@(time, _, _, _) <- rows, round (time * 120) `mod` n == (0 :: Int)]

main = do
    [which] <- getArgs
    let t i = fromIntegral i / 120 :: Double
    case which of
        "swap" -> do
            let frames = [(t i, pose, render (discsAt straight [1, 1, 1] pose)) | i <- [0 .. 600 :: Int], let p = -6 * (1 - cos (2 * pi * t i / 1.5)); pose = headAt (-49) (-4) p 0]
                rows = runPath straight frames
            report (every 6 rows)
            summarise rows
            let bad = [time | (time, pose, _, _) <- rows, let pts = map (project k pose) [mLeft straight, mRight straight, mTop straight], identify id pts /= Just (pts !! 0, pts !! 1, pts !! 2)]
            print (length bad, take 5 bad)
        "merge" -> do
            let frames = [(t i, pose, render (discsAt bent [1, 1, 1] pose)) | i <- [0 .. 960 :: Int], let y = 90 * t i / 8; pose = headAt (-23) y 0 0]
                rows = runPath bent frames
            report (every 6 rows)
            summarise rows
        "rest" -> rest
        "probe" -> probe
        "jumpProbe" -> jumpProbe
        "rollProbe" -> rollProbe
        other -> scen other

rest = mapM_ (\w -> do
    let pose = Pose (tilt `mulMM` rotYXZ 0 w 0) (mulMV tilt place)
        [l, r, tp] = map (project k pose) [mLeft bent, mRight bent, mTop bent]
    printf "worn %5.1f L %s R %s T %s\n" w (show l) (show r) (show tp)) [-40, -35 .. 0]

-- remove the pixels of one image line near a point (a reflector broken in two)
cut :: (Double, Double) -> [Stripe] -> [Stripe]
cut (u, v) = filter (\s -> not (stLine s == round v && abs (fromIntegral (stStart s) - u) < 15))

scen :: String -> IO ()
scen name = do
    let t i = fromIntegral i / 120 :: Double
        frames = case name of
            -- top fades out over 0.5 s at 1 s, hidden 0.4 s, back by 2.3 s; head yaws throughout
            "fade" ->
                [ (t i, pose, render (discsAt bent [1, 1, f] pose))
                | i <- [0 .. 360 :: Int]
                , let pose = headAt (-23) (10 * sin (2 * pi * t i / 3)) 0 0
                      f = max 0 (min 1 (abs (t i - 1.7) / 0.5 - 0.4))
                ]
            -- top hidden for 2 s
            "hidden" ->
                [ (t i, pose, render (discsAt bent [1, 1, if t i > 1 && t i < 3 then 0 else 1] pose))
                | i <- [0 .. 480 :: Int]
                , let pose = headAt (-23) (5 * sin (2 * pi * t i / 3)) 0 0
                ]
            -- a fixed reflection, and the right reflector faded for a while
            "stray" ->
                [ (t i, pose, render (discsAt bent [1, if t i > 1 && t i < 1.6 then 0 else 1, 1] pose ++ [((340, 228 + 60), 3)]))
                | i <- [0 .. 300 :: Int]
                , let pose = headAt (-23) (8 * sin (2 * pi * t i / 2)) 0 0
                ]
            -- the left reflector broken in two, and a tiny piece by the top
            "pieces" ->
                [ (t i, pose, (if t i > 0.5 then cut (project k pose (mLeft bent)) else id) (render (discsAt bent [1, 1, 1] pose ++ [((tu + 4, tv + 9), 2.5)])))
                | i <- [0 .. 300 :: Int]
                , let pose = headAt (-23) (8 * sin (2 * pi * t i / 2)) 0 0
                      (tu, tv) = project k pose (mTop bent)
                ]
            -- rolling over to 75 degrees and back
            "roll" ->
                [ (t i, pose, render (discsAt bent [1, 1, 1] pose))
                | i <- [0 .. 480 :: Int]
                , let pose = headAt (-23) 0 0 (75 * sin (pi * t i / 4))
                ]
            -- lost for 1.5 s at far yaw, back with the right one faded
            "return" ->
                [ (t i, pose, render (discsAt bent (if t i > 2 && t i < 3.5 then [] else [1, 1, if t i < 3.5 then 1 else 0.5]) pose))
                | i <- [0 .. 480 :: Int]
                , let pose = headAt (-23) (min 45 (30 * t i)) 0 0
                ]
            "jump" ->
                [ (t i, pose, render (discsAt bent [1, 1, 1] pose))
                | i <- [0 .. 144 :: Int]
                , let pose = headAt (-23) (if t i < 0.5 then 0 else 40) 0 0
                ]
            "rollS" ->
                [ (t i, pose, render (discsAt straight [1, 1, 1] pose))
                | i <- [0 .. 480 :: Int]
                , let pose = headAt (-49) 0 0 (60 * sin (pi * t i / 4))
                ]
            "sweepB" -> sweep bent (-23)
            "sweepS" -> sweep straight (-49)
            _ -> []
        sweep m worn =
            [ (t i, pose, render (discsAt m [1, 1, 1] pose))
            | i <- [0 .. 20 * 120 :: Int]
            , let pose = headAt worn (40 * sin (2 * pi * t i / 7)) (25 * sin (2 * pi * t i / 5.3)) (20 * sin (2 * pi * t i / 4.1))
            ]
        model = if name `elem` ["sweepS", "rollS"] then straight else bent
        rows = runPath model frames
        steps = zipWith (\(_, _, _, a) (_, _, _, b) -> maybe 0 id (fst <$> (errorOf <$> a <*> b))) rows (drop 1 rows)
    printf "biggest step %.2f deg\n" (maximum steps)
    report (every 6 rows)
    summarise rows

probe = do
    let pose = headAt (-23) 45 0 0
        bs = stripesToBlobs 6 2500 (render (discsAt bent [1, 1, 0.45] pose))
    mapM_ print bs
    print (map (project k pose) [mLeft bent, mRight bent, mTop bent])
    let at b = (blobX b, blobY b)
    case identify at bs of
        Just (l, r, t) -> print (solvePose k bent Nothing (at l, at r, at t), identify at bs)
        Nothing -> putStrLn "no identify"
    let tr0 = newTracker k bent defaultCalibration
        (tr1, o1, _) = step 0 (render (discsAt bent [1, 1, 1] pose)) tr0
        (_, o2, _) = step 1.5 (render (discsAt bent [1, 1, 0.45] pose)) tr1
    print (o1, o2)

rollProbe = mapM_ (\r -> do
    let pose = headAt (-23) 0 0 r
        ps = map (project k pose) [mLeft bent, mRight bent, mTop bent]
    printf "roll %4.0f %s %s\n" r (show (map (\(a, b) -> (round a, round b)) ps)) (show (identify id ps == Just (ps !! 0, ps !! 1, ps !! 2)))) [-90, -75, -60, -45, 45, 60, 75, 90 :: Double]

jumpProbe = do
    let before = headAt (-23) 0 0 0
        after' = headAt (-23) 40 0 0
        bs = stripesToBlobs 6 2500 (render (discsAt bent [1, 1, 1] after'))
    print (map (project k before) [mLeft bent, mRight bent, mTop bent])
    print (map (project k after') [mLeft bent, mRight bent, mTop bent])
    mapM_ (\b -> print (blobX b, blobY b, blobPoints b)) bs
    mapM_ print (take 6 (matches k bent before (pure (Size 250 250)) bs))
