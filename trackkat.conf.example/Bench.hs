import Control.Exception (evaluate)
import Data.List (foldl', sortOn)
import GHC.Clock (getMonotonicTime)
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
k = defaultIntrinsics

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
neckOffset = V3 0 130 90
place = V3 0 180 845
headAt worn y p r = let h = rotYXZ y p r in Pose (tilt `mulMM` h `mulMM` rotYXZ 0 worn 0) (mulMV tilt (place ^+^ neckOffset ^-^ mulMV h neckOffset))

discsAt fades pose =
    [ (project k pose p, f * r * 1000 / z)
    | (p, r, f) <- zip3 [mLeft bent, mRight bent, mTop bent] [8.8, 8.8, 7.6] fades
    , let V3 _ _ z = mulMV (poseR pose) p ^+^ poseT pose
    , f > 0
    ]

bench name frames = do
    fs <- evaluate (foldr (\f acc -> length f `seq` (f : acc)) [] frames)
    _ <- evaluate (sum (map (sum . map stSum) fs))
    t0 <- getMonotonicTime
    let go (tr, n) (i, ss) = let (tr', o, _) = step (fromIntegral i / 120) ss tr in case o of Lost _ -> (tr', n + 1); Tracked{outFit = f} -> f `seq` (tr', n)
        (_, lost) = foldl' go (newTracker k bent defaultCalibration, 0 :: Int) (zip [0 :: Int ..] fs)
    _ <- evaluate lost
    t1 <- getMonotonicTime
    printf "%s: %d frames, %.1f us per frame, lost %d\n" (name :: String) (length fs) ((t1 - t0) / fromIntegral (length fs) * 1e6) lost

benchMatch frames = do
    fs <- evaluate (foldr (\f acc -> length f `seq` (f : acc)) [] frames)
    let bss = map findBlobs fs
        n = fromIntegral (length fs)
    _ <- evaluate (sum (map (sum . map blobPoints) bss))
    let pose = headAt (-23) 0 0 0
        seen i = [project k (headAt (-23) (fromIntegral i / 1000) 0 0) p | p <- [mLeft bent, mRight bent, mTop bent]]
    t0 <- getMonotonicTime
    total <- evaluate (sum [matchCost (head (matches k bent pose (pure (Size 250 250)) bs)) | bs <- bss])
    t1 <- getMonotonicTime
    total2 <- evaluate (sum [maybe 0 snd (solveNear k (V3 0 100 100) pose (zipWith (\p q -> Observation p 1 q) [mLeft bent, mRight bent, mTop bent] (seen i))) | (i, _) <- zip [0 :: Int ..] bss])
    t2 <- getMonotonicTime
    total3 <- evaluate (sum [maybe 0 snd (solvePose k bent (Just pose) (let [a, b, c] = seen i in (a, b, c))) | (i, _) <- zip [0 :: Int ..] bss])
    t3 <- getMonotonicTime
    total4 <- evaluate (sum [length (findBlobs f) | f <- fs])
    t4 <- getMonotonicTime
    printf "matching %.1f us, solveNear %.1f us, solvePose %.1f us, findBlobs %.1f us (%f %f %f %d)\n" ((t1 - t0) / n * 1e6) ((t2 - t1) / n * 1e6) ((t3 - t2) / n * 1e6) ((t4 - t3) / n * 1e6) total total2 total3 total4

main = do
    let t i = fromIntegral i / 120 :: Double
    benchMatch [render (discsAt [1, 1, 1] (headAt (-23) (40 * sin (2 * pi * t i / 7)) 0 0)) | i <- [0 .. 2400 :: Int]]
    bench "sweep" [render (discsAt [1, 1, 1] (headAt (-23) (40 * sin (2 * pi * t i / 7)) (25 * sin (2 * pi * t i / 5.3)) (20 * sin (2 * pi * t i / 4.1)))) | i <- [0 .. 2400 :: Int]]
    bench "merged" [render (discsAt [1, 1, 1] (headAt (-23) (60 + 25 * t i / 4) 0 0)) | i <- [0 .. 480 :: Int]]
    bench "stray" [render (((340, 288), 3) : discsAt [1, 1, 1] (headAt (-23) (8 * sin (2 * pi * t i / 2)) 0 0)) | i <- [0 .. 600 :: Int]]
    bench "two" [render (discsAt [1, 1, 0] (headAt (-23) (8 * sin (2 * pi * t i / 2)) 0 0)) | i <- [0 .. 110 :: Int]]
