import Data.List (sortOn)
import Text.Printf
import TrackKat.Frame
import TrackKat.Match

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

main = mapM_ go [(r1, r2, d) | (r1, r2) <- [(10.6, 9.2), (10.6, 5)], d <- [0, 2 .. 20]]
  where
    go (r1, r2, d) = do
        let a = (300.3, 200.6)
            b = (300.3 + d * 0.8, 200.6 + d * 0.6)
            blobs = stripesToBlobs 1 5000 (render [(a, r1), (b, r2)])
        case blobs of
            [bl] -> do
                let (sa, sb) = splitMerged bl a b (pi * r1 * r1) (pi * r2 * r2)
                    dist (x, y) (u, v) = sqrt ((x - u) ^ 2 + (y - v) ^ 2)
                printf "r %.1f %.1f d %4.1f  px %4d  errA %.2f errB %.2f  sep %.2f\n" r1 r2 d (blobPoints bl) (dist (sightAt sa) a) (dist (sightAt sb) b) (dist (sightAt sa) (sightAt sb))
            _ -> printf "r %.1f %.1f d %4.1f  %d blobs\n" r1 r2 d (length blobs)
