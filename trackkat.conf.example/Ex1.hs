import TrackKat.Pose
import TrackKat.Linear
import Text.Printf
deg = (* (pi/180))
rotYXZ y p r = rodrigues (V3 0 (deg y) 0) `mulMM` rodrigues (V3 (deg p) 0 0) `mulMM` rodrigues (V3 0 0 (deg r))
Just clip = triangle 45 70 116 116
main = do
  let tilt = rodrigues (V3 (deg 12) 0 0)
      seen y p r = Pose (tilt `mulMM` rotYXZ y p r) (mulMV tilt (V3 0 180 840))
  mapM_ (\(y,p) -> do
     let pose = seen y p 0
         [l,r,t] = map (project defaultIntrinsics pose) [mLeft clip, mRight clip, mTop clip]
         d a b = sqrt ((fst a-fst b)^2 + (snd a - snd b)^2)
     printf "y %5.1f p %5.1f  L %s R %s T %s  dLT %.1f dRT %.1f  ok %s\n" y p (sh l) (sh r) (sh t) (d l t) (d r t) (show (identify [l,r,t] == Just (l,r,t))))
    [(y,p) | y <- [-4, 13, 25, 30], p <- [-60,-50..40]]
sh (a,b) = printf "(%.1f,%.1f)" a b :: String
