-- | Which reflectors can be seen (see "TrackKat.Visibility"): tape facing
-- away, and reflectors behind the head.
module Test.Old.Visibility (simulations) where

import Data.List (mapAccumL)
import Test.Tasty (TestTree)

import Test.Check
import Test.Old.Simulation
import TrackKat.Calibration
import TrackKat.Config
import TrackKat.Linear
import TrackKat.Old.Tracker
import TrackKat.Pose
import TrackKat.Synth
import TrackKat.Visibility
import TrackKat.World (clockAt)

-- | The configured clip, its reflectors tape on tabs.
tabs :: Model
tabs = clipModel (clip defaultConfig)

-- | Where the neck pivot is from a clip standing off the side of the head.
side :: V3
side = V3 100 130 60

-- | Frames of a clip on the side of the head turning far, the head hiding
-- it, tracked when it is in sight.
simulations :: TestTree
simulations = group "Visibility" $ do
    -- Frames of it turning 110 degrees each way, the pivot learnt: lost
    -- only while out of sight, and followed whenever in sight.
    let temple = Clip tabs AsTape (Wear (rotYXZ 0 (-23) 0) side)
        truthHead = HeadShape (V3 0 (-90) (-15)) (V3 78 110 98)
        V3 px py pz = pivotInClip (clipWear temple)
        learnt = either error id (parseCalibration standardMounting ("up = 0 -0.9781476 -0.2079117\nturning = 900\npivot = " ++ unwords (map show [px, py, pz]) ++ "\npivot windows = 100\npivot information = 10 0 0 10 0 10\n"))
        turning t = 110 * sin (2 * pi * t / 12)
        path = [(t, headPose userCamera (clipWear temple) (turning t) (5 * sin (2 * pi * t / 5)) 0) | t <- framesAt 60 24]
        shown pose = length (clipDiscsOnHead defaultIntrinsics temple truthHead allLit pose)
        frames = [(t, pose, renderDiscs (jitter 0.3 i (clipDiscsOnHead defaultIntrinsics temple truthHead allLit pose))) | (i, (t, pose)) <- zip [0 ..] path]
        rows = map fst (snd (mapAccumL (\tr (t, truth, stripes) -> let (tr', o, evs) = step (clockAt t) stripes tr in (tr', ((t, truth, o, trackerPose tr'), evs))) (newTracker defaultIntrinsics tabs MaskLights learnt) frames))
        hiddenFrames = length [() | (_, pose) <- path, shown pose < 3]
        lostInSight = [t | ((t, truth, Lost{}, _), _) <- zip rows path, shown truth == 3]
    checkShowing "turning far with the clip on the side of the head, hidden in" (show hiddenFrames ++ " frames, lost in sight at " ++ show (take 3 lostInSight) ++ ", off by " ++ show (worstError rows)) $
        hiddenFrames > 100 && null lostInSight && fst (worstError rows) < 5
