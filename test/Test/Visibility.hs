-- | Which reflectors can be seen (see "TrackKat.Visibility"): tape facing
-- away, and reflectors behind the head.
module Test.Visibility (tests, simulations) where

import Data.List (mapAccumL)
import Test.Tasty (TestTree)

import Test.Check
import Test.Simulation
import TrackKat.Calibration
import TrackKat.Config
import TrackKat.Linear
import TrackKat.Pose
import TrackKat.Synth
import TrackKat.Tracker
import TrackKat.Visibility

-- | The configured clip, its reflectors tape on tabs.
tabs :: Model
tabs = clipModel (clip defaultConfig)

-- | Where the neck pivot is from a clip standing off the side of the head.
side :: V3
side = V3 100 130 60

tests :: TestTree
tests = group "Visibility" $ do
    let points = tabs{mTabs = pointReflectors}
        facing = Pose identity (V3 0 0 800)
        turnedBy y = Pose (rotYXZ y 0 0) (V3 0 0 800)
        everyOne = [LeftReflector, RightReflector, TopReflector]
        prior = mountPivot standardMounting
        sightOf m pose = map (sighted usualHead prior m pose) everyOne
    checkShowing "facing the camera, all in sight" (show (sightOf tabs facing)) $ all (== InSight) (sightOf tabs facing)
    checkShowing "turned half round, the tape faces away" (show (sightOf tabs (turnedBy 180))) $ all (== FacingAway) (sightOf tabs (turnedBy 180))
    -- (The top reflector, on its post, stands above the head.)
    checkShowing "turned half round, the lower points are behind the head" (show (sightOf points (turnedBy 180))) $ take 2 (sightOf points (turnedBy 180)) == [BehindHead, BehindHead]
    check "turned a quarter round, the tape still faces the camera, and points are in sight" $
        all (\m -> all (== InSight) (sightOf m (turnedBy 90))) [tabs, points]
    -- A clip standing off the side of the head: in sight turned toward the
    -- camera, behind the head turned away.
    let onSide m pose = map (sighted usualHead side m pose) everyOne
    checkShowing "a clip on the side of the head, turned toward the camera" (show (onSide points (turnedBy (-60)))) $ all (== InSight) (onSide points (turnedBy (-60)))
    checkShowing "and turned away, the lower ones behind the head" (show (onSide points (turnedBy 100))) $ take 2 (onSide points (turnedBy 100)) == [BehindHead, BehindHead]

-- | Frames of a clip on the side of the head turning far, the head hiding
-- it, tracked when it is in sight.
simulations :: TestTree
simulations = group "Visibility" $ do
    -- Frames of it turning 110 degrees each way, the pivot learnt: lost
    -- only while out of sight, and followed whenever in sight.
    let temple = Clip tabs AsTape (Wear (rotYXZ 0 (-23) 0) side)
        truthHead = HeadShape (V3 0 (-90) (-15)) (V3 78 110 98)
        V3 px py pz = pivotInClip (clipWear temple)
        learnt = either error id (parseCalibration ("up = 0 -0.9781476 -0.2079117\nturning = 900\npivot = " ++ unwords (map show [px, py, pz]) ++ "\npivot windows = 100\npivot information = 10 0 0 10 0 10\n"))
        turning t = 110 * sin (2 * pi * t / 12)
        path = [(t, headPose userCamera (clipWear temple) (turning t) (5 * sin (2 * pi * t / 5)) 0) | t <- framesAt 60 24]
        shown pose = length (clipDiscsOnHead defaultIntrinsics temple truthHead allLit pose)
        frames = [(t, pose, renderDiscs (jitter 0.3 i (clipDiscsOnHead defaultIntrinsics temple truthHead allLit pose))) | (i, (t, pose)) <- zip [0 ..] path]
        rows = map fst (snd (mapAccumL (\tr (t, truth, stripes) -> let (tr', o, evs) = step t stripes tr in (tr', ((t, truth, o, trackerPose tr'), evs))) (newTracker defaultIntrinsics tabs MaskLights learnt) frames))
        hiddenFrames = length [() | (_, pose) <- path, shown pose < 3]
        lostInSight = [t | ((t, truth, Lost{}, _), _) <- zip rows path, shown truth == 3]
    checkShowing "turning far with the clip on the side of the head, hidden in" (show hiddenFrames ++ " frames, lost in sight at " ++ show (take 3 lostInSight) ++ ", off by " ++ show (worstError rows)) $
        hiddenFrames > 100 && null lostInSight && fst (worstError rows) < 5
