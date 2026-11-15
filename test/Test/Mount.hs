-- | Clips mounted otherwise than the standard way.
module Test.Mount (simulations) where

import Control.Monad (forM_)
import Test.Tasty (TestTree)

import Test.Check
import Test.Simulation
import TrackKat.Calibration
import TrackKat.Config
import TrackKat.Linear
import TrackKat.Pose
import TrackKat.Synth
import TrackKat.Tracker

-- | A clip mounted otherwise than the standard way, on a head glancing about
-- in front of a level camera, its neck pivot some way from where the
-- mounting says it usually is: once recentered, the head's turning is
-- reported as it turns, its pivot is learnt where it is, and the clip is
-- drawn on the head where it sits. For points, and for tape on tabs, which
-- face the way the mounting turns them.
simulations :: TestTree
simulations =
    group "Mount"
        $ forM_
            [ ("rolled 90 degrees about the head's forward axis", rotYXZ 0 0 90, AsTape)
            , ("turned 45 degrees about the head's vertical", rotYXZ 45 0 0, AsPoints)
            ]
        $ \(how, turn, look) -> do
            let mount = Mounting turn (V3 (-40) 100 80)
                tabbedClip = clipModel (clip defaultConfig)
                m = mounted mount (if look == AsTape then tabbedClip else tabbedClip{mTabs = pointReflectors})
                neck = V3 (-30) 115 70
                level = userCamera{camDown = 0}
                c = Clip m look (Wear identity neck)
                turned t = if t < 1 then (0, 0, 0) else (glanceYaw (t - 1), glancePitch (t - 1), glanceRoll (t - 1))
                path = [(t, headPose level (clipWear c) y p r, allLit) | t <- framesAt 60 40, let (y, p, r) = turned t]
                (tracker, rows) = trackFramesEvents m (clipFrames c 0.2 path)
                poseOff = (fst (worstError (unmerged (map fst rows))), fst (worstError (map fst rows)))
                -- Reported about the level learnt, which forty seconds of
                -- glancing leave off by a degree or so, and after recentering on
                -- the head facing the camera.
                angleOff = maximum (0 : [maximum (map abs [yaw hp - y, pitch hp - p, roll hp - r]) | ((t, _, Tracked{outPose = hp}, _), _) <- rows, let (y, p, r) = turned t])
                cal = trackerCalibration tracker
                pivotOff = norm (estimatedPivot cal ^-^ neck)
                drawn = learntClip m (trackerCentre tracker) cal
                -- The window draws the clip from the pivot along the head's
                -- axes mirrored: x to the wearer's right, y up, z forward.
                expected = (neck ^-^ mLeft m, neck ^-^ mRight m, neck ^-^ mTop m)
                drawnOff = case drawn of
                    Just (l, r, t) -> let (l', r', t') = expected in maximum [norm (l ^-^ l'), norm (r ^-^ r'), norm (t ^-^ t')]
                    Nothing -> 1 / 0
                name = "a clip " ++ how ++ (if look == AsTape then ", on tabs" else ", points")
            checkShowing (name ++ ": followed") ("its pose off by at most " ++ show poseOff ++ " degrees (reflectors apart, and all), its turning reported off by " ++ show angleOff) $
                lostCount (map fst rows) == 0 && fst poseOff < 1.5 && snd poseOff < 4 && angleOff < 4
            checkShowing (name ++ ": its pivot learnt") ("" ++ show pivotOff ++ " mm off, and drawn on the head " ++ show drawnOff ++ " mm off") $
                pivotOff < 8 && drawnOff < 10
