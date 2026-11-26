-- | Learning which way is up.
module Test.Level (simulations) where

import Control.Monad (forM_)
import Data.List (isInfixOf)
import Test.Tasty (TestTree)

import Test.Check
import Test.Simulation (glancePitch, glanceRoll, glanceYaw)
import TrackKat.Calibration
import TrackKat.Linear
import TrackKat.Pose
import TrackKat.Synth
import TrackKat.World (clockAt)

-- | Learning up (see Note [Learning up]) from glancing about, three minutes
-- at a time at 60 poses a second, each with a little noise, the body
-- swaying: from a wrong start, with roll that goes with yaw, after the
-- camera moves, and how the pivot of a clip worn rolled on the head is
-- told once it is learnt.
simulations :: TestTree
simulations = group "Level" $ do
    let m = maybe (error "bad test triangle") id (triangle 30 70 110 110)
        deg = (* (pi / 180))
        neck = V3 0 130 90
        -- A camera tipped down and rolled to its right by the given
        -- degrees, sees a head turned by the given yaw, pitch and roll
        -- about its neck pivot, wearing a clip turned as given on it.
        seen (down, right) worn t y p r = Pose (tilt `mulMM` turned `mulMM` worn) (mulMV tilt (place ^-^ mulMV turned neck))
          where
            tilt = rotYXZ 0 down 0 `mulMM` rotYXZ 0 0 right
            turned = rotYXZ y p r
            place = V3 (25 * sin (2 * pi * t / 9.1)) (250 + 8 * sin (2 * pi * t / 13)) (900 + 20 * sin (2 * pi * t / 11.3))
        noisy i (Pose r t) =
            Pose
                (rodrigues (V3 (deg 0.2 * noise i 1) (deg 0.2 * noise i 2) (deg 0.2 * noise i 3)) `mulMM` r)
                (t ^+^ V3 (noise i 4) (noise i 5) (2 * noise i 6))
        -- Glancing 30 degrees each way, nodding 15 and tilting 10, the head
        -- rolling by the given share of its yaw as well.
        glancing cam worn rolling from seconds =
            [ (time, noisy i (seen cam worn time y (1.5 * glancePitch time) (1.4 * glanceRoll time + rolling * y)))
            | i <- [0 .. round (seconds * 60 :: Double) :: Int]
            , let time = from + fromIntegral i / 60
                  y = 1.1 * glanceYaw time
            ]
        learnFrom c poses = foldl' (\l (time, pose) -> fst (learn (clockAt time) pose l)) (newLearner m c) poses
        upOf (down, right) = mulMV (rotYXZ 0 down 0 `mulMM` rotYXZ 0 0 right) (V3 0 (-1) 0)
        off c up = angleBetween (levelUp (estimatedLevel c)) up * 180 / pi
        camera = (12, 4)
        startingAt (down, right) = either error id (parseCalibration standardMounting ("up = " ++ unwords (map show (let V3 x y z = upOf (down, right) in [x, y, z])) ++ "\nturning = 3600\n"))
    -- A level learnt 25 degrees off, as an earlier way of learning could
    -- hold on to, is learnt again within three minutes, and with it the
    -- pivot of a centred clip is told under the head again, not off to one
    -- side by its height times the sine of how far up is off about the
    -- line of sight (see Note [Learning up]).
    let fromWrong = learnerCalibration (learnFrom (startingAt (11, -21)) (glancing camera identity 0 0 180))
        sideways = maybe (1 / 0) (\(l, r, _) -> let V3 x _ _ = 0.5 *^ (l ^+^ r) in x) (learntClip m (Just (seen camera identity 0 0 0 0)) fromWrong)
    checkShowing "a wrong level learnt again" (show (off fromWrong (upOf camera)) ++ " degrees off, the pivot told " ++ show sideways ++ " mm to one side") $
        off fromWrong (upOf camera) < 2 && abs sideways < 4
    -- Learnt afresh, up is the axis of turning left and right, even with
    -- the head rolling with its yaw, a sixth as much: that axis leans out
    -- of the face by about 9 degrees.
    forM_ [0, 1 / 6] $ \rolling -> do
        let afresh = learnerCalibration (learnFrom defaultCalibration (glancing camera identity rolling 0 180))
            turningAxis = normalize (mulMV (rotYXZ 0 12 0 `mulMM` rotYXZ 0 0 4) (V3 0 (-1) (negate rolling)))
        checkShowing ("up learnt afresh, the head rolling " ++ show rolling ++ " of its yaw") (show (off afresh turningAxis) ++ " degrees off its axis of turning") $
            off afresh turningAxis < 1.5
    -- A camera tipped and rolled 10 degrees more after three minutes is
    -- learnt again in three more.
    let moved = (20, -6)
        movedOn = learnFrom defaultCalibration (glancing camera identity 0 0 180 ++ glancing moved identity 0 180 180)
    checkShowing "a moved camera learnt again" (show (off (learnerCalibration movedOn) (upOf moved)) ++ " degrees off") $
        off (learnerCalibration movedOn) (upOf moved) < 2
    -- Calibrating level still sets it at once.
    let calibrated = calibrateLevel (levelFromUp (upOf moved)) (learnFrom defaultCalibration (glancing camera identity 0 0 60))
    check "calibrating level sets it at once" $
        off (learnerCalibration calibrated) (upOf moved) < 0.01 && angleBetween (levelUp (appliedLevel calibrated)) (upOf moved) < 1e-4
    -- A centred clip worn rolled 15 degrees on the head, configured as worn
    -- the standard way or as it is: its pivot is told and drawn under the
    -- head, below and behind the clip, not off to one side.
    let rolled = rotYXZ 0 0 15
    forM_ [("as worn the standard way", m, rolled), ("as mounted so", mounted (Mounting rolled (V3 0 100 100)) m, identity)] $ \(how, model, worn) -> do
        let c = learnerCalibration (foldl' (\l (time, pose) -> fst (learn (clockAt time) pose l)) (newLearner model defaultCalibration) (glancing camera worn 0 0 180))
            centre = seen camera worn 0 0 0 0
            drawn = learntClip model (Just centre) c
            V3 x y z = maybe (V3 (1 / 0) 0 0) (\(l, r, _) -> 0.5 *^ (l ^+^ r)) drawn
        checkShowing ("a clip worn rolled 15 degrees, " ++ how) (describePivotBriefly (Just centre) c ++ " Drawn at " ++ show (x, y, z)) $
            abs x < 4 && abs (y - 130) < 8 && abs (z - 90) < 8 && ("to your" `isInfixOf` describePivotBriefly (Just centre) c)
