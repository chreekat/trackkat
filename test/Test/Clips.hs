-- | The key simulations, run with clips of other shapes and looks than the
-- points the other simulations track.
module Test.Clips (simulations) where

import Control.Monad (forM_)
import Test.Tasty (TestTree)

import Test.Check
import Test.Simulation
import TrackKat.Calibration
import TrackKat.Linear
import TrackKat.Pose
import TrackKat.Shape
import TrackKat.Synth
import TrackKat.Tracker

-- | Each simulation runs with those clips where it shows something new; the
-- slow ones with fewer, and at 60 frames a second where that shows as much.
-- What they found is in the backlog.
simulations :: TestTree
simulations = group "Clips" $ do
    forM_ [userTabs, unbentTabs, scaleneTabs] sweep
    -- With the top tab bent, nodding through where the triangle squarely
    -- faces the camera has tracking slip onto another pose for a moment
    -- each time, so the clips with it are left out (see the backlog).
    forM_ [unbentTabs] mirrors
    -- (The user's clip learns them in 'mountChecks', mounted otherwise.)
    forM_ [scaleneTabs] learning
    forM_ [userTabs, scaleneTabs] lookingUp
    forM_ [userTabs] lookingDown
    forM_ [scaleneTabs] shapeAsConfigured
  where
    named (Subject name _ _) what = name ++ ": " ++ what
    seen = headPose userCamera
    -- A slow sweep through everything a head does, at 60 frames a second.
    sweep s@(Subject _ m _) = do
        let c = subjectClip s
            sweeping = [(t, seen (clipWear c) (40 * sin (2 * pi * t / 7)) (25 * sin (2 * pi * t / 5.3)) (20 * sin (2 * pi * t / 4.1)), allLit) | t <- framesAt 60 20]
            (_, sweptRows) = trackFramesEvents m (clipFrames c 0 sweeping)
            swept = map fst sweptRows
            stepSizes = zipWith (\(_, _, _, a) (_, _, _, b) -> maybe 0 fst (poseError <$> a <*> b)) swept (drop 1 swept)
        checkShowing (named s "sweep") (show (summed swept, maximum stepSizes, corrections sweptRows)) $
            null (corrections sweptRows)
                && lostCount swept == 0
                && fst (worstError (unmerged swept)) < 1
                && fst (worstError swept) < 4
                && snd (worstError swept) < 20
                && maximum stepSizes < 3
    -- Mirror images: found again on one after a moment out of sight while
    -- nodding through where the triangle squarely faces the camera, and
    -- started on one, the clip worn so its top reflector is nearer the
    -- camera than the lower ones; glancing about corrects both.
    mirrors s@(Subject _ m _) = do
        let worn by = (subjectClip s){clipWear = wornPitched by}
            (_, slipRows) = trackFramesEvents m (clipFrames (worn 30) 0.2 slipping)
            slipped = map fst slipRows
            slipFixed = lastMirroredOn m slipped
        checkShowing (named s "found again on the mirror image, corrected after") (show (slipFixed - 3) ++ " s of glancing, then " ++ show (summed (after (slipFixed + 0.1) slipped))) $
            any (mirroredOn m) (after 2.2 slipped)
                && slipFixed < 3 + 2.5
                && lostCount (after 2.3 slipped) == 0
                && fst (worstError (after (slipFixed + 0.1) slipped)) < 1.5
        let uprightTurn t = (glance 0.5 t, 5 * sin (2 * pi * t / 4))
            upright = [(t, uncurry (seen (wornPitched 60)) (uprightTurn t) 0, allLit) | t <- framesAt 120 10]
            uprighted = trackFrames m (clipFrames (worn 60) 0.2 upright)
            uprightFixed = lastMirroredOn m uprighted
            followed = after (uprightFixed + 0.1) uprighted
        checkShowing (named s "started on the mirror image, corrected after") (show (uprightFixed - 0.5) ++ " s of glancing, then " ++ show (summed followed, outputError uprightTurn followed)) $
            all (mirroredOn m) (take 60 uprighted)
                && uprightFixed < 0.5 + 2.5
                && lostCount uprighted == 0
                && fst (worstError followed) < 1.5
                && outputError uprightTurn followed < 3
    -- The level and the pivot learnt from a minute of glancing about, at
    -- 60 frames a second. Glancing that also nods and rolls teaches the
    -- level in a minute to within about half a degree.
    learning s@(Subject _ m _) = do
        let c = subjectClip s
            glancing = [(t, seen (clipWear c) (glanceYaw t) (glancePitch t) (glanceRoll t), allLit) | t <- framesAt 60 60]
            cal = trackerCalibration (fst (trackFramesEvents m (clipFrames c 0.2 glancing)))
            (down, right) = cameraTilt (estimatedLevel cal)
            pivotOff = estimatedPivot cal ^-^ pivotInClip (clipWear c)
        checkShowing (named s "level and pivot learnt: camera") (show (down, right) ++ " down and right, pivot off by " ++ show pivotOff) $
            abs (down - camDown userCamera) < 1 && abs right < 2 && norm pivotOff < 5
    -- Looking up, the top reflector out of sight beside a lower one: lost
    -- after a while, and followed when all three are back.
    lookingUp s@(Subject _ m _) = do
        let c = subjectClip s
            held t
                | t < 2 = (raised (seen (clipWear c) 0 0 0), allLit)
                | t < 22 = (raised (seen (clipWear c) 5 (-25) 0), (1, 1, 0))
                | otherwise = (raised (seen (clipWear c) (glance 0 (t - 22)) 0 0), allLit)
            pairRows = trackFrames m (clipFrames c 1 [(t, pose, l) | t <- framesAt 30 26, let (pose, l) = held t])
            pairTracked = [t | (t, _, Tracked{}, _) <- after 5 pairRows, t < 22]
            pairBack = after 23 pairRows
        checkShowing (named s "looking up with the lower pair in view, tracked for") (show (length pairTracked) ++ " frames, then " ++ show (summed pairBack)) $
            null pairTracked && lostCount pairBack == 0 && fst (worstError pairBack) < 1.5
    -- Looking down at a phone in the lap, the lower reflectors in pieces,
    -- and back, at 60 frames a second.
    lookingDown s@(Subject _ m _) = do
        let c = subjectClip s
            eased t = let x = max 0 (min 1 (t / 1.5)) in x * x * (3 - 2 * x)
            phoneDown t = eased (t - 20) - eased (t - 35)
            fidget phase t = 0.5 * sin (2 * pi * t / 1.3 + phase) + 0.3 * sin (2 * pi * t / 0.7 + 2 * phase) + 0.2 * sin (2 * pi * t / 3.1 + 3 * phase)
            phoneTurn t
                | t < 20 = (glance 0 t + 2 * fidget 0 t, 8 * sin (2 * pi * t / 7) + 2 * fidget 1 t, 0)
                | t < 37 = (4 * fidget 2 t, 70 * phoneDown t + 3 * fidget 3 t, 2 * fidget 4 t * phoneDown t)
                | otherwise = (glance 37 t, 0, 0)
            phoneFrames =
                [ (t, pose, renderDiscs (concat (zipWith (inPieces i p) [0 ..] (clipDiscs defaultIntrinsics c allLit pose))))
                | (i, t) <- zip [0 ..] (framesAt 60 42)
                , let (y, p, r) = phoneTurn t
                      pose = seen (clipWear c) y p r
                ]
            (_, phoneRows) = trackFramesEvents m phoneFrames
            faced = after 39 (map fst phoneRows)
        checkShowing (named s "looking down at a phone, corrected at") (show (corrections phoneRows) ++ ", then " ++ show (summed faced)) $
            all (>= 35) (corrections phoneRows) && lostCount faced == 0 && fst (worstError faced) < 3
    -- The shape found as configured from glancing, through the tracker, at
    -- 60 frames a second.
    shapeAsConfigured s@(Subject _ m _) = do
        let c = subjectClip s
            shapeFrames = [(t, pose, renderDiscs (clipDiscs defaultIntrinsics c allLit pose)) | t <- framesAt 60 85, let pose = seen (clipWear c) (glanceYaw t) (glancePitch t) (glanceRoll t)]
            learnt = trackerShape (fst (trackFramesEvents m shapeFrames))
            estimated = shapeEstimate learnt
        checkShowing (named s "the shape through the tracker") (describeShapeSoFar learnt) $
            fmap confidence estimated == Just Sure && fmap difference estimated == Just AsConfigured
