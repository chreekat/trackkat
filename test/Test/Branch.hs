-- | Mirror images: a pose tracked on one is told apart and corrected.
module Test.Branch (simulations) where

import Test.Tasty (TestTree)

import Test.Check
import Test.Simulation
import TrackKat.Calibration
import TrackKat.Linear
import TrackKat.Pose
import TrackKat.Synth
import TrackKat.Tracker

simulations :: TestTree
simulations = group "Branch" $ do
    -- Mirror images. A brief loss of the clip while nodding through where
    -- its triangle squarely faces the camera: found again, it is solved
    -- onto the mirror image, on which yaw and pitch are reversed, and
    -- tracked there. Glancing about shows the mistake, and it is corrected.
    let (slipTracker, slipRows) = trackNoisy clipBent 0.2 slipping
        slipped = map fst slipRows
        slipFixed = lastMirrored slipped
    checkShowing "found again on the mirror image" (show (length (filter isMirrored slipped))) $ any isMirrored (after 2.2 slipped)
    checkShowing "mirror image corrected after" (show (slipFixed - 3) ++ " s of glancing") $ slipFixed < 3 + 2.5
    checkShowing "corrected once" (show (corrections slipRows)) $ length (corrections slipRows) == 1
    -- Corrected, what is reported is what it would have been had the clip
    -- not been lost.
    let unhidden = map fst (snd (trackNoisy clipBent 0.2 [(t, pose, allLit) | (t, pose, _) <- slipping]))
        reported rows = [(t, (yaw hp, pitch hp)) | (t, _, Tracked{outPose = hp}, _) <- after (slipFixed + 0.1) rows]
        reportDifference = maximum (0 : zipWith (\(_, (y, p)) (_, (y', p')) -> max (abs (y - y')) (abs (p - p'))) (reported slipped) (reported unhidden))
    checkShowing "then followed" (show (summed (after (slipFixed + 0.1) slipped), reportDifference)) $
        lostCount (after 2.3 slipped) == 0
            && fst (worstError (after (slipFixed + 0.1) slipped)) < 1.5
            && reportDifference < 0.5
    checkShowing "pivot learnt while tracking is plausible" (show (estimatedPivot (trackerCalibration slipTracker))) $
        plausibleCalibration clipBent (trackerCalibration slipTracker)

    -- Started on the mirror image: the post bent upright enough that, worn
    -- so, the top reflector is nearer the camera than the lower ones, while
    -- a cold start takes it to lean back. Glancing about doesn't bring the
    -- tracking round to the head's pose by itself.
    let uprightTurn t = (glance 0.5 t, 5 * sin (2 * pi * t / 4))
        upright = [(t, uncurry (headSeen 60) (uprightTurn t) 0, allLit) | t <- at120 10]
        (_, uprightRows) = trackNoisy clipBent 0.2 upright
        uprighted = map fst uprightRows
        uprightFixed = lastMirrored uprighted
    check "a cold start takes the mirror image" $ all isMirrored (take 60 uprighted)
    checkShowing "started on the mirror image, corrected after" (show (uprightFixed - 0.5) ++ " s of glancing") $ uprightFixed < 0.5 + 2.5
    checkShowing "and followed" (show (summed (after (uprightFixed + 0.1) uprighted), outputError uprightTurn (after (uprightFixed + 0.1) uprighted))) $
        lostCount uprighted == 0
            && fst (worstError (after (uprightFixed + 0.1) uprighted)) < 1.5
            && outputError uprightTurn (after (uprightFixed + 0.1) uprighted) < 3

    -- Swaying the body while glancing is no reason to switch. (Where the
    -- two poses meet, tracking can touch the mirror image for a frame or
    -- two, as it did before it could tell them apart.)
    let swayingHead = [(t, Pose r (placed ^+^ mulMV (rodrigues (V3 (deg 12) 0 0)) (V3 (30 * sin (2 * pi * t / 3.7)) 0 (20 * sin (2 * pi * t / 5.3)))), allLit) | t <- at120 20, let Pose r placed = headSeen 30 (glance 0 t) (8 * sin (2 * pi * t / 7)) 0]
        (_, swayRows) = trackNoisy clipBent 0.2 swayingHead
    checkShowing "swaying isn't corrected" (show (corrections swayRows, length (filter isMirrored (map fst swayRows)))) $
        null (corrections swayRows) && length (filter isMirrored (map fst swayRows)) < 12

    -- Looking down at a phone in the lap, held there fidgeting and swaying,
    -- and back: the lower reflectors break into pieces in about half the
    -- frames, a piece apart and the rest off centre. That says nothing of
    -- the mirror image: nothing is corrected while looking down, and
    -- facing the screen again the head's pose is followed.
    let eased t = let x = max 0 (min 1 (t / 1.5)) in x * x * (3 - 2 * x)
        phoneDown t = eased (t - 20) - eased (t - 35)
        fidget phase t = 0.5 * sin (2 * pi * t / 1.3 + phase) + 0.3 * sin (2 * pi * t / 0.7 + 2 * phase) + 0.2 * sin (2 * pi * t / 3.1 + 3 * phase)
        phoneTurn t
            | t < 20 = (glance 0 t + 2 * fidget 0 t, 8 * sin (2 * pi * t / 7) + 2 * fidget 1 t, 0)
            | t < 37 = (4 * fidget 2 t, 70 * phoneDown t + 3 * fidget 3 t, 2 * fidget 4 t * phoneDown t)
            | otherwise = (glance 37 t, 0, 0)
        sway t = V3 (6 * sin (2 * pi * t / 9.3)) (3 * sin (2 * pi * t / 7.1)) (8 * sin (2 * pi * t / 11.7))
        phoneFrames =
            [ (t, pose, renderDiscs (concat (zipWith (inPieces i p) [0 ..] (pointDiscs defaultIntrinsics clipBent allLit pose))))
            | (i, t) <- zip [0 ..] (at120 42)
            , let (y, p, r) = phoneTurn t
                  Pose headTurned placed = headSeen bentWorn y p r
                  pose = Pose headTurned (placed ^+^ sway t)
            ]
        (_, phoneRows) = trackFramesEvents clipBent phoneFrames
        faced = after 39 (map fst phoneRows)
    checkShowing "looking down with reflectors in pieces isn't corrected" (show (corrections phoneRows)) $ all (>= 35) (corrections phoneRows)
    checkShowing "and facing the screen again followed" (show (summed faced)) $ lostCount faced == 0 && fst (worstError faced) < 3
