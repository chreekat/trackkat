-- | Poses solved from where the reflectors show, and level from turning.
module Test.Pose (tests) where

import Control.Monad (forM_)
import Data.Maybe (isNothing)
import Test.Tasty (TestTree)

import Test.Check
import Test.Simulation (deg, headset)
import TrackKat.Config
import TrackKat.Linear
import TrackKat.Pose
import TrackKat.Synth

tests :: TestTree
tests = group "Pose" $ do
    -- Pose: project known head poses and recover them.
    let reference = Pose identity (V3 0 0 700)
        neck = V3 0 160 50
        poses =
            [ (y, p, r, dt0)
            | (y, p, r) <- [(0, 0, 0), (20, 0, 0), (-35, 10, 0), (10, -15, 8), (0, 0, -20), (40, 20, 10)]
            , dt0 <- [V3 0 0 0, V3 30 (-20) 50]
            ]
    forM_ poses $ \(y, p, r, dt) -> do
        let truth = Pose (rotYXZ y p r) (V3 0 0 700 ^+^ dt)
            img = (project defaultIntrinsics truth (mLeft trackClip), project defaultIntrinsics truth (mRight trackClip), project defaultIntrinsics truth (mTop trackClip))
            -- Start from a pose a few degrees and mm off, as frame-to-frame tracking would.
            guess = Pose (rotYXZ (y + 3) (p - 2) (r + 2)) (V3 0 0 710 ^+^ dt)
        case solvePose defaultIntrinsics trackClip (Just guess) img of
            Nothing -> checkShowing "solve" (show (y, p, r)) False
            Just (sol, err) -> do
                let hp = relativePose cameraLevel neck reference sol
                    V3 ex ey ez = poseAt truth neck ^-^ (neck ^+^ V3 0 0 700)
                checkShowing "reprojection" (show (y, p, r, err)) (err < 1e-3)
                check
                    ("angles " ++ show (y, p, r) ++ " got " ++ show hp)
                    (all (< 0.01) [abs (yaw hp - y), abs (pitch hp - p), abs (roll hp - r)])
                checkShowing "translation" (show hp) (all (< 0.1) [abs (tx hp - ex), abs (ty hp - ey), abs (tz hp - ez)])

    -- With no previous pose, start from "facing the camera"; fine for modest angles.
    forM_ [(15, 5, 0), (-10, -10, 5)] $ \(y, p, r) -> do
        let truth = Pose (rotYXZ y p r) (V3 40 (-30) 650)
            img = (project defaultIntrinsics truth (mLeft trackClip), project defaultIntrinsics truth (mRight trackClip), project defaultIntrinsics truth (mTop trackClip))
        case solvePose defaultIntrinsics trackClip Nothing img of
            Just (sol, _) ->
                let hp = relativePose cameraLevel neck reference sol
                 in check
                        ("cold start " ++ show (y, p, r) ++ " got " ++ show hp)
                        (all (< 0.01) [abs (yaw hp - y), abs (pitch hp - p), abs (roll hp - r)])
            Nothing -> check "cold start solved" False

    -- Headset clip, post leaning back: from a cold start the solver should pick
    -- the right one of the two mirror-image solutions.
    forM_ [(0, 0, 0), (12, -8, 0), (-10, 10, 5)] $ \(y, p, r) -> do
        let truth = Pose (rotYXZ y p r) (V3 20 (-150) 700)
            img = (project defaultIntrinsics truth (mLeft headset), project defaultIntrinsics truth (mRight headset), project defaultIntrinsics truth (mTop headset))
        case solvePose defaultIntrinsics headset Nothing img of
            Just (sol, _) ->
                let hp = relativePose cameraLevel neck reference sol
                 in check
                        ("headset cold start " ++ show (y, p, r) ++ " got " ++ show hp)
                        (all (< 0.01) [abs (yaw hp - y), abs (pitch hp - p), abs (roll hp - r)])
            Nothing -> check "headset cold start solved" False

    -- Level axes from turning the head left and right, seen by a camera tipped
    -- 13 degrees down and 4 to its right.
    let tilt = rodrigues (V3 0 0 (deg 4)) `mulMM` rodrigues (V3 (deg 13) 0 0)
        seenFrom rLevel tLevel = Pose (tilt `mulMM` rLevel) (mulMV tilt tLevel)
        neutral = seenFrom identity (V3 0 0 700)
        observe truth =
            let img = (project defaultIntrinsics truth (mLeft headset), project defaultIntrinsics truth (mRight headset), project defaultIntrinsics truth (mTop headset))
             in fst <$> solvePose defaultIntrinsics headset (Just truth) img
        relativeRot pose = poseR pose `mulMM` transpose (poseR neutral)
        sweep = [seenFrom (rotYXZ y 0 0) (V3 0 0 700) | y <- [-40, -30 .. 40]]
    case levelFromYaws . map relativeRot =<< traverse observe sweep of
        Nothing -> check "level from yaws" False
        Just level -> do
            let (down, right) = cameraTilt level
            checkShowing "camera tilt" (show (down, right)) (abs (down - 13) < 0.01 && abs (right - 4) < 0.01)
            forM_ [(20, 10, 5), (-30, -15, 0), (0, 0, 25)] $ \(y, p, r) ->
                case observe (seenFrom (rotYXZ y p r) (V3 0 0 700)) of
                    Nothing -> check "levelled pose solved" False
                    Just sol -> do
                        let hp = relativePose level neck neutral sol
                        check
                            ("levelled pose " ++ show (y, p, r) ++ " got " ++ show hp)
                            (all (< 0.01) [abs (yaw hp - y), abs (pitch hp - p), abs (roll hp - r)])
    check "too little turning" $ isNothing (levelFromYaws [identity, rotYXZ 2 0 0])

    check "identify" $ identify trackClip id [(297, 154), (254, 122), (249, 154)] == Just ((249, 154), (297, 154), (254, 122))

    -- The clip's sides are the wearer's; the model's are the camera's.
    case clipShape 70 100 120 of
        Just cl ->
            let m = clipModel cl
             in check "clip sides are the wearer's" $
                    abs (norm (mTop m ^-^ mLeft m) - 120) < 1e-9 && abs (norm (mTop m ^-^ mRight m) - 100) < 1e-9
        Nothing -> check "asymmetric clip" False
