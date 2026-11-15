-- | The clip's reflectors as tape on curved tabs, and its shape learnt from
-- how the head moves.
module Test.Shape (simulations) where

import Control.Monad (forM_)
import Test.Tasty (TestTree)

import Test.Check
import Test.Simulation
import TrackKat.Config
import TrackKat.Pose
import TrackKat.Shape
import TrackKat.Synth
import TrackKat.Tracker

simulations :: TestTree
simulations = group "Shape" $ do
    -- Curved tabs: each reflector is tape round a tab, and the centre of
    -- its light slides round the arc toward the camera as the head turns
    -- (see Note [Curved reflectors] in TrackKat.Pose). Turning left and
    -- right while nodding, with the reflectors taken for points, the clip
    -- is tracked moving several mm it didn't, and turning a little off;
    -- taken for tape on tabs, both come to about what tracking points
    -- drawn as discs leaves.
    let tabTurning t = (50 * sin (2 * pi * t / 4), 15 * sin (2 * pi * t / 2.7))
        tabFrames = [(t, pose, renderDiscs (tapeDiscs defaultIntrinsics (clipModel (clip defaultConfig)) allLit pose)) | t <- at120 8, let (y, p) = tabTurning t; pose = headSeen bentWorn y p 0]
        asPoints = trackFrames clipBent tabFrames
        asTabs = trackFrames (clipModel (clip defaultConfig)) tabFrames
        ((pointsTurn, pointsMoved), _) = turnError asPoints
        ((tabsTurn, tabsMoved), _) = turnError asTabs
    checkShowing "tape on tabs taken for points, off by" (show (turnError asPoints)) $ lostCount asPoints == 0 && pointsMoved > 5
    checkShowing "tape on tabs followed, off by" (show (turnError asTabs)) $
        lostCount asTabs == 0 && tabsMoved < 0.3 * pointsMoved && tabsTurn < 0.75 * pointsTurn

    -- The clip's shape from how the head moves (see Note [Clip shape from
    -- movement] in TrackKat.Shape): three minutes of glancing, the clip
    -- other than configured, its reflectors seen with 0.2 px of noise,
    -- show the true shape within a mm or so, scaled to the configured left
    -- to right distance; as configured, it is found so.
    let configured = clipModel (clip defaultConfig)
        shapeCases =
            [ ("75 105 115", (75, 105, 115), (98, 107.33), Different)
            , ("with its post straight, 70 116 116", (70, 116, 116), (116, 116), Different)
            , ("as configured", (70, 110, 110), (110, 110), AsConfigured)
            ]
    forM_ shapeCases $ \(name, (lr, lt, rt), (wantLt, wantRt), want) -> do
        let truth = maybe (error "shape") clipModel (clipShape lr lt rt)
            (learner, told) = shapeSession truth configured 3
            estimated = shapeEstimate learner
            (_, gotLt, gotRt) = maybe (0, 0, 0) (wornDistances . estimatedModel) estimated
        checkShowing ("the clip's shape, " ++ name ++ ", estimated") (describeShapeSoFar learner) $
            fmap confidence estimated == Just Sure
                && abs (gotLt - wantLt) < 1.5
                && abs (gotRt - wantRt) < 1.5
                && fmap difference estimated == Just want
        checkShowing ("the clip's shape, " ++ name ++ ", told as " ++ show want) (show (map describeShape told)) $
            not (null told) && all ((== want) . difference) told

    -- Through the tracker, from curved tabs: learnt from clear poses only,
    -- and found as configured, or found to be so when configured with the
    -- post straight.
    let shapeFrames = [(t, pose, renderDiscs (tapeDiscs defaultIntrinsics (clipModel (clip defaultConfig)) allLit pose)) | t <- at120 100, let pose = headSeen bentWorn (glanceYaw t) (glancePitch t) (glanceRoll t)]
        straightened = maybe (error "shape") clipModel (clipShape 70 116 116)
    forM_ [("as configured", configured, AsConfigured), ("configured straight", straightened, Different)] $ \(name, m, want) -> do
        let (shapeTracker, shapeRows) = trackFramesEvents m shapeFrames
            shapesTold = [est | (_, evs) <- shapeRows, ShapeEstimated est <- evs]
            estimated = shapeEstimate (trackerShape shapeTracker)
            (_, gotLt, gotRt) = maybe (0, 0, 0) (wornDistances . estimatedModel) estimated
        checkShowing ("the clip's shape through the tracker, " ++ name) (describeShapeSoFar (trackerShape shapeTracker)) $
            fmap confidence estimated == Just Sure && fmap difference estimated == Just want && abs (gotLt - 110) < 1.5 && abs (gotRt - 110) < 1.5
        checkShowing ("the clip's shape through the tracker, " ++ name ++ ", told as " ++ show want) (show (map describeShape shapesTold)) $ not (null shapesTold) && all ((== want) . difference) shapesTold
