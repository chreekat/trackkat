-- | The old tracker, tracking for hours, holds on to no more memory than
-- tracking for a minute. These need the run-time system's statistics
-- (+RTS -T).
module Test.Old.Memory (simulations) where

import Control.Exception (evaluate)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

import Test.Memory (liveWhileStepping)
import Test.Old.Simulation
import TrackKat.Calibration
import TrackKat.Frame
import TrackKat.Old.Tracker
import TrackKat.Pose
import TrackKat.Synth
import TrackKat.World (clockAt)

simulations :: TestTree
simulations =
    testGroup
        "Memory"
        [ testCase ("memory in use stays flat from 2000 frames to 20000 with the head " ++ name) $
            flat =<< liveWhileTracking frameAt [2000, 20000]
        | (name, frameAt) <- [("held nearly still", stillFrame), ("turning", turningFrame), ("gone", goneFrame)]
        ]
  where
    flat lives = assertBool (show lives) $ case lives of
        [short, long] -> long - short < 100000
        _ -> False
    -- The head by a lit fire: nearly still facing the camera (where its
    -- pose and the mirror image meet), turning left and right, and gone.
    fireLit i t pose = (t, renderDiscs (heads ++ occluded heads (fireDiscs fireAt i)))
      where
        heads = pointDiscs defaultIntrinsics clipBent allLit pose
    stillFrame i = let t = fromIntegral i / 120 in fireLit i t (fidgeting t)
    turningFrame i = let t = fromIntegral i / 120 in fireLit i t (raised (headSeen bentWorn (25 * sin (2 * pi * t / 3)) (8 * sin (2 * pi * t / 5)) 0))
    goneFrame i =
        let t = fromIntegral i / 120
         in if t < 10 then fireLit i t (fidgeting t) else (t, renderDiscs (fireDiscs fireAt i))

-- | Track the i-th frame of (time, stripes) for i from 0, as the app does,
-- keeping only the tracker: the bytes of memory in use, after a major
-- collection, once each given number of frames has been tracked.
liveWhileTracking :: (Int -> (Double, [Stripe])) -> [Int] -> IO [Integer]
liveWhileTracking frameAt = liveWhileStepping track (newTracker defaultIntrinsics clipBent MaskLights defaultCalibration)
  where
    track i t = do
        let (time, stripes) = frameAt i
            (t', o, evs) = step (clockAt time) stripes t
        _ <- evaluate (length (show o) + length evs)
        pure t'
