-- | Tracking for hours holds on to no more memory than tracking for a
-- minute. These need the run-time system's statistics (+RTS -T).
module Test.Memory (simulations) where

import Control.Exception (evaluate)
import GHC.Stats (gc, gcdetails_live_bytes, getRTSStats, getRTSStatsEnabled)
import System.Mem (performMajorGC)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

import Test.Simulation
import TrackKat.Calibration
import TrackKat.Config
import TrackKat.Frame
import TrackKat.Pose
import TrackKat.Response
import TrackKat.Synth
import TrackKat.Tracker
import TrackKat.World (clockAt)

simulations :: TestTree
simulations =
    testGroup "Memory" $
        testCase "memory statistics are on (+RTS -T)" (getRTSStatsEnabled >>= assertBool "")
            : [ testCase ("memory in use stays flat from 2000 frames to 20000 with the head " ++ name) $
                    flat =<< liveWhileTracking frameAt [2000, 20000]
              | (name, frameAt) <- [("held nearly still", stillFrame), ("turning", turningFrame), ("gone", goneFrame)]
              ]
            ++ [ testCase ("memory in use stays flat from 2000 frames to 20000 shaping while paused" ++ name) $
                    flat =<< liveWhileStepping (shapeUnused wanted) newShaper [2000, 20000]
               | (name, wanted) <- [("", const NotAiming), (", aiming", const Aiming), (", aiming now and then", \i -> if even (i `div` 100) then Aiming else NotAiming)]
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
    -- Shaping while paused, its output unused.
    swaying i = let t = fromIntegral i / 120 in HeadPose{yaw = 30 * sin t, pitch = 10 * sin (t / 2), roll = 0, tx = 20 * sin (t / 3), ty = 0, tz = 0}
    shapeUnused wanted i sh = pure (snd (shape (response defaultConfig) NoTrim (wanted i) (clockAt (fromIntegral i / 120)) (swaying i) sh))

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

-- | Take steps 0, 1, ... from a state, keeping only the latest state,
-- evaluated as far as a strict IORef update would: the bytes of memory in
-- use, after a major collection, once each given number of steps has been
-- taken.
liveWhileStepping :: (Int -> a -> IO a) -> a -> [Int] -> IO [Integer]
liveWhileStepping next = go 0
  where
    go _ _ [] = pure []
    go i a (mark : rest)
        | i == mark = do
            performMajorGC
            live <- fromIntegral . gcdetails_live_bytes . gc <$> getRTSStats
            (live :) <$> go i a rest
        | otherwise = do
            a' <- next i a
            a' `seq` go (i + 1) a' (mark : rest)
