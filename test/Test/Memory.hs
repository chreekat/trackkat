{-# LANGUAGE LambdaCase #-}

-- | Tracking for hours holds on to no more memory than tracking for a
-- minute, as 'retainedBytes' counts it.
module Test.Memory (tests, simulations, retainedWhileStepping) where

import Control.Exception (evaluate)
import qualified Data.IntMap.Strict as IntMap
import Foreign.Storable (sizeOf)
import GHC.Exts.Heap (Box (..), GenClosure (BlackholeClosure, IndClosure, TSOClosure), allClosures, asBox, getBoxedClosureData)
import GHC.Exts.Heap.Closures (closureSize)
import System.Mem.StableName (hashStableName, makeStableName)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Test.Simulation
import TrackKat.Calibration
import TrackKat.Config
import TrackKat.Frame
import TrackKat.Pose
import TrackKat.Response
import TrackKat.Synth
import TrackKat.Tracker
import TrackKat.World (clockAt)

tests :: TestTree
tests =
    testGroup
        "retained bytes"
        [ testCase "a list holds on to its cells and its numbers" $ do
            short <- retainedBytes =<< numbers 1000
            long <- retainedBytes =<< numbers 2000
            long - short @?= 1000 * (3 + 2) * wordBytes
        , testCase "what is held twice is counted once" $ do
            xs <- numbers 1000
            once <- retainedBytes xs
            twice <- retainedBytes (xs, xs)
            twice - once @?= 3 * wordBytes
        , testCase "a value not yet worked out holds on to what it will need" $ do
            xs <- numbers 1000
            alone <- retainedBytes xs
            owed <- retainedBytes (Just (sum xs))
            assertBool (show (alone, owed)) (owed > alone)
        ]
  where
    -- Evaluated through, so that each is a cons cell and a Double.
    numbers n = let xs = map fromIntegral [1 .. n :: Int] :: [Double] in evaluate (foldr seq () xs `seq` xs)

simulations :: TestTree
simulations =
    testGroup "Memory" $
        [ testCase ("memory in use stays flat from 2000 frames to 20000 with the head " ++ name) $
            flat =<< retainedWhileTracking frameAt [2000, 20000]
        | (name, frameAt) <- [("held nearly still", stillFrame), ("turning", turningFrame), ("gone", goneFrame)]
        ]
            ++ [ testCase ("memory in use stays flat from 2000 frames to 20000 shaping while paused" ++ name) $
                    flat =<< retainedWhileStepping (shapeUnused wanted) newShaper [2000, 20000]
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
-- keeping only the tracker: the bytes it holds on to once each given
-- number of frames has been tracked.
retainedWhileTracking :: (Int -> (Double, [Stripe])) -> [Int] -> IO [Integer]
retainedWhileTracking frameAt = retainedWhileStepping track (newTracker defaultIntrinsics clipBent MaskLights defaultCalibration)
  where
    track i t = do
        let (time, stripes) = frameAt i
            (t', o, evs) = step (clockAt time) stripes t
        _ <- evaluate (length (show o) + length evs)
        pure t'

-- | Take steps 0, 1, ... from a state, keeping only the latest state,
-- evaluated as far as a strict IORef update would: the bytes it holds on
-- to once each given number of steps has been taken.
retainedWhileStepping :: (Int -> a -> IO a) -> a -> [Int] -> IO [Integer]
retainedWhileStepping next = go 0
  where
    go _ _ [] = pure []
    go i a (mark : rest)
        | i == mark = do
            held <- retainedBytes a
            (held :) <$> go i a rest
        | otherwise = do
            a' <- next i a
            a' `seq` go (i + 1) a' (mark : rest)

-- | The bytes of the heap a value holds on to: every closure reachable from
-- it, each counted once, evaluated or not, and none forced. It is the
-- value's own, whatever else the program holds at the time.
retainedBytes :: a -> IO Integer
retainedBytes root = go IntMap.empty 0 [asBox root]
  where
    go _ total [] = pure total
    go seen total (b@(Box x) : rest) =
        getBoxedClosureData b >>= \case
            -- An indirection has the stable name of what it leads to, and
            -- is gone after the next collection: count only that.
            IndClosure _ to -> go seen total (to : rest)
            BlackholeClosure _ to -> go seen total (to : rest)
            c -> do
                name <- makeStableName x
                let key = hashStableName name
                if maybe False (elem name) (IntMap.lookup key seen)
                    then go seen total rest
                    else go (IntMap.insertWith (++) key [name] seen) (total + fromIntegral (closureSize b) * wordBytes) (held c ++ rest)
    -- A thread is what evaluates a value, not part of it.
    held c = case c of
        TSOClosure{} -> []
        _ -> allClosures c

wordBytes :: Integer
wordBytes = fromIntegral (sizeOf (0 :: Word))
