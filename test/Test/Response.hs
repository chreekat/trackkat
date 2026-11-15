-- | Shaping the head pose for games: curves, smoothing, leaning with the
-- view and trimming, and the pose as games take it.
module Test.Response (tests) where

import Control.Monad (forM_)
import Test.Tasty (TestTree)

import Test.Check
import Test.Simulation (deg)
import TrackKat.GamePose
import TrackKat.Pose (HeadPose (..))
import TrackKat.Response

tests :: TestTree
tests = group "Response" $ do
    -- Game conventions: yaw and roll positive to the left, pitch positive down.
    check "game pose signs" $
        toGamePose (HeadPose{yaw = 90, pitch = 90, roll = 90, tx = 1, ty = 2, tz = 3})
            == GamePose (realToFrac (-pi / 2 :: Double)) (realToFrac (pi / 2 :: Double)) (realToFrac (pi / 2 :: Double)) 1 (-2) 3

    -- Response curves.
    let deadzone = Points [(0.5, 0), (1.5, 1)]
        lookBehind = Points [(10, 10), (45, 120)]
        close a b = abs (a - b) < 1e-9
    check "curve deadzone" $ all (uncurry close) [(applyCurve deadzone 0.3, 0), (applyCurve deadzone 1, 0.5), (applyCurve deadzone (-1), -0.5), (applyCurve deadzone 10, 9.5)]
    check "curve gain" $ all (uncurry close) [(applyCurve lookBehind 5, 5), (applyCurve lookBehind 45, 120), (applyCurve lookBehind (-45), -120), (applyCurve lookBehind 52, 142)]

    -- Multipliers with a centre precision.
    let expos = [Multiplier m p r | m <- [0.5, 2.5, 6, 7], p <- [0, 1, 25, 40, 75, 100], r <- [yawRange, pitchRange, leanRange]]
        across e = [reach e * t / 50 | t <- [-150 .. 150]]
    forM_ expos $ \e -> do
        let f = applyCurve (Expo e)
            xs = across e
        checkShowing "expo never turns back" (show e) $ and (zipWith (<=) (map f xs) (drop 1 (map f xs)))
        checkShowing "expo the same both sides" (show e) $ all (\x -> close (f (-x)) (negate (f x))) xs
        checkShowing "expo reaches the end of the view" (show e) $ close (f (reach e)) (viewRange e) && close (f (-reach e)) (negate (viewRange e))
        checkShowing "expo continues past the reach" (show e) $ abs (f (reach e * 1.000001) - viewRange e) < 1e-3
        checkShowing "expo gentler near centre" (show e) $
            if precision e == 0
                then all (\x -> close (f x) (factor e * x)) xs
                else all (\x -> abs (f x) < factor e * abs x) [x | x <- xs, x /= 0, abs x < reach e]
    check "expo at the centre" $ applyCurve (Expo (Multiplier 6 40 yawRange)) 0 == 0
    check "expo reach" $ close (reach (Multiplier 6 40 yawRange)) 30

    -- Shaping: jumps a head can't make are held off unless they persist;
    -- smoothing settles still jitter and follows steps.
    let linear = Points [(1, 1)]
        plain = Response NoSmoothing linear linear linear linear linear linear LeanWithScreen
        smooth = plain{smoothing = Smoothing 1 0.05}
        at y = HeadPose{yaw = y, pitch = 0, roll = 0, tx = 0, ty = 0, tz = 0}
        run r frames = go newShaper (zip [0, 1 / 120 ..] frames)
          where
            go _ [] = []
            go sh ((t, p) : rest) = let (out, sh') = shape r NoTrim t p sh in yaw out : go sh' rest
    checkShowing "one-frame spike ignored" (show (run plain (map at [0, 1, 60, 2, 3]))) $
        run plain (map at [0, 1, 60, 2, 3]) == [0, 1, 1, 2, 3]
    checkShowing "persistent jump accepted" (show (run plain (map at [0, 60, 61, 62, 63]))) $
        run plain (map at [0, 60, 61, 62, 63]) == [0, 0, 0, 62, 63]
    let jittery = run smooth (map at (take 240 (cycle [0.4, -0.4])))
        steady = run smooth (map at (replicate 240 10))
    checkShowing "smoothing settles jitter" (show (maximum (map abs (drop 120 jittery)))) $ maximum (map abs (drop 120 jittery)) < 0.05
    check "smoothing reaches a held value" $ abs (last steady - 10) < 1e-6

    -- Leaning with the view: the view turned 160 degrees to the right, to
    -- about 5 o'clock, and a lean to the user's left goes toward the
    -- aircraft's right (out of the right side) and a little forward.
    let lookingBack = toGamePose (leanWithView HeadPose{yaw = 160, pitch = 0, roll = 0, tx = 10, ty = 5, tz = 0})
    checkShowing "lean out of the right side" (show lookingBack) $
        gpYaw lookingBack < 0
            && abs (gpX lookingBack - realToFrac (10 * cos (deg 160))) < 1e-4
            && gpX lookingBack < -9
            && gpZ lookingBack < -3
            && gpY lookingBack == -5
    let lookingRight = leanWithView HeadPose{yaw = 90, pitch = 0, roll = 0, tx = 0, ty = 0, tz = -10}
    checkShowing "lean toward the screen while looking right goes right" (show lookingRight) $
        abs (tx lookingRight + 10) < 1e-9 && abs (tz lookingRight) < 1e-9
    let ahead = leanWithView HeadPose{yaw = 0, pitch = 30, roll = 0, tx = 4, ty = 0, tz = -7}
    check "looking ahead, leaning is unchanged" $ abs (tx ahead - 4) < 1e-12 && abs (tz ahead + 7) < 1e-12
    -- The lean turns by the yaw the game shows, after the curves.
    let viewing = plain{yawCurve = Points [(1, 6)], lean = LeanWithView}
        (shapedBack, _) = shape viewing NoTrim 0 HeadPose{yaw = 160 / 6, pitch = 0, roll = 0, tx = 10, ty = 0, tz = 0} newShaper
    checkShowing "lean follows the curved yaw" (show shapedBack) $
        abs (yaw shapedBack - 160) < 1e-9 && abs (tx shapedBack - 10 * cos (deg 160)) < 1e-9 && abs (tz shapedBack + 10 * sin (deg 160)) < 1e-9

    -- Trimming: where the view looks, and where it is, becomes forward,
    -- until recentering.
    let trimmed t y p = fst (shape plain t 0 HeadPose{yaw = y, pitch = p, roll = 3, tx = 4, ty = 5, tz = 6} newShaper)
        axes hp = [yaw hp, pitch hp, roll hp, tx hp, ty hp, tz hp]
        lookingAt = trimmed NoTrim (-40) 20
        once = trimTo lookingAt
    checkShowing "trim takes the view" (show once) $ once == Trim (HeadPose (-40) 20 3 4 5 6)
    check "trim adds to the view" $ axes (trimmed once 5 (-5)) == [-35, 15, 6, 8, 10, 12]
    let twice = trimTo (trimmed once (-10) 10)
    checkShowing "trims add up" (show twice) $ twice == Trim (HeadPose (-50) 30 6 8 10 12)
    check "untrimmed after recentering" $ axes (trimmed NoTrim 5 (-5)) == [5, -5, 3, 4, 5, 6]
    let turnedBy y p = Trim HeadPose{yaw = y, pitch = p, roll = 0, tx = 0, ty = 0, tz = 0}
    check "trimmed yaw wraps" $ yaw (trimmed (turnedBy 170 0) 30 0) == -160 && yaw (trimmed (turnedBy (-170) 0) (-30) 0) == 160
    check "trimmed pitch stops straight up or down" $
        pitch (trimmed (turnedBy 0 70) 0 40) == 90 && pitch (trimmed (turnedBy 0 (-70)) 0 (-40)) == -90
    check "trimmed roll wraps" $ roll (trimmed (Trim (HeadPose 0 0 179 0 0 0)) 0 0) == -178
    check "trim of a view turned past behind" $
        trimTo HeadPose{yaw = 200, pitch = 100, roll = 190, tx = 0, ty = 0, tz = 0} == Trim (HeadPose (-160) 90 (-170) 0 0 0)
    let (trimmedLean, _) = shape viewing (turnedBy 150 0) 0 HeadPose{yaw = 10 / 6, pitch = 0, roll = 0, tx = 10, ty = 0, tz = 0} newShaper
    checkShowing "lean follows the trimmed yaw" (show trimmedLean) $
        abs (yaw trimmedLean - 160) < 1e-9 && abs (tx trimmedLean - 10 * cos (deg 160)) < 1e-9 && abs (tz trimmedLean + 10 * sin (deg 160)) < 1e-9
    let craned = Trim HeadPose{yaw = 0, pitch = 30, roll = 0, tx = 0, ty = -40, tz = -80}
        (lookingAway, _) = shape viewing craned 0 HeadPose{yaw = 90 / 6, pitch = 0, roll = 0, tx = 0, ty = 0, tz = 0} newShaper
    checkShowing "trimmed movement stays put as the view turns" (show lookingAway) $
        abs (tx lookingAway) < 1e-9 && abs (ty lookingAway + 40) < 1e-9 && abs (tz lookingAway + 80) < 1e-9
