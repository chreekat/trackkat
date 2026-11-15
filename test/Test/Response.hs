-- | Shaping the head pose for games: curves, smoothing, leaning with the
-- view and trimming, and the pose as games take it.
module Test.Response (tests) where

import Control.Monad (forM_)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck hiding (once)

import Test.Check
import Test.Gen (Fraction (..), genPoints, shrinkFrom, shrinkPoints, shrinkWithin)
import qualified Test.Gen as Gen
import Test.Instances ()
import Test.Simulation (deg)
import TrackKat.GamePose
import TrackKat.Pose (HeadPose (..))
import TrackKat.Response
import TrackKat.World (World (..), clockAt, inDegrees, inMillimetres, world)

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
        run r frames = go newShaper (zip (map clockAt [0, 1 / 120 ..]) frames)
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
        (shapedBack, _) = shape viewing NoTrim (clockAt 0) HeadPose{yaw = 160 / 6, pitch = 0, roll = 0, tx = 10, ty = 0, tz = 0} newShaper
    checkShowing "lean follows the curved yaw" (show shapedBack) $
        abs (yaw shapedBack - 160) < 1e-9 && abs (tx shapedBack - 10 * cos (deg 160)) < 1e-9 && abs (tz shapedBack + 10 * sin (deg 160)) < 1e-9

    -- Trimming: where the view looks, and where it is, becomes forward,
    -- until recentering.
    let trimmed t y p = fst (shape plain t (clockAt 0) HeadPose{yaw = y, pitch = p, roll = 3, tx = 4, ty = 5, tz = 6} newShaper)
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
    let (trimmedLean, _) = shape viewing (turnedBy 150 0) (clockAt 0) HeadPose{yaw = 10 / 6, pitch = 0, roll = 0, tx = 10, ty = 0, tz = 0} newShaper
    checkShowing "lean follows the trimmed yaw" (show trimmedLean) $
        abs (yaw trimmedLean - 160) < 1e-9 && abs (tx trimmedLean - 10 * cos (deg 160)) < 1e-9 && abs (tz trimmedLean + 10 * sin (deg 160)) < 1e-9
    let craned = Trim HeadPose{yaw = 0, pitch = 30, roll = 0, tx = 0, ty = -40, tz = -80}
        (lookingAway, _) = shape viewing craned (clockAt 0) HeadPose{yaw = 90 / 6, pitch = 0, roll = 0, tx = 0, ty = 0, tz = 0} newShaper
    checkShowing "trimmed movement stays put as the view turns" (show lookingAway) $
        abs (tx lookingAway) < 1e-9 && abs (ty lookingAway + 40) < 1e-9 && abs (tz lookingAway + 80) < 1e-9
    tree properties

-- | Curves are sums and products of a few numbers, right to a few times
-- the rounding of one, 'rounding'; where a property says so, it holds
-- exactly.
properties :: TestTree
properties =
    testGroup
        "properties"
        [ testProperty "a curve takes no movement to none, and is the same both sides of centre" $
            \c (Moved x) ->
                applyCurve c 0 === 0 .&&. applyCurve c (negate x) === negate (applyCurve c x)
        , testProperty "a multiplier's curve never turns back" $
            \e (Moved x) (Moved y) ->
                property (applyCurve (Expo e) (min x y) <= applyCurve (Expo e) (max x y))
        , testProperty "a multiplier's curve reaches the end of the view at its reach, gentler than the multiplier before it" $
            \e (Fraction part) ->
                let x = part * reach e
                    f = applyCurve (Expo e)
                 in Gen.close (rounding 4 (viewRange e)) (f (reach e)) (viewRange e)
                        .&&. property (f x <= factor e * x + rounding 4 (viewRange e))
                        .&&. property (f x >= (1 - precision e / 100) * factor e * x - rounding 4 (viewRange e))
        , testProperty "a multiplier's curve with no precision is the multiplier" $
            \e (Moved x) ->
                Gen.close (rounding 8 (viewRange e + factor e * x)) (applyCurve (Expo e{precision = 0}) x) (factor e * x)
        , testProperty "a curve of points goes through its points, and on past the last along its last line" $
            \(CurvePoints points) (Beyond beyond) ->
                let f = applyCurve (Points points)
                    size = maximum (map (abs . snd) points) + maximum (map fst points)
                    (x0, y0) = last ((0, 0) : init points)
                    (x1, y1) = last points
                    slope = (y1 - y0) / (x1 - x0)
                 in conjoin [Gen.close (rounding 8 (size + abs slope * x1)) (f x) y | (x, y) <- points]
                        .&&. Gen.close (rounding 8 (size + abs slope * (x1 + beyond))) (f (x1 + beyond)) (y1 + beyond * slope)
        , testProperty "a curve of points that go up never turns back, but for rounding" $
            \(Rising points) (Moved x) (Moved y) ->
                let f = applyCurve (Points points)
                 in property (f (min x y) <= f (max x y) + rounding 8 (abs (f (max x y))))
        , testProperty "steadier smoothing smooths a still head more, and more responsive gives way sooner" $
            \(Knob steadiness) (Knob responsiveness) (Knob other) ->
                case (tuned steadiness responsiveness, tuned (max steadiness other) (max responsiveness other)) of
                    (Smoothing cutoff gives, Smoothing steadier sooner) -> property (cutoff > 0 && gives > 0 && steadier <= cutoff && sooner >= gives)
                    _ -> property False
        , testProperty "leaning with the view keeps how far the lean is, and changes nothing else" $
            \(Seated p) ->
                let leant = leanWithView p
                 in Gen.close (rounding 8 600) (sqrt (tx leant ^ (2 :: Int) + tz leant ^ (2 :: Int))) (sqrt (tx p ^ (2 :: Int) + tz p ^ (2 :: Int)))
                        .&&. leant{tx = tx p, tz = tz p} === p
                        .&&. leanWithView p{yaw = 0} === p{yaw = 0}
        , testProperty "a head held still is shaped to the same view in every frame, and with straight curves to itself" $
            \r (Seated p) ->
                let views response = shaped response NoTrim (replicate 5 p)
                 in views r === concat (replicate 5 (take 1 (views r))) .&&. views straight === replicate 5 p
        , testProperty "a jump no head makes is left out for a frame" $
            \(Seated p) (Jump beyond) (Axis axis) ->
                shaped straight NoTrim [p, jumped axis beyond p, p] === [p, p, p]
        , testProperty "no trim is a trim of nothing, and the view trimmed to is where a head at centre looks" $
            \(Seated p) ->
                shaped straight (Trim centre) [p] === shaped straight NoTrim [p]
                    .&&. trimTo p === Trim p
                    .&&. shaped straight (trimTo p) [centre] === [p]
        , testProperty "a trimmed view looks within half a turn either way, and no further than straight up or down" $
            \r (Seated p) (Seated view) ->
                conjoin
                    [ property (yaw out >= -180 && yaw out < 180 && roll out >= -180 && roll out < 180 && pitch out >= -90 && pitch out <= 90)
                    | out <- shaped r (trimTo view{yaw = 3 * yaw view, roll = 3 * roll view, pitch = 3 * pitch view}) [p]
                    ]
        ]
  where
    straight = Response NoSmoothing one one one one one one LeanWithScreen
    one = Points [(1, 1)]
    centre = HeadPose 0 0 0 0 0 0

-- | The rounding of so many sums or products of numbers of a size.
rounding :: Double -> Double -> Double
rounding count size = count * 2 ** (-52) * size

-- | The views of poses seen one after another at 120 frames a second.
shaped :: Response -> Trim -> [HeadPose] -> [HeadPose]
shaped r forward = go newShaper . zip (map clockAt [0, 1 / 120 ..])
  where
    go _ [] = []
    go sh ((t, p) : rest) = let (out, sh') = shape r forward t p sh in out : go sh' rest

-- | A pose with one of its six axes moved by so many times what a head
-- can move it in a frame.
jumped :: Int -> Double -> HeadPose -> HeadPose
jumped axis beyond p = case axis of
    0 -> p{yaw = yaw p + turn}
    1 -> p{pitch = pitch p + turn}
    2 -> p{roll = roll p + turn}
    3 -> p{tx = tx p + move}
    4 -> p{ty = ty p + move}
    _ -> p{tz = tz p + move}
  where
    turn = beyond * inDegrees (headStepTurn world)
    move = beyond * inMillimetres (headStepMove world)

-- | A head's pose from its centre: turned short of half a turn, and of
-- straight up or down, and moved as far as a seated head leans.
-- Simplest at centre, and simpler with an axis there.
newtype Seated = Seated HeadPose
    deriving (Show)

instance Arbitrary Seated where
    arbitrary = Seated <$> (HeadPose <$> choose (-170, 170) <*> choose (-85, 85) <*> choose (-170, 170) <*> choose (-300, 300) <*> choose (-300, 300) <*> choose (-300, 300))

    shrink (Seated pose) = Seated <$> Gen.shrinkParts [centred yaw (\p x -> p{yaw = x}), centred pitch (\p x -> p{pitch = x}), centred roll (\p x -> p{roll = x}), centred tx (\p x -> p{tx = x}), centred ty (\p x -> p{ty = x}), centred tz (\p x -> p{tz = x})] pose
      where
        centred axis with p = [p `with` 0 | axis p /= 0]

-- | A curve's points: inputs going up from above zero, and any outputs;
-- simpler of fewer.
newtype CurvePoints = CurvePoints [(Double, Double)]
    deriving (Show)

instance Arbitrary CurvePoints where
    arbitrary = CurvePoints <$> genPoints
    shrink (CurvePoints points) = CurvePoints <$> shrinkPoints points

-- | A curve's points as 'CurvePoints' has them, the outputs going up from
-- nothing.
newtype Rising = Rising [(Double, Double)]
    deriving (Show)

instance Arbitrary Rising where
    arbitrary = Rising <$> genPoints `suchThat` rising
    shrink (Rising points) = Rising <$> filter rising (shrinkPoints points)

rising :: [(Double, Double)] -> Bool
rising points = and (zipWith (<=) (0 : map snd points) (map snd points))

-- | How far a head has moved from centre along an axis, in its degrees or
-- mm, up to 400.
newtype Moved = Moved Double
    deriving (Show)

instance Arbitrary Moved where
    arbitrary = Moved <$> choose (0, 400)
    shrink (Moved x) = Moved <$> shrinkWithin (0, 400) 0 x

-- | How far beyond a curve's last point, up to 100.
newtype Beyond = Beyond Double
    deriving (Show)

instance Arbitrary Beyond where
    arbitrary = Beyond <$> choose (0, 100)
    shrink (Beyond x) = Beyond <$> shrinkWithin (0, 100) 0 x

-- | Where a knob of the smoothing's is set, from 0 to 100.
newtype Knob = Knob Double
    deriving (Show)

instance Arbitrary Knob where
    arbitrary = Knob <$> choose (0, 100)
    shrink (Knob x) = Knob <$> shrinkWithin (0, 100) 0 x

-- | How many times what a head can move in a frame a jump is, from just
-- over it to ten times.
newtype Jump = Jump Double
    deriving (Show)

instance Arbitrary Jump where
    arbitrary = Jump <$> choose (1.01, 10)
    shrink (Jump times) = Jump <$> shrinkWithin (1.01, 10) 2 times

-- | One of a pose's six axes, by number.
newtype Axis = Axis Int
    deriving (Show)

instance Arbitrary Axis where
    arbitrary = Axis <$> chooseInt (0, 5)
    shrink (Axis axis) = Axis <$> shrinkFrom 0 axis
