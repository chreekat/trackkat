-- | That the domains properties are about are made simpler as Note
-- [Simplest first] in "Test.Gen" says: each of those several subjects
-- share, by what 'shrinksWell' checks.
module Test.Shrink (tests, shrinksWell) where

import Data.Foldable (toList)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Test.Gen
import Test.Instances ()
import Test.Simulation (headset)
import TrackKat.Belief (Belief (..), chance, unknown)
import TrackKat.Branch (Clarity (..))
import TrackKat.CommandLine (Start (..))
import TrackKat.Config
import TrackKat.Frame (Blob (..), Stripe (..), frameHeight, frameWidth)
import TrackKat.Idle (Use (..))
import TrackKat.Linear (V3 (..), norm)
import TrackKat.Match (Kind (..), Sight (..), Size (..), Three (..), noSize)
import TrackKat.Protocol (Command (..))
import TrackKat.Response
import TrackKat.ServedGame (HeadTracking (..), ServedGame (..))
import TrackKat.Term (LogOdds (..), Score (..), impossible, possible)
import TrackKat.Tracker.Outcome (Way (..))
import TrackKat.Updater (Looking (..))
import TrackKat.World (clockAt, seconds)

-- | That a domain's values are made simpler as Note [Simplest first] in
-- "Test.Gen" says, given its simplest value and what tells its values
-- from others: values made up are of the domain, and so is all they
-- shrink to; none shrinks to itself; one that isn't the simplest shrinks
-- to that first, and the simplest to nothing; and going on to a value's
-- last shrink, forty times over, never comes back to a value. A value
-- these fail of is told as it was made up: making it simpler would go by
-- the shrink in question.
shrinksWell :: (Arbitrary a, Eq a, Show a) => String -> a -> (a -> Bool) -> TestTree
shrinksWell name simplest inDomain =
    testGroup
        name
        [ law "a value made up, and all it shrinks to, is of the domain" $
            \x -> conjoin [counterexample (show y) (inDomain y) | y <- x `asTypeOf` simplest : shrink x]
        , law "a value never shrinks to itself" $
            \x -> counterexample (show (shrink x)) (x `asTypeOf` simplest `notElem` shrink x)
        , law "a value shrinks first to the simplest, which shrinks to nothing" $
            \x -> take 1 (shrink x) === [simplest | x /= simplest]
        , law "shrinking to the last of a value's shrinks never comes back to a value" $
            \x ->
                let walk = take 40 (lasts (x `asTypeOf` simplest))
                 in counterexample (show walk) (and [a `notElem` later | (a, later) <- zip walk (drop 1 (iterate (drop 1) walk))])
        ]
  where
    law about holds = testProperty about (noShrinking . holds)
    lasts x =
        x : case shrink x of
            [] -> []
            steps -> lasts (last steps)

tests :: TestTree
tests =
    testGroup
        "Shrink"
        [ shrinksWell "a fraction" (Fraction 0) (\(Fraction x) -> x >= 0 && x <= 1)
        , shrinksWell "a gap between frames" (FrameGap (seconds (1 / 120))) (\(FrameGap dt) -> dt >= seconds (1 / 120) && dt <= seconds 0.5)
        , shrinksWell "a turn" (Turn (V3 0 0 0)) (\(Turn w) -> turnOf 180 w)
        , shrinksWell "a rigid motion" (Placing (V3 0 0 0) (V3 0 0 0)) (\(Placing turn at) -> turnOf 180 turn && upTo 2000 at)
        , shrinksWell "a pose seen whole" (SeenWhole (squarelyAhead trackedRange)) (\(SeenWhole placing) -> seen 60 trackedRange placing)
        , shrinksWell "a pose with the reflectors apart, facing within 20 degrees" (Apart20 (squarelyAhead (400, 1500))) (\(Apart20 placing) -> seen 20 (400, 1500) placing && reflectorsApart headset placing)
        , shrinksWell "a pose with the reflectors apart, facing within 30 degrees" (Apart30 (squarelyAhead (400, 1500))) (\(Apart30 placing) -> seen 30 (400, 1500) placing && reflectorsApart headset placing)
        , shrinksWell "a place in the picture" (InPicture (0, 0)) (\(InPicture place) -> inPicture place)
        , shrinksWell "a stray blob" (Stray (roundBlob (0, 0) 6)) (\(Stray b) -> blobOf 400 b)
        , shrinksWell "stray blobs" (Strays []) (\(Strays blobs) -> all (blobOf 400) blobs)
        , shrinksWell "the reflectors' blobs" (ReflectorBlobs (pure (roundBlob (0, 0) 6))) (\(ReflectorBlobs blobs) -> all (blobOf 500) blobs)
        , shrinksWell "the reflectors' sizes" (Sizes (pure noSize)) (\(Sizes sizes) -> sizes == pure noSize || all (\s -> sizeLatest s >= 1 && sizeLatest s <= 3000 && sizeUsual s >= 0.05 * sizeLatest s && sizeUsual s <= 2 * sizeLatest s) sizes)
        , shrinksWell "the reflectors' sights" (ReflectorSights (pure (Sight (0, 0) 6 Whole))) (\(ReflectorSights sights) -> all (sightOf 500) sights)
        , shrinksWell "settings a file can say" (Sayable defaultConfig{recenterBindings = [], pauseBindings = []}) (\(Sayable c) -> parseConfig (renderConfig c) == Right c)
        , shrinksWell "a vector" (V3 0 0 0) (upTo 1000)
        , shrinksWell "a moment" (clockAt 0) (\at -> at >= clockAt 0 && at <= clockAt 86400)
        , shrinksWell "a length of time" (seconds 0) (\d -> d >= seconds 0 && d <= seconds 86400)
        , shrinksWell "three of something" (pure (V3 0 0 0)) (all (upTo 1000) . toList :: Three V3 -> Bool)
        , shrinksWell "a blob" (roundBlob (0, 0) 6) (blobOf 5000)
        , shrinksWell "a sight" (Sight (0, 0) 6 Whole) (sightOf 5000)
        , shrinksWell "a binding" (Key [] "F1") binding
        , shrinksWell "a command" (Cmd1 0) (const True)
        , shrinksWell "a stripe" (Stripe 0 0 1 0 0) $ \(Stripe line start points sumX brightness) ->
            and [line >= 0, line < frameHeight, start >= 0, points >= 1, start + points <= frameWidth, sumX >= 0, sumX <= 0xFFFFF, brightness >= 0, brightness <= 0x7FFF]
        , shrinksWell "a belief" unknown $ \(Belief tt tm mt mm) ->
            tt > 0 && all (>= 0) [tm, mt, mm] && abs (tt + tm + mt + mm - 1) < 1e-9
        , shrinksWell "a chance" (chance 0) (\p -> p >= chance 0 && p <= chance 1)
        , shrinksWell "log odds" (LogOdds 0) (\(LogOdds x) -> abs x <= 30)
        , shrinksWell "a score of what can be" (Likely possible) (\(Likely scored) -> scored <= possible && scored >= Score (-60))
        , shrinksWell "a score" possible (\scored -> scored == impossible || (scored <= possible && scored >= Score (-60)))
        , shrinksWell "how trackkat was started" ByHand (const True)
        , shrinksWell "whether trackkat is used" Used (const True)
        , shrinksWell "a multiplier" (Multiplier 1 0 yawRange) multiplier
        , shrinksWell "a curve" (Points [(1, 1)]) curve
        , shrinksWell "whether aiming is asked for" NotAiming (const True)
        , shrinksWell "aiming" unaimed aiming
        , shrinksWell "a response" (Response NoSmoothing straight straight straight straight straight straight LeanWithScreen unaimed) $ \r ->
            all curve [yawCurve r, pitchCurve r, rollCurve r, xCurve r, yCurve r, zCurve r] && smooth (smoothing r) && aiming (aim r)
        , shrinksWell "a view of the head" VirtualView (const True)
        , shrinksWell "a game served" (ServedGame 1 Nothing 0 Nothing TrackingStopped) (\g -> servedProcess g >= 1 && maybe True (\profile -> profile >= 0 && profile <= 65535) (servedProfile g))
        , shrinksWell "why an update is looked for" Due (const True)
        , shrinksWell "how clearly the reflectors show" Clear (const True)
        , shrinksWell "a way the clip was made out" Followed (const True)
        ]
  where
    straight = Points [(1, 1)]
    unaimed = Aim NoSmoothing straight straight straight
    aiming a = all curve [aimYaw a, aimPitch a, aimRoll a] && smooth (aimSmoothing a)
    smooth NoSmoothing = True
    smooth (Smoothing cutoff b) = cutoff >= 0.2 - 1e-9 && cutoff <= 20 && b >= 0.001 && b <= 1 + 1e-9
    turnOf most w = degreesOf w <= most + 1e-9
    upTo size (V3 x y z) = all ((<= size) . abs) [x, y, z]
    seen most (nearest, farthest) placing@(Placing turn at) = turnOf most turn && norm at >= nearest - 1e-9 && norm at <= farthest + 1e-9 && inView headset placing
    inPicture (u, v) = u >= 0 && u <= fromIntegral frameWidth && v >= 0 && v <= fromIntegral frameHeight
    blobOf most b = b == roundBlob (blobX b, blobY b) (blobPoints b) && blobPoints b >= 6 && blobPoints b <= most && inPicture (blobX b, blobY b)
    sightOf most s = sightPixels s >= 6 && sightPixels s <= most && inPicture (sightAt s)
    binding b = case b of
        Key _ _ -> Just b `elem` [bindingFromCodes mask code | mask <- [0 .. 15], code <- [0 .. 255]]
        JoystickButton (Joystick name index) button -> not (null (words name)) && index >= 1 && index <= 4 && button >= 1 && button <= maxButton
    multiplier e = factor e >= 0.1 && factor e <= 20 && precision e >= 0 && precision e <= 100 && viewRange e `elem` [yawRange, pitchRange, rollRange, leanRange]
    curve (Expo e) = multiplier e
    curve (Points points) = not (null points) && and (zipWith (<) (0 : map fst points) (map fst points)) && all ((<= 200) . abs . snd) points
