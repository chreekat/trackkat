-- | The two trackers behind one (see Note [Two trackers] in
-- "TrackKat.Trackers"): each tracks through it as it does on its own, and
-- one takes over from the other with what the user would miss.
module Test.Trackers (tests, simulations) where

import Control.Exception (evaluate)
import Data.List (mapAccumL)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import Test.Tasty.QuickCheck

import Test.Check
import Test.Gen (shrinkParts, shrinkTo)
import Test.Instances ()
import Test.Memory (liveWhileStepping)
import Test.Simulation
import TrackKat.Calibration
import TrackKat.Config
import TrackKat.Frame (Stripe)
import TrackKat.Linear
import qualified TrackKat.Old.Tracker as Old
import TrackKat.Pose
import TrackKat.Recording (resettled, trackerFor)
import TrackKat.Synth
import qualified TrackKat.Tracker as New
import TrackKat.Trackers
import TrackKat.World (Instant, clockAt)

tests :: TestTree
tests =
    testGroup
        "Trackers"
        [ testProperty "a tracker is the one the settings ask for, of their clip, from the calibration given" $
            \which ->
                let t = trackerFor defaultIntrinsics (asking which) defaultCalibration
                 in whichTracker t === which
                        .&&. trackerModel t === clipModel (clip defaultConfig)
                        .&&. renderCalibration (trackerCalibration t) === renderCalibration defaultCalibration
        , testProperty "through this, each tracker makes of every frame what it makes of it on its own" $
            \(Reached which frames asks) ->
                let through = snd (mapAccumL (\t (at, stripes) -> let (t', o, evs) = step at stripes t in (t', (made o, map show evs, show (trackerPose t'), show (trackerCentre t')))) (asked asks (fresh which)) (timed frames))
                    old = snd (mapAccumL (\t (at, stripes) -> let (t', o, evs) = Old.step at stripes t in (t', (madeOld o, map show evs, show (Old.trackerPose t'), show (Old.trackerCentre t')))) (foldl (flip Old.request) (Old.newTracker defaultIntrinsics model MaskLights defaultCalibration) (map oldAsk asks)) (timed frames))
                    new = snd (mapAccumL (\t (at, stripes) -> let (t', o, evs) = New.step at stripes t in (t', (madeNew o, map show evs, show (New.trackerPose t'), show (New.trackerCentre t')))) (foldl (flip New.request) (New.newTracker defaultIntrinsics model MaskLights defaultCalibration) asks) (timed frames))
                 in through === (case which of OldTracker -> old; NewTracker -> new)
        , testProperty "taken to settings that ask for the tracker it is, a tracker tracks on as it would have" $
            \(Reached which frames asks) ->
                let t = reached which frames asks
                 in probed (length frames) (resettled defaultIntrinsics (asking which) (asking which) t) === probed (length frames) t
        , testProperty "taken to settings that ask for the other tracker, that one takes over: with the calibration and the center the first had, and what the first was still asked for carried out on the clip next seen" $
            \(Reached which frames asks) ->
                let t = reached which frames asks
                    taken = resettled defaultIntrinsics (asking which) (asking (other which)) t
                    carriedOut tracker = [e | (_, events) <- probed (length frames) tracker, e <- events, e `elem` ["Recentered", "CalibrationStarted"]]
                 in whichTracker taken === other which
                        .&&. renderCalibration (trackerCalibration taken) === renderCalibration (trackerCalibration t)
                        .&&. trackerModel taken === trackerModel t
                        .&&. show (trackerCentre taken) === show (trackerCentre t)
                        .&&. carriedOut taken === carriedOut t
        ]
  where
    made o = case o of
        Tracked{outPose = hp, outDistance = d, outFit = fit, outSeen = seen} -> Just (hp, d, fit, show seen)
        Lost{} -> Nothing
    madeOld o = case o of
        Old.Tracked{Old.outPose = hp, Old.outDistance = d, Old.outFit = fit, Old.outSeen = seen} -> Just (hp, d, fit, show seen)
        Old.Lost{} -> Nothing
    madeNew o = case o of
        New.Tracked{New.outPose = hp, New.outDistance = d, New.outFit = fit, New.outSeen = seen} -> Just (hp, d, fit, show seen)
        New.Lost{} -> Nothing
    oldAsk Recenter = Old.Recenter
    oldAsk Calibrate = Old.Calibrate

simulations :: TestTree
simulations = group "Trackers" $ do
    -- A head glancing about, recentered on while turned away, and then
    -- tracked by the other tracker from three seconds in.
    let glancing t = headSeen bentWorn (20 + 25 * sin (2 * pi * t / 2.5)) (10 * sin (2 * pi * t / 3.7)) 0
        frames = [(t, renderDiscs (pointDiscs defaultIntrinsics clipBent allLit (glancing t))) | t <- at120 6]
        tracking which = newTracker which defaultIntrinsics clipBent MaskLights defaultCalibration
        tookOver from = snd (mapAccumL frame (tracking from) frames)
          where
            frame t (time, stripes) =
                let before = if time == 3 then takeOver t (tracking (other from)) else t
                    (t', o, evs) = step (clockAt time) stripes before
                 in (t', (time, o, evs, whichTracker t'))
        alone which = snd (mapAccumL (\t (time, stripes) -> let (t', o, _) = step (clockAt time) stripes t in (t', (time, o))) (tracking which) frames)
        -- The furthest the view is, after the other tracker takes over,
        -- from what that tracker makes of the same frames alone, centered
        -- where the first was: degrees of yaw and pitch.
        apart from =
            maximum
                ( 0
                    : [ max (abs (yaw hp - yaw alonePose)) (abs (pitch hp - pitch alonePose))
                      | ((time, Tracked{outPose = hp}, _, _), (_, Tracked{outPose = alonePose})) <- zip (tookOver from) (alone (other from))
                      , time >= 3
                      ]
                )
        lostAfter from = length [() | (time, Lost{}, _, _) <- tookOver from, time >= 3]
        recentersAfter from = length [() | (time, _, evs, _) <- tookOver from, time >= 3, Recentered <- evs]
    sequence_
        [ do
            checkShowing ("the " ++ name ++ " tracker taking over tracks from the frame it takes over on, the view centered where it was") (show (lostAfter from, recentersAfter from, apart from)) $
                lostAfter from == 0 && recentersAfter from == 0 && apart from < 0.5
            check ("and is the tracker from then on, the " ++ name) $
                and [which == (if time < 3 then from else other from) | (time, _, _, which) <- tookOver from]
        | (name, from) <- [("new", OldTracker), ("old", NewTracker)]
        ]
    tree . testCase "tracking through this holds on to no more memory than the tracker alone does, the old one or the new" $ do
        -- What tracking evaluates once and keeps, whichever tracks.
        _ <- holding (stepping step) (tracking OldTracker) 200
        _ <- holding (stepping step) (tracking NewTracker) 200
        old <- holding (stepping Old.step) (Old.newTracker defaultIntrinsics clipBent MaskLights defaultCalibration) 6000
        throughOld <- holding (stepping step) (tracking OldTracker) 6000
        new <- holding (stepping New.step) (New.newTracker defaultIntrinsics clipBent MaskLights defaultCalibration) 6000
        throughNew <- holding (stepping step) (tracking NewTracker) 6000
        assertBool (show [("old", old, throughOld), ("new", new, throughNew)]) $
            throughOld - old < 2000 && throughNew - new < 2000
  where
    -- A head turning left and right by a lit fire, as "Test.Memory" has
    -- it.
    fireLit i = renderDiscs (heads ++ occluded heads (fireDiscs fireAt i))
      where
        t = fromIntegral i / 120 :: Double
        heads = pointDiscs defaultIntrinsics clipBent allLit (raised (headSeen bentWorn (25 * sin (2 * pi * t / 3)) (8 * sin (2 * pi * t / 5)) 0))
    -- Track the i-th frame, as the app does, keeping only the tracker.
    stepping track i t = do
        let (t', o, evs) = track (clockAt (fromIntegral i / 120)) (fireLit i) t
        _ <- evaluate (length (show o) + length evs)
        pure t'
    -- The bytes of memory in use once so many frames have been tracked.
    holding next t0 frames = sum <$> liveWhileStepping next t0 [frames]

-- | The default settings, asking for a tracker.
asking :: WhichTracker -> Config
asking which = defaultConfig{chosenTracker = which}

other :: WhichTracker -> WhichTracker
other OldTracker = NewTracker
other NewTracker = OldTracker

-- | The clip the default settings give.
model :: Model
model = clipModel (clip defaultConfig)

-- | A tracker of the default settings' clip, with nothing learnt.
fresh :: WhichTracker -> Tracker
fresh which = trackerFor defaultIntrinsics (asking which) defaultCalibration

asked :: [Request] -> Tracker -> Tracker
asked asks t = foldl (flip request) t asks

-- | What the camera shows in a frame.
data Shown
    = -- | the clip, the head turned by this yaw and pitch, in degrees
      Head Int Int
    | -- | nothing
      Dark
    deriving (Eq, Show)

-- | Frames 120 a second from time 0, each with its time.
timed :: [Shown] -> [(Instant, [Stripe])]
timed frames = [(clockAt (fromIntegral i / 120), stripesOf shown) | (i, shown) <- zip [0 :: Int ..] frames]

stripesOf :: Shown -> [Stripe]
stripesOf Dark = []
stripesOf (Head y p) = concatMap (squareStripes . project defaultIntrinsics pose) [mLeft model, mRight model, mTop model]
  where
    pose = Pose (rotYXZ (fromIntegral y) (fromIntegral p) 0) (V3 0 0 700)

-- | A tracker of the default settings' clip that has tracked frames, and
-- then been asked for things.
reached :: WhichTracker -> [Shown] -> [Request] -> Tracker
reached which frames asks = asked asks (foldl (\t (at, stripes) -> let (t', _, _) = step at stripes t in t') (fresh which) (timed frames))

-- | What a tracker that has tracked so many frames makes of the clip held
-- facing the camera for the next few: each frame's pose, if it is
-- tracked, and what happened.
probed :: Int -> Tracker -> [(Maybe HeadPose, [String])]
probed tracked t0 = snd (mapAccumL shown t0 (drop tracked (timed (replicate (tracked + 3) (Head 0 0)))))
  where
    shown t (at, stripes) =
        let (t', o, evs) = step at stripes t
         in (t', (case o of Tracked{outPose = hp} -> Just hp; Lost{} -> Nothing, map (takeWhile (/= ' ') . show) evs))

-- | A tracker of either kind by what it has been through: the frames it
-- has tracked, a head turning a little way from one to the next among
-- them, and what it was then asked for. Simplest the old tracker, having
-- tracked nothing and been asked for nothing; simpler of fewer frames,
-- showing less, and of less asked for.
data Reached = Reached WhichTracker [Shown] [Request]
    deriving (Eq, Show)

instance Arbitrary Reached where
    arbitrary =
        Reached
            <$> arbitrary
            <*> scale (`div` 3) (listOf (frequency [(6, Head <$> chooseInt (-8, 8) <*> chooseInt (-5, 5)), (1, pure Dark)]))
            <*> scale (`div` 20) (listOf (elements [Recenter, Calibrate]))
    shrink =
        shrinkParts
            [ \(Reached which frames asks) -> [Reached w frames asks | w <- shrink which]
            , \(Reached which frames asks) -> [Reached which fs asks | fs <- shrinkList (shrinkTo Dark (\shown -> [Head 0 0 | shown /= Head 0 0])) frames]
            , \(Reached which frames asks) -> [Reached which frames as | as <- shrinkList (const []) asks]
            ]
