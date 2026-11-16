-- | A scorecard of tracking: scenes of a simulated head and clip, with what
-- is true in every frame, and for each what tracking came to, as a few
-- figures to compare one build's tracking with another's.
--
-- See Note [The scorecard].
module Test.Scorecard (simulations) where

import Test.Tasty (TestTree)
import Text.Printf (printf)

import Test.Check
import qualified Test.Old.Scorecard as Old
import qualified Test.Old.Simulation as Old
import Test.Simulation
import TrackKat.Config
import TrackKat.Frame (Stripe)
import TrackKat.Linear
import TrackKat.Pose
import TrackKat.Synth
import TrackKat.Tracker

-- Note [The scorecard]
-- ~~~~~~~~~~~~~~~~~~~~
-- A change to how tracking judges a frame is better or worse only by what
-- tracking then does, and the camera isn't to hand in a test. What is to
-- hand is simulation, where the truth is known in every frame: where the
-- head is, and whether the clip shows at all. Each scene here is tracked
-- from a fresh tracker and summed up ('Card') by what went wrong: frames
-- lost with the whole clip showing, frames tracked with no clip in view,
-- poses far from the truth, poses on the mirror image, the worst and usual
-- errors, and the longest the clip went unfound while it showed.
--
-- Each scene is held to the figures it came to ('Limits'), so that a
-- change that moves one must say so where it is pinned. The limits are
-- bounds, not the figures themselves, as the figures can differ in a
-- frame or two from one machine's arithmetic to another's. With
-- TRACKKAT_SHOW set, each scene's card is printed.
--
-- Each scene is tracked by the old tracker too (see Note [The old
-- tracker] in "TrackKat.Old.Tracker"), and that card held to the figures
-- the old tracker came to ("Test.Old.Scorecard"): the two cards, one
-- after the other, are how the trackers compare.
--
-- The scenes are those the other simulations check one thing of, and
-- those the backlog says tracking gets wrong; what is wrong is then on the
-- card, pinned as it is, until it is put right.

-- | What a frame truly shows of the clip.
data Showing
    = -- | all three reflectors, each lit
      Whole
    | -- | less: a reflector dark or dim, or hidden
      Partly
    | -- | nothing of it
      Absent
    deriving (Eq, Show)

-- | A scene: its name, the clip the tracker is told of, whether still
-- lights are masked, and its frames, each with its time, the true pose,
-- what truly shows, and the stripes.
data Scenario = Scenario String Model Masking [(Double, Pose, Showing, [Stripe])]

-- | What tracking a scene came to.
data Card = Card
    { cardFrames :: Int
    , cardLost :: Int
    -- ^ frames lost with the whole clip showing
    , cardGhosts :: Int
    -- ^ frames tracked with nothing of the clip showing
    , cardWrong :: Int
    -- ^ frames tracked, the clip showing, more than 'wrongDegrees' or
    -- 'wrongMM' from the truth, and not on the mirror image
    , cardMirrored :: Int
    -- ^ frames tracked on the mirror image of the truth
    , cardWorst :: (Double, Double)
    -- ^ the worst error of the poses tracked with the clip showing, off
    -- the mirror image: degrees, mm
    , cardRMS :: Double
    -- ^ the root mean square of those errors, degrees
    , cardUnfound :: Double
    -- ^ the longest, in seconds, from a frame lost with the whole clip
    -- showing to the next frame tracked, or to the scene's end
    }

-- | How far from the truth a pose tracked is wrong: degrees, mm.
wrongDegrees, wrongMM :: Double
wrongDegrees = 5
wrongMM = 30

-- | The most a scene's card may come to: frames lost with the clip
-- showing, tracked with it absent, wrong, and mirrored; the worst error in
-- degrees; and the longest unfound, in seconds.
data Limits = Limits Int Int Int Int Double Double

-- | Track a scene from a fresh tracker: by what tracks frames of (time,
-- true pose, stripes) to each frame's time, true pose, outcome and pose
-- tracked, given whether an outcome is the clip lost.
card :: (Masking -> Model -> [(Double, Pose, [Stripe])] -> [(Double, Pose, o, Maybe Pose)]) -> (o -> Bool) -> Scenario -> Card
card track lost (Scenario _ m masking frames) =
    Card
        { cardFrames = length rows
        , cardLost = length [() | (Whole, (_, _, o, _)) <- shown, lost o]
        , cardGhosts = length [() | (Absent, (_, _, o, _)) <- shown, not (lost o)]
        , cardWrong = length [() | (turn, move) <- errors, turn > wrongDegrees || move > wrongMM]
        , cardMirrored = length mirrored
        , cardWorst = (maximum (0 : map fst errors), maximum (0 : map snd errors))
        , cardRMS = sqrt (sum [turn * turn | (turn, _) <- errors] / fromIntegral (max 1 (length errors)))
        , cardUnfound = longest
        }
  where
    rows = track masking m [(t, pose, stripes) | (t, pose, _, stripes) <- frames]
    shown = zip [s | (_, _, s, _) <- frames] rows
    present = [r | (s, r@(_, _, o, _)) <- shown, not (lost o), s /= Absent]
    mirrored = filter (mirroredOn m) present
    errors = [poseError truth p | r@(_, truth, _, Just p) <- present, not (mirroredOn m r)]
    end = maximum (0 : [t | (t, _, _, _) <- rows])
    -- The longest stretch unfound, and since when the clip has gone
    -- unfound while it wholly showed, if it has.
    (longest, _) = foldl unfound (0, Nothing) shown
    unfound (most, since) (s, (t, _, o, _)) = case (lost o, since) of
        (False, Just began) -> (max most (t - began), Nothing)
        (False, Nothing) -> (most, Nothing)
        (True, Just began) -> (max most (if t >= end then t - began else 0), Just began)
        (True, Nothing)
            | s == Whole -> (most, Just t)
            | otherwise -> (most, Nothing)

-- | A card on a line.
renderCard :: Card -> String
renderCard c =
    printf
        "%d frames: %d lost, %d ghosts, %d wrong, %d mirrored; worst %.2f deg %.1f mm, rms %.3f deg; unfound %.3f s"
        (cardFrames c)
        (cardLost c)
        (cardGhosts c)
        (cardWrong c)
        (cardMirrored c)
        (fst (cardWorst c))
        (snd (cardWorst c))
        (cardRMS c)
        (cardUnfound c)

within :: Limits -> Card -> Bool
within (Limits lost ghosts wrong mirrored worst unfound) c =
    cardLost c <= lost
        && cardGhosts c <= ghosts
        && cardWrong c <= wrong
        && cardMirrored c <= mirrored
        && fst (cardWorst c) <= worst
        && cardUnfound c <= unfound

simulations :: TestTree
simulations = group "Scorecard" $ mapM_ scored scenarios
  where
    scored (scenario@(Scenario name _ _ _), limits) = do
        let c = card (\masking m -> map fst . snd . trackFramesEventsWith masking m) isLost scenario
            old = card (\masking m -> map fst . snd . Old.trackFramesEventsWith masking m) Old.isLost scenario
        checkShowing name (renderCard c) (within limits c)
        checkShowing (name ++ ", by the old tracker") (renderCard old) $
            maybe False (\(a, b, wrong, mirrored, worst, unfound) -> within (Limits a b wrong mirrored worst unfound) old) (lookup name Old.limits)

-- | What shows of a clip lit so: wholly, with each reflector at least
-- half lit.
showing :: Lit -> Showing
showing (l, r, t)
    | all (>= 0.5) [l, r, t] = Whole
    | all (<= 0) [l, r, t] = Absent
    | otherwise = Partly

-- | A scene of a clip of points along a path of (time, true pose, how lit
-- each reflector is), with other discs in each frame at its time.
points :: String -> Model -> (Double -> [Disc]) -> [(Double, Pose, Lit)] -> Scenario
points name m others path =
    Scenario name m (lightMasking defaultConfig) [(t, pose, showing lit, renderDiscs (others t ++ pointDiscs defaultIntrinsics m lit pose)) | (t, pose, lit) <- path]

-- | A scene of a subject's clip along a path, each reflector's centre
-- moved by up to half the given pixels.
tabs :: String -> Subject -> Wear -> Double -> [(Double, Pose, Lit)] -> Scenario
tabs name s@(Subject _ m _) wear amp path =
    Scenario name m (lightMasking defaultConfig) [(t, pose, showing lit, stripes) | ((t, pose, lit), (_, _, stripes)) <- zip path (clipFrames (subjectClip s){clipWear = wear} amp path)]

-- | A scene of given frames, the whole clip showing in each but those at
-- the times that say otherwise.
framed :: String -> Model -> (Double -> Showing) -> [(Double, Pose, [Stripe])] -> Scenario
framed name m showsAt frames = Scenario name m (lightMasking defaultConfig) [(t, pose, showsAt t, stripes) | (t, pose, stripes) <- frames]

-- | With still lights not masked.
unmasked :: Scenario -> Scenario
unmasked (Scenario name m _ frames) = Scenario name m NoMasking frames

nothingElse :: Double -> [Disc]
nothingElse = const []

-- | The scenes, each with the most its card may come to.
scenarios :: [(Scenario, Limits)]
scenarios =
    [ -- Those "Test.Tracker" follows the clip through.
      (points "nodding through where labels by height swap" clipStraight nothingElse [(t, headSeen straightWorn (-4) (-6 * (1 - cos (2 * pi * t / 1.5))) 0, allLit) | t <- at120 5], Limits 0 0 0 0 0.5 0)
    , (points "a jump of 40 degrees" clipBent nothingElse [(t, headSeen bentWorn (if t < 0.5 then 0 else 40) 0 0, allLit) | t <- at120 1.2], Limits 0 0 3 0 46.5 0)
    , (points "the top reflector fading for a moment" clipBent nothingElse [(t, headSeen bentWorn (10 * sin (2 * pi * t / 3)) 0 0, (1, 1, max 0 (min 1 (abs (t - 1.7) / 0.5 - 0.4)))) | t <- at120 3], Limits 0 0 0 0 1 0)
    , (points "the top reflector gone for two seconds" clipBent nothingElse [(t, headSeen bentWorn (5 * sin (2 * pi * t / 3)) 0 0, (1, 1, if t > 1 && t < 3 then 0 else 1)) | t <- at120 4], Limits 0 0 0 0 1 0)
    , (points "a stray reflection while a reflector fades" clipBent (const [((340, 288), 3)]) [(t, headSeen bentWorn (8 * sin (2 * pi * t / 2)) 0 0, (1, if t > 1 && t < 1.6 then 0 else 1, 1)) | t <- at120 2.5], Limits 0 0 0 0 1.6 0)
    , (framed "lights by the camera laid out like the clip" clipBent (\t -> if t < 1 then Whole else Absent) [(t, face, renderDiscs (if t < 1 then pointDiscs defaultIntrinsics clipBent allLit face else phoneAt 190)) | t <- at120 3], Limits 0 0 0 0 0.5 0)
    , (points "leaning in to 270 mm" clipBent nothingElse [(t, leantTo (870 - 50 * max 0 (t - 1)), allLit) | t <- at120 13], Limits 0 0 0 0 0.5 0)
    , (points "sat back half a metre after a break" clipBent nothingElse [(t, leantTo (if t < 1 then 870 else 1370), if t >= 1 && t < 3 then (0, 0, 0) else allLit) | t <- at120 5], Limits 0 0 0 0 0.4 0)
    , (points "a phone in front of the clip in view" clipBent (\t -> if t >= 1 then phoneAt (norm (poseT face) - 400) else []) [(t, face, allLit) | t <- at120 2], Limits 0 0 0 0 0.5 0)
    , (framed "a phone in front of where the clip was" clipBent (\t -> if t < 1 then Whole else Absent) [(t, face, renderDiscs (if t < 1 then pointDiscs defaultIntrinsics clipBent allLit face else phoneAt (norm (poseT face) - 400))) | t <- at120 3], Limits 0 0 0 0 0.5 0)
    , (framed "a reflector in pieces and a fleck by another" clipBent (const Whole) [(t, pose, (if t > 0.5 then breakAt (project defaultIntrinsics pose (mLeft clipBent)) else id) (renderDiscs (fleck pose : pointDiscs defaultIntrinsics clipBent allLit pose))) | t <- at120 2.5, let pose = headSeen bentWorn (8 * sin (2 * pi * t / 2)) 0 0], Limits 0 0 0 0 1.1 0)
    , (points "rolling over until the top reflector isn't highest" clipStraight nothingElse [(t, headSeen straightWorn 0 0 (60 * sin (pi * t / 4)), allLit) | t <- at120 4], Limits 0 0 0 0 1.9 0)
    , (points "turned far, the top reflector running into a lower one" clipBent nothingElse [(t, headSeen bentWorn (90 * t / 8) 0 0, allLit) | t <- at120 8], Limits 0 0 0 0 2.6 0)
    , (points "back, turned, with the top reflector faint" clipBent nothingElse [(t, headSeen bentWorn (min 45 (30 * t)) 0 0, if t > 2 && t < 3.5 then (0, 0, 0) else (1, 1, if t < 3.5 then 1 else 0.5)) | t <- at120 4], Limits 0 0 0 0 0.5 0)
    , (points "a slow sweep, the bent clip" clipBent nothingElse (sweeping bentWorn), Limits 0 0 0 0 2.2 0)
    , (points "a slow sweep, the straight clip" clipStraight nothingElse (sweeping straightWorn), Limits 0 0 0 0 1.8 0)
    , -- Those "Test.Following" weighs following and finding afresh by.
      (framed "a frame of the clip where it was 15 degrees back, in a fast turn" clipBent (const Whole) [(t, fastTurn t, renderDiscs (pointDiscs defaultIntrinsics clipBent allLit (fastTurn (if i == 150 then t - 0.075 else t)))) | (i, t) <- zip [0 :: Int ..] (at120 3)], Limits 0 0 3 0 17.3 0)
    , (points "a leap of 40 degrees while glancing" clipBent nothingElse [(t, if t < 6 then glancing t else headSeen bentWorn (40 + 25 * sin (2 * pi * t / 2.5)) (10 * sin (2 * pi * t / 3.7)) 0, allLit) | t <- at120 9], Limits 0 0 3 0 46 0)
    , (framed "the clip shown elsewhere for three frames" clipBent (const Whole) [(t, glancing t, renderDiscs (pointDiscs defaultIntrinsics clipBent allLit (if i >= 720 && i < 723 then elsewhere else glancing t))) | (i, t) <- zip [0 :: Int ..] (at120 9)], Limits 0 0 5 0 31.3 0)
    , (framed "a frame of the clip turned away, two reflectors' places still lit" clipBent (const Whole) [(t, glancing t, renderDiscs (pointDiscs defaultIntrinsics clipBent allLit (if i == 720 then headSeen bentWorn 40 0 0 else glancing t))) | (i, t) <- zip [0 :: Int ..] (at120 7)], Limits 0 0 3 0 30.1 0)
    , (framed "a light by a reflector gone dark for a moment" clipBent (\t -> if t >= 6 && t < 6.05 then Partly else Whole) [(t, glancing t, renderDiscs (lightBy 0.74 t ++ dim (1, 1, if t >= 6 && t < 6.05 then 0 else 1) (glancing t))) | t <- at120 9], Limits 0 0 8 0 8.3 0)
    , (points "a lamp coming on beside a glancing head" clipBent (\t -> [((500, 100), 14) | t >= 1]) [(t, glancing t, allLit) | t <- at120 4], Limits 0 0 0 0 0.5 0)
    , -- Those "Test.Clips" runs with tape on tabs, and what the backlog
      -- says goes wrong with them.
      (tabs "a slow sweep at 60 frames a second, the user's clip" userTabs userWear 0 (tabSweep userWear), Limits 0 0 0 0 2.1 0)
    , (tabs "a slow sweep at 60 frames a second, the top tab unbent" unbentTabs userWear 0 (tabSweep userWear), Limits 0 0 0 0 2.1 0)
    , (tabs "a slow sweep at 60 frames a second, a scalene clip" scaleneTabs userWear 0 (tabSweep userWear), Limits 0 0 0 0 2.1 0)
    , (tabs "nodding through square-on, the top tab unbent" unbentTabs (wornPitched 30) 0.2 slipping, Limits 0 0 6 115 22.7 0)
    , (tabs "nodding through square-on, the user's clip" userTabs (wornPitched 30) 0.2 slipping, Limits 108 0 12 10 28.5 0.86)
    , (tabs "looking up 30 degrees while turning far, the user's clip" userTabs userWear 0 (lookingUpTurning 75), Limits 0 0 0 0 2.6 0)
    , (points "looking up 30 degrees while turning far, points" clipBent nothingElse [(t, headSeen bentWorn (75 * sin (2 * pi * t / 8)) (-30 * min 1 t) 0, allLit) | t <- at120 8], Limits 0 0 0 0 3.4 0)
    , (tabs "looking up, only the lower pair in view for 20 seconds" userTabs userWear 1 [(t, pose, lit) | t <- framesAt 30 26, let (pose, lit) = lookingUp t], Limits 0 0 32 0 18.1 0)
    , -- What the other simulations don't try.
      (framed "a light as big as a reflector by one gone dark for half a second" clipBent (\t -> if t >= 6 && t < 6.5 then Partly else Whole) [(t, glancing t, renderDiscs (lightBy 1 t ++ dim (1, 1, if t >= 6 && t < 6.5 then 0 else 1) (glancing t))) | t <- at120 9], Limits 0 0 82 0 22.1 0)
    , (points "quick glances, 280 degrees a second, at 60 frames a second" clipBent nothingElse [(t, headSeen bentWorn (35 * sin (2 * pi * t / 0.8)) (8 * sin (2 * pi * t / 1.3)) 0, allLit) | t <- framesAt 60 6], Limits 0 0 0 0 0.6 0)
    , (points "the fastest glances, 550 degrees a second" clipBent nothingElse [(t, headSeen bentWorn (35 * sin (2 * pi * t / 0.4)) 0 0, allLit) | t <- at120 4], Limits 0 0 0 0 0.5 0)
    , (points "a hand passing over each reflector in turn" clipBent nothingElse [(t, glancing t, passing t) | t <- at120 6], Limits 0 0 0 0 3.4 0)
    , (points "seen first among two strays as big as reflectors" clipBent (const [((90, 400), 8), ((560, 90), 9)]) [(t, glancing t, allLit) | t <- at120 3], Limits 0 0 0 0 0.5 0)
    , (points "a still light as big as a reflector while another is hidden for half a second" clipBent (const [((200, 150), 9)]) [(t, glancing t, (1, if t >= 2 && t < 2.5 then 0 else 1, 1)) | t <- at120 5], Limits 0 0 0 0 4.3 0)
    , (points "two reflectors and a stray, the clip not seen before" clipBent (const [((340, 288), 3)]) [(t, face, (1, if t < 1 then 0 else 1, 1)) | t <- at120 2], Limits 0 0 0 0 0.5 0)
    , (unmasked (points "two strays coming on beside a glancing head, lights not masked" clipBent (\t -> [((90, 400), 8) | t >= 1] ++ [((560, 90), 9) | t >= 2]) [(t, glancing t, allLit) | t <- at120 6]), Limits 0 0 0 0 0.7 0)
    , (framed "glancing with centres a pixel out" clipBent (const Whole) [(t, pose, renderDiscs (jitter 2 i (pointDiscs defaultIntrinsics clipBent allLit pose))) | (i, t) <- zip [0 ..] (at120 6), let pose = glancing t], Limits 0 0 0 0 4.1 0)
    , (points "leaving the picture at its edge, and back" clipBent nothingElse [(t, pose, lit) | t <- at120 6, let (pose, lit) = leaving t], Limits 0 0 0 0 0.8 0)
    , (framed "glancing by a lit fire" clipBent (const Whole) [(t, pose, renderDiscs (heads ++ occluded heads (fireDiscs fireAt i))) | (i, t) <- zip [0 ..] (at120 10), let pose = raised (headSeen bentWorn (25 * sin (2 * pi * t / 3)) (8 * sin (2 * pi * t / 5)) 0); heads = pointDiscs defaultIntrinsics clipBent allLit pose], Limits 0 0 0 0 0.5 0)
    , (unmasked (framed "glancing by a lit fire, lights not masked" clipBent (const Whole) [(t, pose, renderDiscs (heads ++ occluded heads (fireDiscs fireAt i))) | (i, t) <- zip [0 ..] (at120 10), let pose = raised (headSeen bentWorn (25 * sin (2 * pi * t / 3)) (8 * sin (2 * pi * t / 5)) 0); heads = pointDiscs defaultIntrinsics clipBent allLit pose]), Limits 0 0 0 0 0.5 0)
    ]
  where
    face = headSeen bentWorn 0 0 0
    glancing t = headSeen bentWorn (25 * sin (2 * pi * t / 2.5)) (10 * sin (2 * pi * t / 3.7)) 0
    userWear = wornPitched (-23)
    -- A phone's lights laid out like the clip, so far from the camera
    -- along the line of sight to the face.
    phoneAt d = [(project defaultIntrinsics (Pose (poseR face) (d *^ normalize (poseT face))) p, 5) | p <- [mLeft clipBent, mRight clipBent, mTop clipBent]]
    leantTo d = let Pose r p = face in Pose r (d *^ normalize p)
    fleck pose = let (u, v) = project defaultIntrinsics pose (mTop clipBent) in ((u + 4, v + 9), 2.5)
    sweeping wornAt = [(t, headSeen wornAt (40 * sin (2 * pi * t / 7)) (25 * sin (2 * pi * t / 5.3)) (20 * sin (2 * pi * t / 4.1)), allLit) | t <- at120 20]
    tabSweep wear = [(t, headPose userCamera wear (40 * sin (2 * pi * t / 7)) (25 * sin (2 * pi * t / 5.3)) (20 * sin (2 * pi * t / 4.1)), allLit) | t <- framesAt 60 20]
    fastTurn t = headSeen bentWorn (-40 + 200 * max 0 (min 0.4 (t - 1))) 0 0
    elsewhere = let Pose r p = headSeen bentWorn 40 0 0 in Pose r (p ^+^ V3 150 0 0)
    -- Dim reflectors, half their usual width, and a light coming on at
    -- six seconds 8 pixels from where the top one then is, so many times
    -- its width.
    dim lit pose = [(p, 0.5 * r) | (p, r) <- pointDiscs defaultIntrinsics clipBent lit pose]
    lightBy width t = let (topAt, topWidth) = dim allLit (glancing 6) !! 2 in [((fst topAt + 8, snd topAt), width * topWidth) | t >= 6]
    lookingUpTurning far = [(t, headPose userCamera userWear (far * sin (2 * pi * t / 8)) (-30 * min 1 t) 0, allLit) | t <- at120 8]
    lookingUp t
        | t < 2 = (raised (headPose userCamera userWear 0 0 0), allLit)
        | t < 22 = (raised (headPose userCamera userWear 5 (-25) 0), (1, 1, 0))
        | otherwise = (raised (headPose userCamera userWear (glance 0 (t - 22)) 0 0), allLit)
    -- Each reflector dark for a third of a second, one after another,
    -- from one second on.
    passing t = (hidden 1, hidden 2, hidden 3)
      where
        hidden n = if t >= n && t < n + 0.33 then 0 else 1 :: Double
    -- The head moving aside until the clip is out of the picture, at a
    -- quarter of a metre a second, and back.
    leaving t =
        let aside = 250 * (if t < 3 then max 0 (t - 0.5) else max 0 (5.5 - t))
            pose@(Pose r p) = face
            moved = Pose r (p ^+^ V3 aside 0 0)
            inPicture = and [u >= 8 && u <= 632 | which <- [mLeft clipBent, mRight clipBent, mTop clipBent], let (u, _) = project defaultIntrinsics moved which]
         in if aside <= 0 then (pose, allLit) else (moved, if inPicture then allLit else (0, 0, 0))
