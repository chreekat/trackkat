-- | Still lights: a fire or a lamp learnt and masked, and the clip's own
-- reflectors not taken for one.
module Test.Lights (tests, simulations) where

import Control.Monad (forM_)
import Data.List (inits, mapAccumL, sort, sortOn, tails, (\\))
import Data.Maybe (fromMaybe, isNothing)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Test.Check
import Test.Gen (earlier, shrinkFrom, shrinkParts, shrinkPlace, shrinkTo)
import Test.Simulation
import TrackKat.Frame
import TrackKat.Lights
import TrackKat.Linear
import TrackKat.Match (Kind (..), Sight (..))
import TrackKat.Pose
import TrackKat.Synth
import TrackKat.Tracker
import TrackKat.World (clockAt, inSeconds)

simulations :: TestTree
simulations = group "Lights" $ do
    -- A lit fireplace behind the user, as the camera saw one: a steady core
    -- and flickering flames around it. Finding the clip afresh needs
    -- exactly three big blobs, and the fire's core is always a fourth;
    -- learnt as a still light and masked, it no longer stops that.
    let lit seen = if seen then allLit else (0, 0, 0)
        -- (time, head, reflectors lit, fire lit) to rendered frames
        withFire place rows =
            [ (t, pose, renderDiscs (heads ++ (if fire then occluded heads (fireDiscs place i) else [])))
            | (i, (t, pose, l, fire)) <- zip [0 :: Int ..] rows
            , let heads = pointDiscs defaultIntrinsics clipBent l pose
            ]
        firstTracked from rows = case [t | (t, _, Tracked{}, _) <- rows, t >= from] of
            t : _ -> Just t
            [] -> Nothing
        ignored rows = [p | (_, evs) <- rows, LightIgnored p _ <- evs]
        forgotten rows = [(t, p) | ((t, _, _, _), evs) <- rows, LightForgotten p <- evs]
        moved rows = [(t, p) | ((t, _, _, _), evs) <- rows, LightMoved p _ <- evs]
        nearFire p = distance p fireAt < 10
        coldFrames = withFire fireAt [(t, fidgeting t, allLit, True) | t <- at120 8]
        coldWith masking = trackFramesEventsWith masking clipBent coldFrames
        (coldTracker, coldRows) = coldWith MaskLights
        cold = map fst coldRows
        coldStart = firstTracked 0 cold
        coldUnmasked = firstTracked 0 (map fst (snd (coldWith NoMasking)))
    checkShowing "the clip where the user's camera saw it" (show (map (project defaultIntrinsics (fidgeting 0)) [mLeft clipBent, mRight clipBent, mTop clipBent])) True
    checkShowing "with the fire lit, the clip found" (show coldStart ++ " s after a cold start, ignoring " ++ show (ignored coldRows)) $
        maybe False (< 3.5) coldStart && all nearFire (ignored coldRows) && length (ignored coldRows) == 1
    checkShowing "then followed" (show (summed (after 3.5 cold))) $ lostCount (after 3.5 cold) == 0 && fst (worstError cold) < 1.5
    checkShowing "the masked fire reported" (show (take 1 (seens (after 3.5 cold)))) $ all ((> 0) . seenMasked) (seens (after 3.5 cold))
    checkShowing "without the mask, never found" (show coldUnmasked) $ isNothing coldUnmasked
    let roles = [(role, distance (blobX b, blobY b) fireAt < 30) | (b, role) <- trackerBlobs coldTracker]
    checkShowing "the last frame's blobs: the reflectors, and the fire masked" (show roles) $
        length [() | (ReflectorBlob, _) <- roles] == 3 && and [role == MaskedBlob | (role, True) <- roles] && all (\(role, byFire) -> byFire || role == ReflectorBlob) roles

    -- Lost for a second and a half with the fire lit, and found again.
    let refinding = trackFramesEvents clipBent (withFire fireAt [(t, fidgeting t, lit (t < 5 || t > 6.5), True) | t <- at120 8])
        refound = fmap (subtract 6.5) (firstTracked 6.5 (map fst (snd refinding)))
    checkShowing "found again with the fire lit after" (show refound ++ " s") $ maybe False (< 0.05) refound

    -- The head passes in front of the fire, hiding it, and over its area:
    -- the reflectors there are not masked.
    let passPose t = leaning (max 0 (min 1 (min (t - 5) (9 - t))))
        leaning f = let Pose r p = raised (headSeen bentWorn 0 0 0) in Pose r (p ^+^ V3 (-130 * f) 0 0)
        passFire = project defaultIntrinsics (leaning 1) (mLeft clipBent)
        (_, passRows) = trackFramesEvents clipBent (withFire passFire [(t, passPose t, allLit, True) | t <- at120 10.5])
        passed = map fst passRows
        passIgnored = [p | ((t, _, _, _), evs) <- passRows, t < 5, LightIgnored p _ <- evs]
    checkShowing "the fire at" (show passFire ++ " learnt before the head passes " ++ show passIgnored) $
        length passIgnored == 1 && all (\p -> distance p passFire < 10) passIgnored
    checkShowing "the head passing in front of the fire" (show (summed (after 4 passed), maximum (0 : map (length . seenMissing) (seens (after 4 passed))))) $
        lostCount (after 4 passed) == 0 && all (null . seenMissing) (seens (after 4 passed)) && fst (worstError (after 4 passed)) < 1.5
    checkShowing "the fire not taken to have moved with the head" (show (moved passRows, forgotten passRows)) $ null (moved passRows) && null (forgotten passRows)

    -- The fire goes out: it is forgotten after a while.
    let (_, outRows) = trackFramesEvents clipBent (withFire fireAt [(t, fidgeting t, (0, 0, 0), t < 5) | t <- at30 60])
    checkShowing "the fire learnt alone, then forgotten after it goes out" (show (ignored outRows, forgotten outRows)) $
        map nearFire (ignored outRows) == [True] && case forgotten outRows of
            [(t, p)] -> nearFire p && t > 45 && t < 55
            _ -> False
    -- Before it was learnt, the fire and its flames were at times taken
    -- for the clip, as three big blobs; masked, never.
    checkShowing "the fire masked isn't taken for the clip" (show [(t, s) | ((t, _, Tracked{outSeen = s}, _), _) <- outRows, t > 3]) $ all isLost [o | ((t, _, o, _), _) <- outRows, t > 3.5]
    let (_, unmaskedRows) = trackFramesEventsWith NoMasking clipBent (withFire fireAt [(t, fidgeting t, (0, 0, 0), True) | t <- at30 10])
    check "without the mask, nothing learnt or masked" $
        null (ignored unmaskedRows) && and [m == 0 | ((_, _, Lost{outMasked = m}, _), _) <- unmaskedRows]

    -- A head held still for a minute by the fire, from a cold start: its
    -- reflectors, still with the fire for as long as it takes to learn it,
    -- are learnt with it but found in it at once, and not announced; then,
    -- being matched, they are never masked.
    let (_, stillRows) = trackFramesEvents clipBent (withFire fireAt [(t, raised (headSeen bentWorn 0 0 0), allLit, True) | t <- at30 60])
        stilled = map fst stillRows
    checkShowing "a still head isn't masked" (show (summed stilled, ignored stillRows)) $
        lostCount stilled <= 93 && lostCount (after 3.5 stilled) == 0 && map nearFire (ignored stillRows) == [True]

    -- The clip's own reflectors aren't learnt as lights, however long the
    -- head is held still with some of them out of sight or with the clip
    -- tracked wrongly: looking down at a phone with only the lower pair or
    -- one reflector in view, lost and then showing one alone, held still
    -- while tracked, and held still on the mirror image. Each centroid
    -- wanders by up to half a pixel. Afterwards, glancing about with all
    -- three in view, the clip is found and followed.
    let lookingDown = raised (headSeen bentWorn 5 (-25) 0)
        glancingAt t = raised (headSeen bentWorn (glance 0 t) 0 0)
        -- (what is in view, how long it is held, the head and reflectors
        -- lit meanwhile, whether the clip was tracked before)
        holds =
            [ ("lost, then the lower pair in view", 60, \t -> (lookingDown, if t < 5 then (0, 0, 0) else (1, 1, 0)), True)
            , ("the top reflector in view", 60, \_ -> (lookingDown, (0, 0, 1)), True)
            , ("lost, then one reflector alone", 60, \t -> (lookingDown, if t < 5 then (0, 0, 0) else (1, 0, 0)), True)
            , ("all three tracked", 60, \_ -> (fidgeting 0, allLit), True)
            ]
        heldScene held holdingAt trackedBefore =
            [ (t, pose, l)
            | t <- at30 (held + 6)
            , let (pose, l)
                    | trackedBefore && t < 2 = (fidgeting t, allLit)
                    | t < held + 2 = holdingAt t
                    | otherwise = (glancingAt (t - held - 2), allLit)
            ]
    forM_ holds $ \(name, held, holdingAt, trackedBefore) -> do
        let (_, heldRows) = trackNoisy clipBent 1 (heldScene held holdingAt trackedBefore)
            back = after (held + 3) (map fst heldRows)
        checkShowing ("held still for " ++ show (round held :: Int) ++ " s with " ++ name ++ ", no light learnt") (show (ignored heldRows, forgotten heldRows)) $
            null (ignored heldRows)
        checkShowing ("held still with " ++ name ++ ", and afterwards followed") (show (summed back)) $ lostCount back == 0 && fst (worstError back) < 1.5
    let (_, mirrorRows) = trackNoisy clipBent 1 [(t, headSeen 60 0 0 0, allLit) | t <- at30 60]
    checkShowing "held still on the mirror image" (show (length (filter isMirrored (map fst mirrorRows))) ++ ", no light learnt " ++ show (ignored mirrorRows)) $
        null (ignored mirrorRows)
    -- Looking up, the top reflector goes out of sight beside a lower one,
    -- leaving the lower pair. A blob no bigger than one reflector isn't
    -- taken for two for long: the clip is lost, and with all three back,
    -- followed again.
    let (_, pairRows) = trackNoisy clipBent 1 (heldScene 20 (const (lookingDown, (1, 1, 0))) True)
        pairTracked = [t | (t, _, Tracked{}, _) <- after 5 (map fst pairRows), t < 22]
        pairBack = after 23 (map fst pairRows)
    checkShowing "with the lower pair in view, lost after a while, not tracked for" (show (length pairTracked) ++ " frames") $ null pairTracked
    checkShowing "and followed when all three are back" (show (summed pairBack)) $ lostCount pairBack == 0 && fst (worstError pairBack) < 1.5

    -- A lamp the size of a reflector is learnt as quickly as a fire while
    -- the clip is tracked elsewhere with all three reflectors in view.
    let lamp = ((156, 297), 10)
        (_, lampRows) = trackFramesEvents clipBent [(t, fidgeting t, renderDiscs (lamp : pointDiscs defaultIntrinsics clipBent allLit (fidgeting t))) | t <- at30 8]
    checkShowing "a lamp the size of a reflector learnt beside the tracked clip" (show [(t, p) | ((t, _, _, _), evs) <- lampRows, LightIgnored p _ <- evs]) $
        case [(t, p) | ((t, _, _, _), evs) <- lampRows, LightIgnored p _ <- evs] of
            [(t, p)] -> t < 3.5 && nearFire p
            _ -> False

    -- The clip taken off for 15 minutes, out of view, and put back on: held
    -- still for 5 s while the headset settles, looking down with only the
    -- lower pair in view or with all three, or nearer the camera than
    -- before beside a stray reflection moving about. Its reflectors look as
    -- they did before it was taken off, and aren't learnt as lights; it is
    -- tracked as soon as all three show.
    let between from to rate = [from + fromIntegral i / rate | i <- [0 .. round ((to - from) * rate) - 1 :: Int]]
        -- (time, true pose, discs) to frames, each centroid wandering by
        -- up to half a pixel
        wandering rows = [(t, pose, renderDiscs (jitter 1 i discs)) | (i, (t, pose, discs)) <- zip [0 :: Int ..] rows]
        clipLit l pose = pointDiscs defaultIntrinsics clipBent l pose
        gone = 15 * 60
        -- Tracked for 10 s, then gone, then back on.
        putBack back =
            wandering $
                [(t, fidgeting t, clipLit allLit (fidgeting t)) | t <- between 0 10 30]
                    ++ [(t, fidgeting 0, []) | t <- between 10 (10 + gone) 5]
                    ++ [(t, pose, discs) | t <- between (10 + gone) (20 + gone) 30, let (pose, discs) = back (t - 10 - gone)]
        nearer = let Pose r p = fidgeting 0 in Pose r (p ^+^ V3 0 0 (-300))
        glint t = ((300 + 120 * cos (2 * pi * t / 2), 250 + 40 * sin (2 * pi * t / 2)), 9)
        -- (how it is put back, the head and discs since it was, when all
        -- three show)
        returns =
            [ ("all three in view", \t -> if t < 5 then (fidgeting 0, clipLit allLit (fidgeting 0)) else (glancingAt (t - 5), clipLit allLit (glancingAt (t - 5))), 0)
            , ("looking down, the lower pair in view", \t -> if t < 5 then (lookingDown, clipLit (1, 1, 0) lookingDown) else (glancingAt (t - 5), clipLit allLit (glancingAt (t - 5))), 5)
            , ("nearer, beside a stray reflection", \t -> (nearer, glint t : clipLit allLit nearer), 0)
            ]
    forM_ returns $ \(name, back, showing) -> do
        let (_, backRows) = trackFramesEvents clipBent (putBack back)
            shown = 10 + gone + showing
        checkShowing ("put back on after 15 minutes, held still with " ++ name ++ ", no light learnt") (show (ignored backRows)) $ null (ignored backRows)
        checkShowing ("put back on with " ++ name ++ ", and tracked") (show (fmap (subtract shown) (firstTracked shown (map fst backRows))) ++ " s after all three show, then followed " ++ show (summed (after (shown + 1) (map fst backRows)))) $
            maybe False (< 1) (fmap (subtract shown) (firstTracked shown (map fst backRows))) && lostCount (after (shown + 1) (map fst backRows)) == 0

    -- The clip lying still in view for 10 minutes, as on the desk, beside
    -- a fire, then picked up and worn. Lying with two reflectors run
    -- together, it can't be the clip, and is learnt; once worn, it is seen
    -- to have gone from there, and is forgotten, but the fire isn't. Lying
    -- with all three apart, at a pose a head can be at, it is tracked
    -- there, and not learnt.
    let lyingFor = 600
        lyingDown place =
            wandering $
                [(t, fidgeting t, heads ++ occluded heads (fireDiscs fireAt i)) | (i, t) <- zip [0 ..] (between 0 10 30), let heads = clipLit allLit (fidgeting t)]
                    ++ [(t, place, clipLit allLit place ++ fireDiscs fireAt i) | (i, t) <- zip [0 ..] (between 10 (10 + lyingFor) 10)]
                    ++ [(t, fidgeting 0, fireDiscs fireAt i) | (i, t) <- zip [0 ..] (between (10 + lyingFor) (11 + lyingFor) 30)]
                    ++ [(t, pose, heads ++ occluded heads (fireDiscs fireAt i)) | (i, t) <- zip [0 ..] (between (11 + lyingFor) (16 + lyingFor) 30), let pose = glancingAt (t - 11 - lyingFor); heads = clipLit allLit pose]
        wornAt = 11 + lyingFor
        runTogether = Pose (rotYXZ 30 (-70) 10) (V3 120 200 650)
        (_, desked) = trackFramesEvents clipBent (lyingDown runTogether)
        deskLights = filter (not . nearFire) (ignored desked)
    checkShowing "lying on the desk, learnt as lights" (show (ignored desked) ++ ", forgotten once worn " ++ show (moved desked, forgotten desked)) $
        length deskLights == 2
            && length (filter nearFire (ignored desked)) == 1
            && map snd (moved desked) == deskLights
            && all (\(t, _) -> t > wornAt && t < wornAt + 1) (moved desked)
            && null (forgotten desked)
    checkShowing "and the clip worn followed" (show (summed (after (wornAt + 1) (map fst desked)))) $ lostCount (after (wornAt + 1) (map fst desked)) == 0
    let (_, flat) = trackFramesEvents clipBent (lyingDown (Pose (rotYXZ 10 (-40) 0) (V3 120 200 650)))
        onDesk = after 10.5 (filter (\(t, _, _, _) -> t < 10 + lyingFor) (map fst flat))
    checkShowing "lying on the desk at a pose a head can be at, mostly tracked there" (show (lostCount onDesk, length onDesk)) $
        lostCount onDesk * 5 < length onDesk
    checkShowing "and not learnt" (show (ignored flat, moved flat) ++ ", then worn and followed " ++ show (summed (after (wornAt + 1) (map fst flat)))) $
        map nearFire (ignored flat) == [True] && null (moved flat) && lostCount (after (wornAt + 1) (map fst flat)) == 0

    -- From a cold start, nothing tells a reflector from a light: the lower
    -- pair held still in view is learnt. Then the top reflector shows, the
    -- clip is found in what was learnt, and as the head turns, the lights
    -- are seen to move with it and are forgotten at once.
    let coldPair = [(t, pose, l) | t <- at30 40, let (pose, l) = if t < 30 then (fidgeting 0, (1, 1, 0)) else if t < 32 then (fidgeting 0, allLit) else (glancingAt (t - 32), allLit)]
        (_, coldPairRows) = trackNoisy clipBent 1 coldPair
        coldPairTracked = map fst coldPairRows
    checkShowing "the lower pair learnt from a cold start" (show (ignored coldPairRows) ++ ", forgotten as it moves " ++ show (moved coldPairRows)) $
        length (ignored coldPairRows) == 2 && length (moved coldPairRows) == 2 && all (\(t, _) -> t > 32 && t < 33) (moved coldPairRows)
    checkShowing "the clip found in them and followed" (show (summed (after 30.5 coldPairTracked))) $
        lostCount (after 30.5 coldPairTracked) == 0 && fst (worstError (after 30.5 coldPairTracked)) < 1.5

-- | Lights learnt from frames made up, a fifth of a second apart, so that
-- a few dozen show lights learnt, widened and forgotten.
tests :: TestTree
tests =
    testGroup
        "Lights"
        [ testProperty "a blob that stays put is learnt as a light there once it has, and forgotten once it has gone" $
            \(Spot (place, size)) (Rate rate) (Wobble wobble) ->
                let lit = [(fromIntegral i / rate, [blobAt (nudged place dx dy) size]) | (i, dx, dy) <- zip3 [0 .. ceiling ((inSeconds learnSeconds + 0.2) * rate) :: Int] (cycle wobble) (drop 3 (cycle wobble))]
                    gone = [(inSeconds learnSeconds + 0.3 + fromIntegral i / 4, []) | i <- [0 .. ceiling (4 * inSeconds forgetSeconds) + 4 :: Int]]
                    (learnt, told) = watched noLights lit
                    (left, toldAfter) = watched learnt gone
                 in counterexample (show (told, lightAreas learnt, toldAfter)) $
                        case (told, lightAreas learnt, toldAfter) of
                            ([(when, Learnt at (Learning still _ NoReflectorSeen))], [(centre, radius)], [(whenGone, Forgotten was)]) ->
                                conjoin
                                    [ property (when >= inSeconds learnSeconds && still >= inSeconds learnSeconds)
                                    , property (centre == at && was == at && distance at place <= stillPixels)
                                    , property (radius >= stillPixels + marginPixels && radius <= reachPixels + marginPixels)
                                    , property (whenGone > when + inSeconds forgetSeconds)
                                    , lightAreas left === []
                                    ]
                            _ -> property False
        , testProperty "nothing is learnt in less time than a light must stay put" $
            \scene ->
                let early = takeWhile ((< inSeconds learnSeconds) . fst) (sceneFrames scene)
                 in lightAreas (fst (watched noLights early)) === [] .&&. snd (watched noLights early) === []
        , testProperty "lights are told as they come and go, their areas within bounds and never smaller" $
            \scene ->
                cover 20 (any (\(_, _, _, news) -> not (null news)) (steps scene)) "lights come or go" . conjoin $
                    [ counterexample (show (t, news, before, now)) $
                        conjoin
                            [ sort (map fst now) === sort ((map fst before \\ ([p | Forgotten p <- news] ++ [p | Moved p _ <- news])) ++ [p | Learnt p _ <- news])
                            , property (all (\(_, r) -> r >= stillPixels + marginPixels && r <= reachPixels + marginPixels) now)
                            , property (and [r' >= r | (p, r) <- before, (p', r') <- now, p == p'])
                            ]
                    | (t, before, now, news) <- steps scene
                    ]
        , testProperty "no light is learnt where the clip's reflectors are" $
            \scene ->
                property (and [distance p q > radius | (_, _, _, news) <- steps scene, Learnt p _ <- news, (q, radius) <- maybe [] viewDiscs (sceneClip scene)])
        , testProperty "a blob is masked just when it is in a light's area, and never where a reflector is predicted" $
            \scene (Spot (place, size)) ->
                let ls = fst (watched noLights (sceneFrames scene))
                    b = blobAt place size
                 in inMask (activeMask [] ls) b === any (\(centre, radius) -> distance place centre <= radius) (lightAreas ls)
                        .&&. property (not (inMask (activeMask [place] ls) b))
                        .&&. property (not (inMask noMask b))
        , testProperty "of the blobs in masked areas, the heaviest in each are kept in their order" $
            \scene (Spots spots) ->
                let ls = fst (watched noLights (sceneFrames scene))
                    mask = activeMask [] ls
                    blobs = sortOn (negate . blobPoints) (map (uncurry blobAt) spots)
                    kept = heaviestIn mask blobs
                 in conjoin
                        [ property (all (inMask mask) kept)
                        , property (length kept <= length (lightAreas ls))
                        , kept === filter (`elem` kept) blobs
                        , property (null kept == not (any (inMask mask) blobs))
                        ]
        , testProperty "lights unlearnt hold nothing, and the rest are left" $
            \scene (Spots spots) ->
                let places = map fst spots
                    ls = fst (watched noLights (sceneFrames scene))
                    centres = map fst (lightAreas ls)
                    (left, gone) = unlearn centres places ls
                 in conjoin
                        [ property (not (holding centres places left))
                        , holding centres places ls === not (null gone)
                        , sort (map fst (lightAreas left) ++ gone) === sort centres
                        ]
        ]

-- | Somewhere in the picture, and a blob's pixels there, from a
-- reflector's to a lamp's; simplest a reflector's at the picture's
-- corner, and simpler at a whole number of pixels, and smaller.
newtype Spot = Spot ((Double, Double), Int)
    deriving (Show)

instance Arbitrary Spot where
    arbitrary = Spot <$> genSpot
    shrink (Spot spot) = Spot <$> shrinkSpot spot

-- | A few of 'Spot'.
newtype Spots = Spots [((Double, Double), Int)]
    deriving (Show)

instance Arbitrary Spots where
    arbitrary = Spots <$> listOf genSpot
    shrink (Spots spots) = Spots <$> shrinkList shrinkSpot spots

genSpot :: Gen ((Double, Double), Int)
genSpot = (,) <$> ((,) <$> choose (0, 640) <*> choose (0, 480)) <*> chooseInt (20, 2000)

shrinkSpot :: ((Double, Double), Int) -> [((Double, Double), Int)]
shrinkSpot =
    shrinkParts
        [ \(place, size) -> [(place', size) | place' <- shrinkPlace place]
        , \(place, size) -> [(place, size') | size' <- shrinkFrom 20 size]
        ]

-- | How many frames a second the camera sends.
newtype Rate = Rate Double
    deriving (Show)

instance Arbitrary Rate where
    arbitrary = Rate <$> elements [30, 60, 120]
    shrink (Rate rate) = Rate <$> earlier [30, 60, 120] rate

-- | How far a still blob's centre wanders, in pixels each way, from frame
-- to frame, over and over; simplest not at all, and simpler wandering
-- less.
newtype Wobble = Wobble [Double]
    deriving (Show)

instance Arbitrary Wobble where
    arbitrary = Wobble <$> vectorOf 8 (choose (-1, 1))
    shrink (Wobble wobble) = Wobble <$> shrinkTo (replicate 8 0) (\offs -> [before ++ [0] ++ rest | (before, _ : rest) <- zip (inits offs) (tails offs)]) wobble

nudged :: (Double, Double) -> Double -> Double -> (Double, Double)
nudged (x, y) dx dy = (x + dx, y + dy)

-- | A round blob of so many pixels.
blobAt :: (Double, Double) -> Int -> Blob
blobAt (x, y) size = Blob x y size (200 * size) spread spread 0
  where
    spread = fromIntegral size / (4 * pi)

-- | Learn from frames of blobs at their times, the clip unseen: the lights
-- after them, and the news with its times.
watched :: Lights -> [(Double, [Blob])] -> (Lights, [(Double, News)])
watched ls frames = (ls', concat told)
  where
    (ls', told) = mapAccumL (\l (t, blobs) -> let (l', news) = learnLights (clockAt t) unseen blobs l in (l', map ((,) t) news)) ls frames

-- | Frames of blobs at their times, and the clip, if it is tracked in all
-- of them, its reflectors' blobs among the frames'.
data Scene = Scene
    { sceneClip :: Maybe View
    , sceneFrames :: [(Double, [Blob])]
    }

instance Show Scene where
    show scene = show (viewDiscs <$> sceneClip scene, [(t, [((blobX b, blobY b), blobPoints b) | b <- blobs]) | (t, blobs) <- sceneFrames scene])

-- | Each frame's time, the lights' areas before and after it, and its
-- news.
steps :: Scene -> [(Double, [((Double, Double), Double)], [((Double, Double), Double)], [News])]
steps scene = snd (mapAccumL step' noLights (sceneFrames scene))
  where
    step' ls (t, blobs) = let (ls', news) = learnLights (clockAt t) (fromMaybe unseen (sceneClip scene)) blobs ls in (ls', (t, lightAreas ls, lightAreas ls', news))

-- | A few lamps, each lit in most frames, with a fragment beside it in
-- some, and strays that come and go; perhaps the clip, tracked throughout;
-- a fifth of a second between frames, and now and then a minute.
-- Simplest no frames and no clip; simpler ending sooner, and without the
-- clip.
instance Arbitrary Scene where
    arbitrary = do
        lamps <- scale (`div` 20) (listOf genSpot)
        clip <- oneof [pure Nothing, Just <$> vectorOf 3 genSpot]
        gaps <- listOf (frequency [(30, pure 0.2), (1, pure 60)])
        frames <- mapM (frame lamps (fromMaybe [] clip)) (drop 1 (scanl (+) 0 gaps))
        pure (Scene (tracked <$> clip) frames)
      where
        frame lamps clip t = do
            lit <- mapM (\lamp -> frequency [(9, core lamp), (1, pure [])]) lamps
            strays <- scale (`div` 30) (listOf genSpot)
            pure (t, map (uncurry blobAt) (concat lit ++ strays ++ clip))
        core (place, size) = do
            dx <- choose (-1, 1)
            dy <- choose (-1, 1)
            fragment <- frequency [(3, pure []), (1, (\fx fy -> [(nudged place fx fy, size `div` 4)]) <$> choose (-18, 18) <*> choose (-18, 18))]
            pure ((nudged place dx dy, size) : fragment)
        tracked clip =
            View
                { viewSights = [Just (Sight place (fromIntegral size) Whole) | (place, size) <- clip]
                , viewDiscs = [(place, 15) | (place, _) <- clip]
                , viewUsual = Just (maximum [fromIntegral size | (_, size) <- clip])
                }
    shrink =
        shrinkParts
            [ \scene -> [scene{sceneFrames = take k (sceneFrames scene)} | let n = length (sceneFrames scene), k <- [0 | n > 0] ++ [n `div` 2 | n > 1] ++ [n - 1 | n > 2]]
            , \scene -> [scene{sceneClip = Nothing} | Just _ <- [sceneClip scene]]
            ]
