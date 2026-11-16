-- | The tracker: what it does with requests, how it follows the clip
-- through what the camera shows, and what it says of why.
module Test.Old.Tracker (tests, simulations) where

import Control.Monad (forM_)
import Data.List (isPrefixOf, mapAccumL, nub)
import Test.Tasty (TestTree, testGroup)

import Test.Check
import Test.Old.Simulation
import qualified Test.Old.Tracker.Blobs
import qualified Test.Old.Tracker.Choose
import qualified Test.Old.Tracker.Follow
import qualified Test.Old.Tracker.Judge
import qualified Test.Old.Tracker.Kept
import qualified Test.Old.Tracker.Layout
import qualified Test.Old.Tracker.Sizes
import qualified Test.Old.Tracker.Step
import qualified Test.Old.Tracker.Trail
import TrackKat.Calibration
import TrackKat.Linear
import TrackKat.Old.Tracker
import TrackKat.Pose
import TrackKat.Synth
import TrackKat.Term (Bound (..), Finding (..), Measure (..), Name (..))
import qualified TrackKat.Term as Term
import TrackKat.World (clockAt)

tests :: TestTree
tests = group "Tracker" $ do
    -- Tracker: requests wait for a tracked frame; losing the clip is reported.
    let clipAt pose = concatMap (squareStripes . project defaultIntrinsics pose) [mLeft headset, mRight headset, mTop headset]
        facing = Pose identity (V3 0 0 700)
        turned = Pose (rotYXZ 20 0 0) (V3 0 0 700)
        t0 = newTracker defaultIntrinsics headset MaskLights defaultCalibration
        (t1, firstOut, firstEvents) = step (clockAt 0) (clipAt facing) t0
        (t2, lostOut, lostEvents) = step (clockAt 0.1) (take 5 (clipAt facing)) (request Recenter t1)
        (_, laterOut, laterEvents) = step (clockAt 0.2) (clipAt turned) t2
    checkShowing "first frame recenters" (show (firstOut, firstEvents)) $ case (firstOut, firstEvents) of
        (Tracked{outPose = hp}, [Recentered]) -> all ((< 0.01) . abs) [yaw hp, pitch hp, roll hp]
        _ -> False
    checkShowing "one blob is lost" (show (lostOut, lostEvents)) $ case (lostOut, lostEvents) of
        (Lost 1 0 _ _, []) -> True
        _ -> False
    checkShowing "pending recenter waits for tracking" (show (laterOut, laterEvents)) $ case (laterOut, laterEvents) of
        (Tracked{outPose = hp}, [Recentered]) -> all ((< 0.01) . abs) [yaw hp, pitch hp, roll hp]
        _ -> False

    -- Told to the user once it has lasted a second, and not again for a
    -- while, unless the clip came back meanwhile and the quiet has passed.
    let lostFor why = Lost 3 0 why Nothing
        because name = Why Following (Unfit [Finding (Name name ("it is " ++ name)) Term.impossible Nothing])
        tracked = Tracked (HeadPose 0 0 0 0 0 0) 800 0 (Seen [] 0 0 0) [] (Chosen Followed Nothing)
        tellings = snd . mapAccumL (\a (t, o) -> let (a', said) = absence (clockAt t) o a in (a', (,) t <$> said)) noAbsence
        outcomeAt t
            | t < 2 = tracked
            | t < 8 = lostFor (because "aside")
            | t < 20 = lostFor (because "far")
            | t < 21 = tracked
            | t < 40 = lostFor (because "far")
            | otherwise = Lost 2 0 (because "far") Nothing
        told = [said | Just said <- tellings [(t, outcomeAt t) | t <- at120 50]]
    checkShowing "out of sight is told once a second in, then seldom" (show told) $ case told of
        [(a, first), (b, second), (c, third)] ->
            abs (a - 3) < 0.02
                && first == "The clip is out of sight, though 3 lights are in view: it is aside."
                && abs (b - 13) < 0.02
                && second == "The clip is out of sight, though 3 lights are in view: it is far."
                && abs (c - 23) < 0.02
                && third == second
        _ -> False

    tree Test.Old.Tracker.Blobs.tests
    tree Test.Old.Tracker.Sizes.tests
    tree Test.Old.Tracker.Trail.tests
    tree Test.Old.Tracker.Judge.tests
    tree Test.Old.Tracker.Layout.tests
    tree Test.Old.Tracker.Follow.tests
    tree Test.Old.Tracker.Choose.tests
    tree Test.Old.Tracker.Kept.tests
    tree Test.Old.Tracker.Step.tests

simulations :: TestTree
simulations = testGroup "Tracker" [followed, explained]

-- | Following the clip through what the camera shows.
followed :: TestTree
followed = group "following the clip" $ do
    -- Following the clip through what the camera shows: reflectors fading
    -- at a slant, running together, breaking up, and stray reflections.
    -- Frames come at 120 a second, rendered as discs of lit pixels.
    let heights m pose = let ps = map (project defaultIntrinsics pose) [mLeft m, mRight m, mTop m] in (ps, identify m id ps == Just (ps !! 0, ps !! 1, ps !! 2))

    -- The label swap seen at pitch -8: with the camera above, the top
    -- reflector dips to the lower pair's level and labelling by height in
    -- the image swaps two reflectors, giving a wrong pose that fits just
    -- as well. Labelling by prediction keeps following.
    let nodding = [(t, headSeen straightWorn (-4) (-6 * (1 - cos (2 * pi * t / 1.5))) 0, allLit) | t <- at120 5]
        nodded = trackPath clipStraight nodding
        swapped = length [() | (_, pose, _) <- nodding, not (snd (heights clipStraight pose))]
    checkShowing "height labelling swaps while nodding" (show swapped) $ swapped > 100
    checkShowing "nodding through the swap" (show (summed nodded)) $ lostCount nodded == 0 && fst (worstError nodded) < 1 && snd (worstError nodded) < 10

    -- An impossible jump is not followed: the head 40 degrees away from
    -- one frame to the next is lost at first, while it is likelier where
    -- it was with a reflector out of sight, and within a few frames is
    -- found afresh there.
    let jumping = [(t, headSeen bentWorn (if t < 0.5 then 0 else 40) 0 0, allLit) | t <- at120 1.2]
        jumped = trackPath clipBent jumping
    checkShowing "a jump is lost at first, for" (show (lostCount jumped) ++ " frames") $
        all isLost [o | (t, _, o, _) <- jumped, t >= 0.5, t < 0.51] && lostCount jumped <= 3
    checkShowing "then believed" (show (summed (after 1 jumped))) $ lostCount (after 1 jumped) == 0 && fst (worstError jumped) < 1

    -- The top reflector fading from view for a moment while the head
    -- turns: the two lower ones are tracked, the turn followed.
    let fading = [(t, headSeen bentWorn (10 * sin (2 * pi * t / 3)) 0 0, (1, 1, max 0 (min 1 (abs (t - 1.7) / 0.5 - 0.4)))) | t <- at120 3]
        faded = trackPath clipBent fading
    checkShowing "tracked through fading" (show (summed faded)) $ lostCount faded == 0 && fst (worstError faded) < 1
    check "two of three reflectors reported" $ any ((== 1) . length . seenMissing) (seens faded)

    -- Out of sight for longer, it is lost, and found again on its return.
    let hiding = [(t, headSeen bentWorn (5 * sin (2 * pi * t / 3)) 0 0, (1, 1, if t > 1 && t < 3 then 0 else 1)) | t <- at120 4]
        hidden = trackPath clipBent hiding
    check "with one reflector gone for a second, lost" $ all isLost [o | (t, _, o, _) <- hidden, t > 2.05, t < 3]
    checkShowing "back after hiding" (show (summed (after 3 hidden))) $ lostCount (after 3 hidden) == 0 && fst (worstError hidden) < 1

    -- A reflection that's no reflector, while one reflector fades out.
    let reflecting = [(t, headSeen bentWorn (8 * sin (2 * pi * t / 2)) 0 0, (1, if t > 1 && t < 1.6 then 0 else 1, 1)) | t <- at120 2.5]
        stray = ((340, 288), 3)
        reflected = trackFrames clipBent [(t, pose, renderDiscs (stray : pointDiscs defaultIntrinsics clipBent lit pose)) | (t, pose, lit) <- reflecting]
    checkShowing "a stray reflection ignored" (show (summed reflected)) $ lostCount reflected == 0 && fst (worstError reflected) < 1.5
    check "the stray reported" $ all ((== 1) . seenStrays) (seens reflected)
    let alone = headSeen bentWorn 0 0 0
    check "two reflectors and a stray aren't three reflectors" $ case trackFrames clipBent [(0, alone, renderDiscs (stray : pointDiscs defaultIntrinsics clipBent (1, 0, 1) alone))] of
        [(_, _, Lost 3 0 _ _, _)] -> True
        _ -> False

    -- Lights close to the camera laid out like the clip, as a phone's can
    -- be: no head can be there, so the clip is lost, not followed there.
    let phone pose = [(project defaultIntrinsics (Pose (poseR pose) (190 *^ normalize (poseT pose))) p, 5) | p <- [mLeft clipBent, mRight clipBent, mTop clipBent]]
        phoned = trackFrames clipBent [(t, pose, renderDiscs (if t < 1 then pointDiscs defaultIntrinsics clipBent allLit pose else phone pose)) | t <- at120 3, let pose = headSeen bentWorn 0 0 0]
    check "lights by the camera aren't the clip" $ all isLost [o | (t, _, o, _) <- phoned, t >= 1]

    -- Leaning in toward the camera, slowly, the clip coming straight at it
    -- from 870 mm (the pivot at about 950) to 270: a seated head reaches
    -- far closer than it reaches sideways.
    let leantTo d = let Pose r p = headSeen bentWorn 0 0 0 in Pose r (d *^ normalize p)
        leanDistance t = 870 - 50 * max 0 (t - 1)
        leaningIn = [(t, leantTo (leanDistance t), allLit) | t <- at120 13]
        leant = trackPath clipBent leaningIn
        lostAt = [round (leanDistance t) :: Int | (t, _, Lost{}, _) <- leant]
    checkShowing "leaning in to 270 mm" (show (take 3 lostAt, summed leant)) $ null lostAt && fst (worstError leant) < 1

    -- Put back on 500 mm farther back than where it was recentered, as after
    -- sitting back during a break: found again.
    let sitBack t = if t < 1 then leantTo 870 else leantTo 1370
        returned = trackFrames clipBent [(t, sitBack t, renderDiscs (if t >= 1 && t < 3 then [] else pointDiscs defaultIntrinsics clipBent allLit (sitBack t))) | t <- at120 5]
    checkShowing "sat back after a break" (show (summed returned)) $
        not (any isLost [o | (t, _, o, _) <- returned, t >= 3.1]) && fst (worstError [r | r@(t, _, _, _) <- returned, t >= 3.1]) < 1

    -- A phone's lights laid out like the clip, 400 mm in front of the face,
    -- toward the camera, are not the clip leaning in: not while the clip
    -- is in view, nor once it has gone from view.
    let face = headSeen bentWorn 0 0 0
        inFront = let Pose r p = face in Pose r ((norm p - 400) *^ normalize p)
        phoneLights = [(project defaultIntrinsics inFront p, 5) | p <- [mLeft clipBent, mRight clipBent, mTop clipBent]]
        withPhone = trackFrames clipBent [(t, face, renderDiscs (pointDiscs defaultIntrinsics clipBent allLit face ++ (if t >= 1 then phoneLights else []))) | t <- at120 2]
        phoneOnly = trackFrames clipBent [(t, face, renderDiscs (if t < 1 then pointDiscs defaultIntrinsics clipBent allLit face else phoneLights)) | t <- at120 3]
    checkShowing "a phone in front of the clip in view isn't the clip" (show (summed withPhone)) $ lostCount withPhone == 0 && fst (worstError withPhone) < 1
    check "a phone in front of the clip gone from view isn't the clip" $ all isLost [o | (t, _, o, _) <- phoneOnly, t >= 1]

    -- A reflector broken in two by a line too dim, and a fleck beside
    -- another, as at a low threshold.
    let broken = [(t, pose, (if t > 0.5 then breakAt (project defaultIntrinsics pose (mLeft clipBent)) else id) (renderDiscs (fleck pose : pointDiscs defaultIntrinsics clipBent allLit pose))) | t <- at120 2.5, let pose = headSeen bentWorn (8 * sin (2 * pi * t / 2)) 0 0]
        fleck pose = let (u, v) = project defaultIntrinsics pose (mTop clipBent) in ((u + 4, v + 9), 2.5)
        pieces = trackFrames clipBent broken
    checkShowing "a reflector in pieces" (show (summed pieces)) $ lostCount pieces == 0 && fst (worstError pieces) < 1

    -- Rolling over far enough that the top reflector is no longer highest.
    -- (The straight clip's top reflector, being lower, needs less roll.)
    let rolling = [(t, headSeen straightWorn 0 0 (60 * sin (pi * t / 4)), allLit) | t <- at120 4]
        rolled = trackPath clipStraight rolling
    check "height labelling swaps when rolled over" $ any (\(_, pose, _) -> not (snd (heights clipStraight pose))) rolling
    checkShowing "rolling over" (show (summed rolled)) $
        lostCount rolled == 0 && fst (worstError (unmerged rolled)) < 1 && fst (worstError rolled) < 4

    -- Turned far, the top reflector runs into a lower one: split by shape.
    let turning = [(t, headSeen bentWorn (90 * t / 8) 0 0, allLit) | t <- at120 8]
        turnedFar = trackPath clipBent turning
    check "merged blobs reported" $ any ((> 0) . seenMerged) (seens turnedFar)
    checkShowing "turned far, through merging" (show (summed turnedFar)) $ lostCount turnedFar == 0 && fst (worstError turnedFar) < 4 && snd (worstError turnedFar) < 10

    -- Lost for a second and a half while turned, and back with the top
    -- reflector faint.
    let returning = [(t, headSeen bentWorn (min 45 (30 * t)) 0 0, if t > 2 && t < 3.5 then (0, 0, 0) else (1, 1, if t < 3.5 then 1 else 0.5)) | t <- at120 4]
        back = trackPath clipBent returning
    checkShowing "found again when back" (show (summed (after 3.5 back))) $ lostCount (after 3.5 back) == 0 && fst (worstError back) < 1

    -- A slow sweep through everything a head does, for both clips: the
    -- straight one worn as it was crosses the pitch where the labels swapped.
    forM_ [("bent", clipBent, bentWorn), ("straight", clipStraight, straightWorn)] $ \(name, m, wornAt) -> do
        let sweeping = [(t, headSeen wornAt (40 * sin (2 * pi * t / 7)) (25 * sin (2 * pi * t / 5.3)) (20 * sin (2 * pi * t / 4.1)), allLit) | t <- at120 20]
            swept = trackPath m sweeping
            sweptCorrections = corrections (snd (trackFramesEvents m (rendered m sweeping)))
            stepSizes = zipWith (\(_, _, _, a) (_, _, _, b) -> maybe 0 fst (poseError <$> a <*> b)) swept (drop 1 swept)
        checkShowing ("sweep with the " ++ name ++ " clip") (show (summed swept, maximum stepSizes, sweptCorrections)) $
            null sweptCorrections
                && lostCount swept == 0
                && fst (worstError (unmerged swept)) < 1
                && fst (worstError swept) < 4
                && snd (worstError swept) < 20
                && maximum stepSizes < 3

-- | Tracking says why it does what it does: why the clip is lost, why a
-- light is learnt, and what became of windows of turning.
explained :: TestTree
explained = group "saying why" $ do
    let face = headSeen bentWorn 0 0 0
    -- Why the clip is lost. Moved slowly aside, beyond where a seated
    -- head goes: the reach from the centre says so, by name.
    let aside d = let Pose r p = face in Pose r (p ^+^ V3 d 0 0)
        whys rows = [(t, w) | (t, _, Lost{outWhy = w}, _) <- rows]
        rejecting rows = nub (map (whyRejectedBy . snd) (whys rows))
        asideRows = trackPath clipBent [(t, aside (120 * t), allLit) | t <- at120 4.2]
    checkShowing "moved aside beyond reach, lost for that" (show (take 1 (whys asideRows))) $
        take 1 (rejecting asideRows) == [["aside"]]
            && and [looking == Following | (_, Why looking _) <- take 1 (whys asideRows)]
            && and [measureValue m > 400 && measureLimit m == 400 | (_, Why _ (Unfit [Finding (Name "aside" _) _ (Just m)])) <- whys asideRows]
    checkShowing "and said plainly" (concat (take 1 (map (describeWhy . snd) (whys asideRows)))) $
        take 1 (map (describeWhy . snd) (whys asideRows)) == ["too far to the side of where you recentered"]
    checkShowing "and to a developer" (concat (take 1 (map (explainWhy . snd) (whys asideRows)))) $
        any ("following the clip, rejected: aside 40" `isPrefixOf`) (take 1 (map (explainWhy . snd) (whys asideRows)))

    -- Lights by the camera laid out like the clip: following, they don't
    -- fit where the clip was; found afresh, they are too near.
    let phone pose = [(project defaultIntrinsics (Pose (poseR pose) (190 *^ normalize (poseT pose))) p, 5) | p <- [mLeft clipBent, mRight clipBent, mTop clipBent]]
        phoned = trackFrames clipBent [(t, face, renderDiscs (if t < 1 then pointDiscs defaultIntrinsics clipBent allLit face else phone face)) | t <- at120 2]
    checkShowing "lights by the camera don't fit, then are too near" (show (rejecting phoned)) $
        and ["fit" `elem` whyRejectedBy w | (t, w) <- whys phoned, t < 1.4]
            && and [looking == Afresh && "near" `elem` whyRejectedBy w | (t, w@(Why looking _)) <- whys phoned, t > 1.6]

    -- Lights laid out like the clip where the clip was, but far smaller
    -- than its reflectors were: too small.
    let smallRows = trackFrames clipBent [(t, face, renderDiscs (if t < 1 then lit else if t < 2 then [] else [(p, 0.3 * r) | (p, r) <- lit])) | t <- at120 3, let lit = pointDiscs defaultIntrinsics clipBent allLit face]
    checkShowing "lights too small for the clip" (show (rejecting (after 2 smallRows))) $ rejecting (after 2 smallRows) == [["size"]]

    -- Too few and too many big blobs, and none at all.
    let countRows n = trackFrames clipBent [(t, face, renderDiscs (take n (((100, 100), 9) : ((500, 400), 9) : pointDiscs defaultIntrinsics clipBent allLit face))) | t <- at120 0.1]
        counts n = [(measureValue m, measureBound m, measureLimit m) | (_, Why Afresh (Unfit [Finding (Name "big blobs" _) _ (Just m)])) <- whys (countRows n)]
    checkShowing "fewer than three big blobs, and more" (show (map (take 1 . counts) [0, 2, 5])) $
        map (take 1 . counts) [0, 2, 5] == [[(0, AtLeast, 3)], [(2, AtLeast, 3)], [(5, AtMost, 3)]]

    -- A reflector out of sight for too long: lost for that.
    let hidden = trackPath clipBent [(t, face, (1, 1, if t > 1 then 0 else 1)) | t <- at120 2.4]
    checkShowing "a reflector gone too long" (show (rejecting hidden)) $ take 1 (rejecting hidden) == [["partly seen"]]

    -- An impossible jump: the head would have turned too fast.
    let jumped = trackPath clipBent [(t, headSeen bentWorn (if t < 0.5 then 0 else 40) 0 0, allLit) | t <- at120 0.6]
    checkShowing "a jump" (show (rejecting jumped)) $ rejecting jumped == [["turn"]]

    -- A tracked frame says what it was judged by.
    check "a tracked frame's findings" $ case [map (brief . findingName) judged | (_, _, Tracked{outJudged = judged}, _) <- take 3 jumped] of
        [afresh, following, _] -> afresh == ["near", "far", "size"] && following == ["places", "sizes", "unseen", "fit", "turn", "move", "near", "far", "turned", "aside", "usual size", "misfit", "turning", "moving"]
        _ -> False

    -- Why a light is learnt: a lamp bigger than the reflectors, beside
    -- the clip tracked; and a fire before the clip was ever seen.
    let glancing t = headSeen bentWorn (20 * sin (2 * pi * t / 2.5)) (5 * sin (2 * pi * t / 3.7)) 0
        learnings rows = [why | (_, evs) <- rows, LightIgnored _ why <- evs]
        (_, lampRows) = trackFramesEvents clipBent [(t, glancing t, renderDiscs ([((500, 100), 14) | t >= 1] ++ pointDiscs defaultIntrinsics clipBent allLit (glancing t))) | t <- at120 5]
        (_, fireRows) = trackFramesEvents clipBent [(t, face, renderDiscs [((500, 100), 14)]) | t <- at120 5]
    checkShowing "a lamp learnt for staying put and being big" (show (learnings lampRows)) $ case learnings lampRows of
        [Learning still size (BiggerThanReflectors biggest)] -> still >= 3 && still < 3.1 && size > 1.5 * biggest && biggest > 200
        _ -> False
    checkShowing "a light learnt before the clip was seen" (show (learnings fireRows)) $ case learnings fireRows of
        [Learning still _ NoReflectorSeen] -> still >= 3 && still < 3.1
        _ -> False
    check "and told why" $
        [describeLightIgnored (500, 100) why | why <- learnings fireRows] == ["Ignoring a bright light at (500, 100): it stayed put for 3 seconds, before the clip was seen."]

    -- Windows of turning are counted as taken, by each thing that learns
    -- from them, and those of a head then held still as left out for too
    -- little turning.
    let turnedThenStill = rendered clipBent [(t, if t < 8 then glancing t else glancing 8, allLit) | t <- at120 13]
        (turnedTracker, _) = trackFramesEvents clipBent (takeWhile (\(t, _, _) -> t < 8) turnedThenStill)
        (stillTracker, _) = trackFramesEvents clipBent turnedThenStill
        taken w = sum [n | ("taken", n) <- Term.tallyCounts w]
        leftOut w = sum [n | ("turn", n) <- Term.tallyCounts w]
        tallies tracker = [(what, Term.renderTally w) | (what, w) <- trackerWindows tracker]
    checkShowing "windows of a turning head are taken" (show (tallies turnedTracker)) $
        map fst (trackerWindows turnedTracker) == ["pivot", "clip shape", "mirror image"] && and [taken w > 2 | (_, w) <- trackerWindows turnedTracker]
    checkShowing "and of a head then still, left out for too little turning" (show (tallies stillTracker)) $
        and
            [ taken later <= taken before + 1 && leftOut later >= leftOut before + 3 && map (brief . findingName) (Term.tallyLast later) == ["turn"]
            | ((_, before), (_, later)) <- zip (trackerWindows turnedTracker) (trackerWindows stillTracker)
            ]
