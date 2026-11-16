-- | Hypotheses about a frame weighed against each other: a pose that can't
-- continue the last is taken afresh in the very frame, a pose taken onto a
-- wrong blob gives way to a better one, at once or once the frames have
-- said so for long enough, and what goes by continuity carries on for a
-- pose that continues or resumes one, and starts over for one come afresh.
module Test.Following (simulations) where

import Data.List (isPrefixOf)
import Test.Tasty (TestTree)

import Test.Check
import Test.Simulation
import TrackKat.Linear
import TrackKat.Pose
import TrackKat.Synth
import TrackKat.Term (Finding (..), Name (..), Score (..))
import qualified TrackKat.Term as Term
import TrackKat.Tracker

simulations :: TestTree
simulations = group "Following" $ do
    -- How each frame after the first was made out, that wasn't by
    -- continuing the last pose where it predicted the reflectors.
    let ways rows = [(t, way) | (t, _, Tracked{outChosen = Chosen way picking _ _}, _) <- rows, (way, picking) /= (Continued, ByPrediction), t > 0]
        windowsOf what tracker = concat [Term.tallyCounts tally | (name, tally) <- trackerWindows tracker, name == what]
        taken what tracker = sum [n | ("taken", n) <- windowsOf what tracker]
        glancing t = headSeen bentWorn (25 * sin (2 * pi * t / 2.5)) (10 * sin (2 * pi * t / 3.7)) 0
        tracking frames = let (tracker, rows) = trackFramesEvents clipBent frames in (tracker, map fst rows)
        (undisturbed, _) = tracking [(t, glancing t, renderDiscs (pointDiscs defaultIntrinsics clipBent allLit (glancing t))) | t <- at120 9]

    -- The scores hypotheses are compared by: a frame after a pose, a pose
    -- come afresh has some 25 nats less room than one that continues it
    -- has.
    let judgedBy name rows = [nats (findingScore f) | (_, _, Tracked{outChosen = Chosen Appeared _ _ _, outJudged = judged}, _) <- rows, f <- judged, brief (findingName f) == name]

    -- A fast turn, 200 degrees a second, with one frame showing the clip
    -- where it was 15 degrees back: that frame is taken as it shows, a
    -- turn a head could just make, and so is the next, back again. No
    -- frame is lost.
    let turning t = headSeen bentWorn (-40 + 200 * max 0 (min 0.4 (t - 1))) 0 0
        stale = trackFrames clipBent [(t, turning t, renderDiscs (pointDiscs defaultIntrinsics clipBent allLit (turning (if i == 150 then t - 0.075 else t)))) | (i, t) <- zip [0 :: Int ..] (at120 3)]
    checkShowing "a frame of the clip where it was a moment before, in a fast turn, costs" (show (lostCount stale) ++ " frames, then " ++ show (ways (after 1.25 stale))) $
        lostCount stale == 0
            && all ((== Continued) . snd) (ways (after 1.25 stale))
            && fst (worstError (after 1.27 stale)) < 1

    -- A head jumping 40 degrees: come afresh there, in the very frame,
    -- and what was known of the mirror image starts over, so fewer
    -- windows of turning teach the pivot than undisturbed.
    let leaping t = if t < 6 then glancing t else headSeen bentWorn (40 + 25 * sin (2 * pi * t / 2.5)) (10 * sin (2 * pi * t / 3.7)) 0
        (leapt, leaptRows) = tracking [(t, leaping t, renderDiscs (pointDiscs defaultIntrinsics clipBent allLit (leaping t))) | t <- at120 9]
    checkShowing "a jump come afresh" (show (ways leaptRows) ++ " starts the mirror image's belief over: windows for the pivot " ++ show (taken "pivot" leapt, taken "pivot" undisturbed)) $
        map snd (ways leaptRows) == [Appeared]
            && all ((< 6.03) . fst) (ways leaptRows)
            && lostCount leaptRows == 0
            && taken "pivot" leapt < taken "pivot" undisturbed
    checkShowing "a pose come afresh a frame or two on has room of" (show (take 1 (judgedBy "room" (after 6 leaptRows))) ++ " nats") $
        case judgedBy "room" (after 6 leaptRows) of
            cost : _ -> cost < -20 && cost > -32
            [] -> False

    -- The clip seen elsewhere for three frames, none of its reflectors
    -- where they were, and then back: tracked there, and resumed where it
    -- was with what was known of the mirror image kept, as if those
    -- frames had been lost.
    let elsewhere = let Pose r p = headSeen bentWorn 40 0 0 in Pose r (p ^+^ V3 150 0 0)
        shown i t = if i >= 720 && i < 723 then elsewhere else glancing t
        (visited, visitedRows) = tracking [(t, shown i t, renderDiscs (pointDiscs defaultIntrinsics clipBent allLit (shown i t))) | (i, t) <- zip [0 :: Int ..] (at120 9)]
    checkShowing "the clip elsewhere for three frames and back" (show (ways visitedRows) ++ " keeps the mirror image's belief: windows for the pivot " ++ show (taken "pivot" visited, taken "pivot" undisturbed)) $
        map snd (ways visitedRows) == [Appeared, Resumed]
            && lostCount visitedRows == 0
            && taken "pivot" visited >= taken "pivot" undisturbed - 1
            && taken "mirror image" visited >= taken "mirror image" undisturbed - 1
            && fst (worstError visitedRows) < 1

    -- One frame of the clip 40 degrees away: every blob of it fits the
    -- clip there and none fits it where it was, so it is taken as come
    -- afresh there, and resumed where it was in the next.
    let (blinked, blinkRows) = tracking [(t, glancing t, renderDiscs (pointDiscs defaultIntrinsics clipBent allLit (if i == 720 then headSeen bentWorn 40 0 0 else glancing t))) | (i, t) <- zip [0 :: Int ..] (at120 9)]
    checkShowing "a frame of the clip turned away is taken as it shows, and resumed" (show (ways blinkRows) ++ ", windows for the pivot " ++ show (taken "pivot" blinked, taken "pivot" undisturbed)) $
        lostCount blinkRows == 0
            && map snd (ways blinkRows) == [Appeared, Resumed]
            && taken "pivot" blinked >= taken "pivot" undisturbed - 1
            && fst (worstError (after 6.01 blinkRows)) < 1

    -- Dim reflectors, half their usual width. As the top one goes dark
    -- for six frames, a light three quarters its width comes on 8 pixels
    -- from it: the light is taken for the reflector, a blob being there
    -- and no other, though smaller than the reflector was learnt to be.
    -- When the reflector shows again, the three reflectors are the sizes
    -- they were, and that outscores keeping to the light.
    let dim lit pose = [(p, 0.5 * r) | (p, r) <- pointDiscs defaultIntrinsics clipBent lit pose]
        (topAt, topWidth) = dim allLit (glancing 6) !! 2
        light width t = [((fst topAt + 8, snd topAt), width * topWidth) | t >= 6]
        caughtBy width dark = tracking [(t, glancing t, renderDiscs (light width t ++ dim (1, 1, if t >= 6 && t < 6 + dark then 0 else 1) (glancing t))) | t <- at120 9]
        (_, caughtRows) = caughtBy 0.74 0.05
        wrongFrom rows = [t | (t, truth, Tracked{}, Just p) <- rows, fst (poseError truth p) > 5]
    checkShowing "taken onto a light by a reflector gone dark" (show (worstError [r | r@(t, _, _, _) <- caughtRows, t >= 6, t < 6.05]) ++ ", and back when the reflector shows: wrong at " ++ show (wrongFrom caughtRows) ++ ", then " ++ show (summed (after 6.1 caughtRows))) $
        fst (worstError [r | r@(t, _, _, _) <- caughtRows, t >= 6, t < 6.05]) > 5
            && all (< 6.06) (wrongFrom caughtRows)
            && fst (worstError (after 6.1 caughtRows)) < 1
            && lostCount caughtRows == 0

    -- The light as big as the reflector, and the reflector dark for half
    -- a second: when it shows again nothing in one frame says which three
    -- blobs are the clip. As the head turns on, the pose kept to the
    -- light fits the sizes learnt worse than the three reflectors do,
    -- frame after frame, and that doubt adds up until they win.
    let (_, doubtedRows) = caughtBy 1 0.5
        doubts = [nats (findingScore f) | (_, _, Tracked{outChosen = Chosen Appeared _ _ _, outJudged = judged}, _) <- after 6.5 doubtedRows, f <- judged, brief (findingName f) == "doubt"]
    checkShowing "a light as big as the reflector is left once the frames have said so for long enough" (show (take 1 (reverse (wrongFrom doubtedRows))) ++ " s, on doubt of " ++ show (take 1 doubts) ++ " nats") $
        not (null doubts)
            && all (> 0) doubts
            && all (< 7.5) (wrongFrom doubtedRows)
            && fst (worstError (after 7.5 doubtedRows)) < 1
            && lostCount doubtedRows == 0
    checkShowing "said to a developer" (concat (take 1 [explainChosen chosen | (_, _, Tracked{outChosen = chosen@(Chosen Appeared _ _ _)}, _) <- after 6.5 doubtedRows])) $
        any ("afresh by layout " `isPrefixOf`) [explainChosen chosen | (_, _, Tracked{outChosen = chosen@(Chosen Appeared _ _ _)}, _) <- after 6.5 doubtedRows]

    -- A lamp coming on beside a glancing head: the clip continues as
    -- predicted throughout.
    let (_, lampRows) = tracking [(t, glancing t, renderDiscs ([((500, 100), 14) | t >= 1] ++ pointDiscs defaultIntrinsics clipBent allLit (glancing t))) | t <- at120 4]
    checkShowing "glancing beside a lamp, the clip continues as predicted" (show (length (ways (after 0.01 lampRows)))) $ null (ways (after 0.01 lampRows)) && lostCount lampRows == 0
