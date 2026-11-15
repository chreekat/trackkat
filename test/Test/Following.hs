-- | Following the clip and finding it by layout, weighed against each other
-- in the same frame: a pose following can't continue costs a few frames and
-- not half a second, a pose followed onto a wrong blob gives way to a better
-- one found by layout, and what goes by continuity carries on for a pose
-- found where the clip was, and starts over for one found elsewhere.
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
    let ways rows = [(t, way) | (t, _, Tracked{outChosen = Chosen way (Just _)}, _) <- rows]
        windowsOf what tracker = concat [Term.tallyCounts tally | (name, tally) <- trackerWindows tracker, name == what]
        taken what tracker = sum [n | ("taken", n) <- windowsOf what tracker]
        glancing t = headSeen bentWorn (25 * sin (2 * pi * t / 2.5)) (10 * sin (2 * pi * t / 3.7)) 0
        tracking frames = let (tracker, rows) = trackFramesEvents clipBent frames in (tracker, map fst rows)
        (undisturbed, _) = tracking [(t, glancing t, renderDiscs (pointDiscs defaultIntrinsics clipBent allLit (glancing t))) | t <- at120 9]

    -- The scores hypotheses are compared by: a frame after a pose, finding
    -- afresh gives up some 22 nats to following.
    let judgedBy name rows = [nats (findingScore f) | (_, _, Tracked{outJudged = judged}, _) <- rows, f <- judged, brief (findingName f) == name]

    -- A fast turn, 200 degrees a second, with one frame showing the clip
    -- where it was 15 degrees back: that frame is followed, and from its
    -- pose the next is an impossible jump, the head turning on faster
    -- than the allowance for a jump grows. Following used to be all that
    -- was tried until the pose stopped predicting; now the clip is found
    -- by layout as soon as that is likelier than its being where it was.
    let turning t = headSeen bentWorn (-40 + 200 * max 0 (min 0.4 (t - 1))) 0 0
        stale = trackFrames clipBent [(t, turning t, renderDiscs (pointDiscs defaultIntrinsics clipBent allLit (turning (if i == 150 then t - 0.075 else t)))) | (i, t) <- zip [0 :: Int ..] (at120 3)]
    checkShowing "a frame followed to a wrong pose in a fast turn costs" (show (lostCount stale) ++ " frames, then found afresh " ++ show (take 1 (ways (after 1.25 stale)))) $
        lostCount stale <= 3
            && map snd (take 1 (ways (after 1.25 stale))) == [FoundAfresh]
            && fst (worstError (after 1.3 stale)) < 1
    checkShowing "finding afresh a frame on gives up" (show (take 1 (judgedBy "afresh" (after 1.25 stale))) ++ " nats") $
        case judgedBy "afresh" (after 1.25 stale) of
            cost : _ -> cost < -10 && cost > -23
            [] -> False

    -- A head jumping 40 degrees: found afresh there, and what was known
    -- of the mirror image starts over, so fewer windows of turning teach
    -- the pivot than undisturbed.
    let leaping t = if t < 6 then glancing t else headSeen bentWorn (40 + 25 * sin (2 * pi * t / 2.5)) (10 * sin (2 * pi * t / 3.7)) 0
        (leapt, leaptRows) = tracking [(t, leaping t, renderDiscs (pointDiscs defaultIntrinsics clipBent allLit (leaping t))) | t <- at120 9]
    checkShowing "a jump found afresh" (show (ways leaptRows) ++ " starts the mirror image's belief over: windows for the pivot " ++ show (taken "pivot" leapt, taken "pivot" undisturbed)) $
        map snd (ways leaptRows) == [FoundAfresh]
            && lostCount leaptRows <= 3
            && taken "pivot" leapt < taken "pivot" undisturbed

    -- The clip seen elsewhere for three frames, none of its reflectors
    -- where they were, and then back: tracked there, and resumed where it
    -- was with what was known of the mirror image kept, as when those
    -- frames were lost.
    let elsewhere = let Pose r p = headSeen bentWorn 40 0 0 in Pose r (p ^+^ V3 150 0 0)
        shown i t = if i >= 720 && i < 723 then elsewhere else glancing t
        (visited, visitedRows) = tracking [(t, shown i t, renderDiscs (pointDiscs defaultIntrinsics clipBent allLit (shown i t))) | (i, t) <- zip [0 :: Int ..] (at120 9)]
    checkShowing "the clip elsewhere for three frames and back" (show (ways visitedRows) ++ " keeps the mirror image's belief: windows for the pivot " ++ show (taken "pivot" visited, taken "pivot" undisturbed)) $
        map snd (ways visitedRows) == [FoundAfresh, Resumed]
            && lostCount visitedRows == 0
            && taken "pivot" visited >= taken "pivot" undisturbed - 1
            && taken "mirror image" visited >= taken "mirror image" undisturbed - 1
            && fst (worstError visitedRows) < 1

    -- One frame of the clip 40 degrees away, two of its reflectors'
    -- places still lit: likelier where it was, a reflector out of sight,
    -- and lost for that frame, as it always was.
    let (_, blinkRows) = tracking [(t, glancing t, renderDiscs (pointDiscs defaultIntrinsics clipBent allLit (if i == 720 then headSeen bentWorn 40 0 0 else glancing t))) | (i, t) <- zip [0 :: Int ..] (at120 7)]
        heldBack = [held | (_, _, Lost{outHeld = Just held}, _) <- blinkRows]
    checkShowing "a frame of the clip turned away with reflectors still in their places is lost, not found there" (show (map explainHeld heldBack)) $
        lostCount blinkRows == 1
            && null (ways blinkRows)
            && and [found < kept && missing == 1 | Held found missing kept <- heldBack]
            && length heldBack == 1
            && fst (worstError blinkRows) < 1

    -- Dim reflectors, half their usual width. As the top one goes dark
    -- for six frames, a light three quarters its width comes on 8 pixels
    -- from it: following takes the light for the reflector, a blob being
    -- there and big enough, and keeps to it when the reflector shows
    -- again, since the light is where the pose predicts the reflector.
    -- Found by layout, the three reflectors are the sizes they were, and
    -- that outscores following: the clip is refound, and since the pose
    -- continues the one followed, what is known of the mirror image is
    -- kept. Following alone stayed on the light.
    let dim lit pose = [(p, 0.5 * r) | (p, r) <- pointDiscs defaultIntrinsics clipBent lit pose]
        (topAt, topWidth) = dim allLit (glancing 6) !! 2
        light t = [((fst topAt + 8, snd topAt), 0.74 * topWidth) | t >= 6]
        (dimUndisturbed, _) = tracking [(t, glancing t, renderDiscs (dim allLit (glancing t))) | t <- at120 9]
        (caught, caughtRows) = tracking [(t, glancing t, renderDiscs (light t ++ dim (1, 1, if t >= 6 && t < 6.05 then 0 else 1) (glancing t))) | t <- at120 9]
        outscored = [(t, way, won, lost) | (t, _, Tracked{outChosen = Chosen way (Just (Tried (Outscored won lost) _))}, _) <- caughtRows, way /= Followed]
    checkShowing "followed onto a light by a reflector gone dark" (show (worstError [r | r@(t, _, _, _) <- caughtRows, t >= 6, t < 6.05]) ++ ", refound when the reflector shows " ++ show outscored) $
        fst (worstError [r | r@(t, _, _, _) <- caughtRows, t >= 6, t < 6.05]) > 5
            && [(way, t < 6.06, won > lost) | (t, way, won, lost) <- outscored] == [(Refound, True, True)]
            && fst (worstError (after 6.06 caughtRows)) < 1
            && lostCount caughtRows == 0
    checkShowing "and refound keeps the mirror image's belief: windows" (show ((taken "mirror image" caught, taken "pivot" caught), (taken "mirror image" dimUndisturbed, taken "pivot" dimUndisturbed))) $
        taken "mirror image" caught == taken "mirror image" dimUndisturbed
            && taken "pivot" caught >= taken "pivot" dimUndisturbed - 1
    checkShowing "said to a developer" (concat (take 1 [said | (_, _, Tracked{outChosen = chosen}, _) <- caughtRows, Just said <- [explainChosen chosen]])) $
        any ("refound: following the clip scored " `isPrefixOf`) [said | (_, _, Tracked{outChosen = chosen}, _) <- caughtRows, Just said <- [explainChosen chosen]]

    -- A lamp coming on beside a glancing head: following is never weak,
    -- so nothing is looked for by layout.
    let (_, lampRows) = tracking [(t, glancing t, renderDiscs ([((500, 100), 14) | t >= 1] ++ pointDiscs defaultIntrinsics clipBent allLit (glancing t))) | t <- at120 4]
    checkShowing "glancing beside a lamp, only following is tried" (show (length (ways lampRows))) $ null (ways lampRows) && lostCount lampRows == 0
