-- | Choosing what to make of a frame when a pose still predicts where the
-- clip is: the clip followed from it, found by the layout of the blobs,
-- or still where it was though the frame doesn't show it. See Note
-- [Following or finding afresh].
module TrackKat.Tracker.Choose (
    Hypotheses (..),
    followOrFind,
    weakScore,
    makeOut,
) where

import TrackKat.Term
import TrackKat.Tracker.Blobs (Shown (..))
import TrackKat.Tracker.Follow
import TrackKat.Tracker.Layout
import TrackKat.Tracker.Outcome
import TrackKat.Tracker.Sighting
import TrackKat.Tracker.Trail
import TrackKat.World (Instant)

-- Note [Following or finding afresh]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- With a pose from a moment ago, a frame's blobs can be taken for the clip
-- in two ways: by following, each reflector matched to a blob near where
-- that pose predicts it ('follow'); or by layout alone, the big blobs in
-- view taken for the clip as when there is no pose to go on ('findHere').
-- They are hypotheses about the same frame, scored alike, and the best
-- is taken. Following alone for as long as the pose predicts
-- ('predictionSeconds') would cost up to half a second out of sight
-- after a jump following won't make, as after a frame followed to a
-- wrong pose, and would leave a pose followed onto the wrong blobs there
-- while its gates let it.
--
-- What they are scored by, in nats (see "TrackKat.Term"):
--
--  * the blobs' own evidence ('blobsSay'): how far each blob taken for a
--    reflector is from the size its reflector has usually been
--    ('usualSize', as a match's terms for sizes score a size), a reflector
--    unseen (what a match's term for it costs), and how far the reflectors
--    are from where the pose puts them ('misfit', under the world's
--    'centroidNoise');
--
--  * where the pose is, given the last. For a pose that continues the
--    last, how likely a head turned and moved so far in the time
--    ('continuing': gaussians of the spreads of a head's speed, the
--    world's 'headTurnSpeed' and 'headMoveSpeed', over that time). For a
--    pose that doesn't, how likely a head is there with nothing known of
--    where it was ('anywhere': anywhere a head can be, turned from the
--    centre by the world's 'headTurnSpread', within 'headReachAside' to
--    either side and in the camera's range).
--
-- These are commensurable because each is the log of a probability density
-- over the same things: the blobs' places and sizes, given the pose, and
-- the pose's six freedoms, given the last pose or given nothing. A density
-- that says the pose is within a few degrees and mm is far higher at its
-- peak than one that says only that it is somewhere a head can be, by the
-- ratio of the volumes they spread over; 'afreshCost' is the log of that
-- ratio, some 22 nats a frame after the last pose and less the longer
-- after, and it is all the advantage following has. There is no bonus for
-- following besides: the two are taken as equally likely beforehand. A
-- match's term for the reflectors' places is left out of the score, being
-- the same continuity measured in the image before the pose is solved.
--
-- So following wins whenever both take the same blobs, and a pose found
-- by layout wins when following has to take a blob of the wrong size, or
-- leave a reflector unseen, or jump, to an extent that costs more than
-- the pose found does. The hard gates still veto either: 'followable' for
-- a followed pose, 'clipLike' for one found by layout.
--
-- A pose found by layout that is within the jump following allows
-- ('jumpable') continues the last all the same: it is the clip refound
-- ('Refound'), scored by the better of 'continuing' and 'anywhere', and
-- everything that goes by continuity carries on as if it had been
-- followed: the belief about the mirror image and its window of turning
-- (see "TrackKat.Branch"), the time tracked that lets lights be learnt,
-- and what learns from poses. Only one that isn't ('FoundAfresh') starts
-- them over: either pose may then be the mirror image.
--
-- When following is rejected, a pose found by layout that continues the
-- last is taken. One that doesn't has a third hypothesis to beat
-- ('stillThere'): that the clip is still where the last pose predicts it,
-- and the frame doesn't show it rightly. That scores what a match's term
-- for a reflector unseen costs, for each reflector with no blob near where
-- it is predicted. So blobs like the clip somewhere else altogether, none
-- where the reflectors were, are taken at once, at 22 nats against 54;
-- but two reflectors still in their places and a stray, as a reflector
-- fades for a moment, are not: the clip is likelier where it was, one
-- reflector out of sight, at 18 nats. This is waiting for the clip to
-- show again, as a hypothesis with a score: it holds while the pose
-- predicts and the clip may show less ('showingSeconds'), it gives way
-- as finding afresh gets cheaper with the time since the pose, a few
-- frames for a head that has really jumped, and the frame is lost
-- meanwhile, with what was passed over told ('Held').
--
-- A pose left for one found afresh elsewhere isn't forgotten at once
-- ('LeftBehind'). Blobs that look like the clip somewhere else for a
-- frame or two, none of the reflectors still in its place, are tracked;
-- but if the clip is then found by layout where the pose left behind
-- still predicts it
-- ('Resumed', scored as continuing that pose), that pose's continuity is
-- taken up again as if those frames had been out of sight: a brief wrong
-- sight doesn't cost what was known of the mirror image.
--
-- What following alone would protect is kept:
--
--  * A pose still predicts for 'predictionSeconds' out of sight, and a
--    frame where no hypothesis makes out the clip is lost with following's
--    reason, so a brief occlusion or merge is followed through
--    when the reflectors show again.
--
--  * The clip showing two reflectors, or one behind another
--    ('showingSeconds'), is followed for that long. A reflector
--    unseen costs 18 nats, less than finding afresh a frame on, so such a
--    pose is outscored only once it also jumps; and what would be found
--    instead, a stray taken for the third reflector, pays for its size.
--
--  * Blobs masked as still lights are never taken for the clip while a
--    pose predicts: only blobs in view are tried by layout, exactly three
--    big ones or the best scored three of a few. Masked ones are tried
--    only with no pose to go on.
--
-- Finding by layout costs a solve or several, so it isn't tried on every
-- frame: only when following is rejected, or scores below 'weakScore';
-- and then not of the very three blobs following took, which is the same
-- hypothesis, nor of three whose sizes alone score no better than
-- following did.
--
-- What the scores can't tell: a stray the size of a reflector, taken for
-- it. A blob's size is its only evidence of what it is, the sizes learnt
-- follow what is tracked within seconds ('sizeMemory', and the latest at
-- once), and each frame is scored by itself, with no memory of how well
-- the frames before fitted. So a pose followed onto such a stray is left
-- only when a gate rejects it.

-- | The score below which a followed pose is doubted, and the clip looked
-- for by layout in the same frame too (see Note [Following or finding
-- afresh]): what a head turning and moving at twice the spreads of its
-- speed scores, beyond which, as spreads are stated, little happens.
weakScore :: Score
weakScore = Score (negate (2 * (twice * twice / 2)))
  where
    twice = 2 :: Double

-- | The hypotheses about a frame, with a pose still predicting. Each but
-- the first is worked out only if looked at.
data Hypotheses a = Hypotheses
    { followed :: Either Why (Score, a)
    -- ^ the clip followed, with what it scores; or why it isn't
    , byLayout :: Maybe (Either Reason (Layout a))
    -- ^ the clip by the layout of the big blobs, or why they aren't it;
    -- nothing if the blobs don't allow trying
    , stillWhereItWas :: (Int, Score)
    -- ^ the clip where the pose predicts it, with so many reflectors out
    -- of sight, and what that scores
    }
    deriving (Show)

-- | What to make of a frame, of the hypotheses about it, and how that
-- was come by; or, if nothing, following's reason, with any pose found by
-- layout that was passed over (see Note [Following or finding afresh]).
--
-- Following is taken, and nothing else looked at, while it scores at
-- least 'weakScore'. Below that the better scored of it and the layout is
-- taken, a tie being following's. With following rejected, a layout that
-- continues the last pose is taken, and any other only if it scores
-- better than the clip being still where it was.
--
-- Nothing is taken that a hypothesis didn't give: the frame is made of
-- what following or the layout made of it, or of nothing.
followOrFind :: Hypotheses a -> Either (Why, Maybe Held) (Chosen, a)
followOrFind hypotheses = case followed hypotheses of
    Right (followScore, kept)
        | followScore >= weakScore -> Right (Chosen Followed Nothing, kept)
        | otherwise ->
            let keeping fate jump = Right (Chosen Followed (Just (Tried fate jump)), kept)
             in case byLayout hypotheses of
                    Nothing -> Right (Chosen Followed Nothing, kept)
                    Just (Left reason) -> keeping (Failed (Why Afresh reason)) []
                    Just (Right found)
                        | layoutScore found > followScore -> Right (taking found (Outscored (layoutScore found) followScore))
                        | otherwise -> keeping (Outscored followScore (layoutScore found)) (layoutJump found)
    Left why -> case byLayout hypotheses of
        Just (Right found)
            | layoutWay found == Refound || layoutScore found > held -> Right (taking found (Failed why))
            | otherwise -> Left (why, Just (Held (layoutScore found) missing held))
          where
            (missing, held) = stillWhereItWas hypotheses
        _ -> Left (why, Nothing)
  where
    taking found fate = (Chosen (layoutWay found) (Just (Tried fate (layoutJump found))), layoutFound found)

-- | Make out the clip in a frame's blobs at the given time, given the
-- poses that still predict then: by the better of following and finding
-- by layout if one does ('followOrFind'), and by layout alone if none does
-- ('findAfresh').
makeOut :: Known -> Instant -> Trail -> Shown -> Either (Why, Maybe Held) (Chosen, Sighting)
makeOut known now (Trail following left) shown = case following of
    Nothing -> either (\why -> Left (why, Nothing)) (\(found, _) -> Right (Chosen FoundAfresh Nothing, found)) (findAfresh known shown)
    Just r ->
        let tried = follow known now r shown
         in followOrFind
                Hypotheses
                    { followed = tried
                    , byLayout = findHere known now r ((\(LeftBehind was _) -> was) <$> left) shown (either (const Nothing) (\(scored, sighting) -> Just (scored, sightingSights sighting)) tried)
                    , stillWhereItWas = stillThere (knownCamera known) (knownClip known) now r (shownVisible shown)
                    }
