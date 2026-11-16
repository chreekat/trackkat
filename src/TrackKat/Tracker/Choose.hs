-- | Choosing what to make of a frame: every hypothesis about it scored by
-- the one account, and the best taken. See Note [Choosing].
module TrackKat.Tracker.Choose (
    best,
    hypotheses,
    doubted,
    makeOut,
    maskedClip,
) where

import Data.Foldable (toList)
import Data.List (sortOn)

import TrackKat.Frame (Blob)
import TrackKat.Linear (V3 (..))
import TrackKat.Match (Kind (..), Sight (..), reflectors)
import TrackKat.Pose (poseAt, reflectorPoint, seenAt)
import TrackKat.Term
import TrackKat.Tracker.Account
import TrackKat.Tracker.Blobs (Shown (..))
import TrackKat.Tracker.Follow
import TrackKat.Tracker.Layout
import TrackKat.Tracker.Outcome
import TrackKat.Tracker.Sighting
import TrackKat.Tracker.Trail
import TrackKat.World (Instant, World (..), inPixels, world, (.-.))

-- Note [Choosing]
-- ~~~~~~~~~~~~~~~
-- A frame's hypotheses ('hypotheses') are all of one family, scored by one
-- account (see Note [One account] in "TrackKat.Tracker.Account"):
--
--  * the clip made out near where the last pose predicts its reflectors,
--    for each of the best ranked matches of blobs to them ('followed');
--
--  * the clip made out by layout, for each three of the heaviest few blobs
--    in view ('laidOut');
--
--  * each of those as having come there each way it could: continuing
--    the last pose, resuming one left a moment ago for a pose found
--    afresh, or afresh, with nothing said of where it was;
--
--  * and the clip away, every blob a stray.
--
-- The best scored is what the frame is taken to show ('best'), and the
-- rest, with their scores, are the explanation ('Rival'). Nothing is
-- chosen in any other way: no hypothesis is preferred for how it was come
-- by, none is tried only when another fails, and none is taken whatever
-- it scores. A hypothesis that can't be is one that scores impossible,
-- and the only terms that say so are the camera's range.
--
-- How many there are is bounded by how many are worth a solve: up to
-- 'followTries' matches and the ten threes of 'clipSearch' blobs, each
-- as up to three hypotheses, and one more. A three by layout that a
-- match already made out, each blob the same reflector's, at a pose that
-- fits, is the same hypothesis and isn't solved twice.
--
-- Two rules stand outside the scores, on what is told. One is how sure
-- a pose come afresh must be ('surely'). The other is 'partlySeen'. The
-- clip with a reflector out of sight is as likely there after ten
-- seconds as after one; what wears out is how well two reflectors say
-- where the head is, since they leave it free to turn about the line
-- through them. So when the best hypothesis is of the clip showing less
-- than all three for longer than it may, or come afresh showing less, no
-- pose is told and the frame is lost, saying so. That is a rule and not
-- a term because the scores hold no memory of how sure the pose was a
-- frame ago: see docs/heuristics.md for what would make it one.
--
-- What continues. A pose that continues the last carries on the belief
-- about the mirror image and its window of turning (see
-- "TrackKat.Branch"), the time tracked that lets lights be learnt, and
-- what learns from poses. One come afresh starts them over, and the pose
-- it left isn't forgotten at once ('LeftBehind'): a frame that resumes it
-- takes up what was believed of it, as if the frames between had been out
-- of sight, so that a brief wrong sight doesn't cost what was known.

-- Note [Doubt]
-- ~~~~~~~~~~~~
-- Each frame is scored by itself, and a pose that continues the last has
-- some 20 nats over the same pose come afresh for the room it had (see
-- 'room'), and more for the clip not having left where it was
-- ('comeAfresh'). So a pose followed onto the wrong blobs, a lamp as big as
-- a reflector that went dark beside it, would be kept for as long as each
-- frame's evidence against it stayed under that: the three true
-- reflectors, in view the whole time and fitting better in every frame,
-- would lose every frame by what it costs them to be somewhere the last
-- pose doesn't say.
--
-- What a frame's evidence can't decide, several frames' can. 'doubted'
-- keeps one running sum from frame to frame: by how many nats the frames
-- have been likelier with the clip's three reflectors seen as some three
-- blobs than as the pose followed sees them, counting only what is the
-- frame's own ('judgedFrame': where the pose is, what shows, and for the
-- pose followed how far it had to turn and move, not the room it had).
-- Each frame adds what the best such rival has over the pose followed,
-- which is mostly less than nothing, and the sum is never let below
-- nothing: it is the cumulative sum of a sequential test for a change
-- (Page's), whose forgetting is that floor and nothing tuned. It starts
-- over when there are no three blobs to weigh against the pose followed,
-- when a pose is taken afresh, and when the clip isn't made out. A pose
-- come afresh as three blobs is credited with it ('howItCame', as
-- "doubt"): once the frames have said for long enough that the clip is
-- elsewhere, elsewhere wins.
--
-- It takes the rival of one frame for the rival of the frames before,
-- which nothing checks: a second pose followed beside the first, with its
-- own continuity, would (see docs/heuristics.md).

-- | The doubt of the pose followed after a frame (see Note [Doubt]),
-- given the doubt before it, the hypothesis taken and the others.
doubted :: Double -> Judged -> [Judged] -> Double
doubted before won others = case (judgedGuess won, judgedSeen won) of
    (InView way _, Just followedAs)
        | way /= Appeared ->
            case [nats (judgedFrame j) | j <- others, Just c <- [judgedSeen j], threeWhole c, places c /= places followedAs] of
                [] -> 0
                rivals -> max 0 (before + maximum rivals - nats (judgedFrame won))
    _ -> 0
  where
    threeWhole c = all (maybe False ((== Whole) . sightKind)) (candidateSights c)
    places c = fmap (fmap sightAt) (candidateSights c)

-- | How much likelier than the clip being away, in nats, a pose come
-- afresh must be to be taken: nine times. A pose taken afresh is
-- recentered on, if nothing was, and teaches the reflectors' sizes, so one
-- taken wrongly costs more than a frame's wait for a surer one; and one
-- frame of three blobs laid out like the clip says only so much for it,
-- some five nats at the least when it is the clip, so nine times is about
-- as sure as can be asked.
sureness :: Double
sureness = log 9

-- | A pose come afresh is taken only if so much likelier than the clip
-- being away ('sureness').
surely :: Term Double
surely = atLeast (Name "sure" "the lights in view are no surer the clip than not") "nats" sureness id

-- | The best scored of some things, and the rest best first: the first
-- of those that score highest, of those that can be at all. Nothing if
-- none can.
best :: (a -> Score) -> [a] -> Maybe (a, [a])
best scored xs = case sortOn (negate . nats . scored) (filter can xs) of
    top : rest -> Just (top, rest)
    [] -> Nothing
  where
    can x = let s = nats (scored x) in s == s && not (isImpossible (scored x))

-- | How far below the best scored hypothesis so far the most a way of
-- making out the clip could score must be for it not to be solved, in
-- nats: the most is reckoned at a few distances, and the pose solved may
-- put the clip between them.
boundSlack :: Double
boundSlack = 2

-- | Every hypothesis about a frame's blobs at the given time worth
-- scoring, given the poses that still predict then (see Note [Choosing]),
-- each as the account judges it, the clip away first.
--
-- A way of making out the clip is solved for its pose, and judged, only
-- if the most it could score, every reflector just where it was seen and
-- nothing against where it is or how it came there, is within
-- 'boundSlack' of the best scored so far: so none is left out that could
-- have been the best.
hypotheses :: Known -> Instant -> Trail -> Shown -> [Judged]
hypotheses known now (Trail following left) shown = away : go (judgedScore away) [] (predicted ++ byLayout)
  where
    away = judgeAway known now (recentPose <$> following)
    origin r = Origin (recentPose r) (now .-. recentTime r)
    origins = Origins (origin <$> following) ((\(LeftBehind was _) -> origin was) <$> left)
    predicted = maybe [] (\r -> [(sights, bound, solved) | (sights, bound, solved) <- followed known now r shown]) following
    byLayout = [(laidSights laid, laidBound laid, laidClip laid) | laid <- laidOut known now (maybe TryMasked (const LeaveMasked) following) shown]
    -- The most a way by layout could score: come afresh, or continuing
    -- either pose, less the least that costs it for how far from where
    -- that pose has them it sees the reflectors.
    laidBound laid = maximum (laidAfresh laid : [Score (nats (laidAtBest laid) - continuing from (laidSights laid)) | Just from <- [originLast origins, originLeft origins]])
    continuing (Origin from since) sights =
        movedAtLeast
            (knownCamera known)
            (knownClip known)
            (reachPivot (knownReach known))
            since
            [ (sqrt ((u - x) ^ (2 :: Int) + (v - y) ^ (2 :: Int)), depth)
            | (which, Just s) <- toList ((,) <$> reflectors <*> sights)
            , let (u, v) = seenAt (knownCamera known) (knownClip known) from which
                  (x, y) = sightAt s
                  V3 _ _ depth = poseAt from (reflectorPoint (knownClip known) which)
            ]
    go _ _ [] = []
    go incumbent solved ((sights, bound, candidate) : rest)
        | any (sameAs sights) solved = go incumbent solved rest
        | nats bound + knownDoubt known + boundSlack < nats incumbent = go incumbent solved rest
        | otherwise = case candidate of
            Just c ->
                let judged = judgeClip known now origins c
                 in judged ++ go (maximum (incumbent : filter (not . isNaN . nats) (map judgedScore judged))) (c : solved) rest
            Nothing -> go incumbent solved rest
    -- A way already solved that made out the same three blobs as the same
    -- reflectors, at a pose that fits them as well as one solved from
    -- them alone would have to: the same hypothesis.
    sameAs sights p =
        fmap (fmap sightAt) sights == fmap (fmap sightAt) (candidateSights p)
            && all (maybe False ((== Whole) . sightKind)) (candidateSights p)
            && all (maybe False ((== Whole) . sightKind)) sights
            && candidateFit p < inPixels (centroidNoise world)

-- | Make out the clip in a frame's blobs at the given time, given the
-- poses that still predict then: what the best scored hypothesis makes of
-- it, and how it was chosen; or, if that is the clip away, or a pose not
-- to be told, why, with every hypothesis' score (see Note [Choosing]);
-- and the doubt of the pose followed after the frame (see Note [Doubt]).
makeOut :: Known -> Instant -> Trail -> Shown -> (Double, Either (Why, [Rival]) (Chosen, Sighting))
makeOut known now trail@(Trail following left) shown = (,) (maybe 0 (uncurry (doubted (knownDoubt known))) chosen) $ case chosen of
    Just (won, others)
        | Just candidate <- judgedSeen won
        , InView way source <- judgedGuess won ->
            let pose = candidatePose candidate
                sights = candidateSights candidate
                expect = expecting known now pose
                allSeen = case way of
                    Continued -> recentAllSeen <$> following
                    Resumed -> (\(LeftBehind was _) -> recentAllSeen was) <$> left
                    Appeared -> Nothing
                told = about fst (partlySeen expect now allSeen) <> about snd (if way == Appeared then surely else mempty)
                lead = oddsNats (judgedScore won `over` maximum (impossible : [judgedScore j | j <- others, judgedGuess j == OutOfView]))
             in if accepts told (sights, lead)
                    then
                        Right
                            ( Chosen way source (judgedScore won) (map rival others)
                            , Sighting pose (candidateFit candidate) sights (candidateStrays candidate) (candidateMasked candidate) (judgedFindings won)
                            )
                    else Left (Why (looking way) (Unfit (rejections told (sights, lead))), map rival (won : others))
    Just (won, others) -> Left (unlikely others, map rival (won : others))
    Nothing -> Left (unlikely [], [])
  where
    judged = hypotheses known now trail shown
    chosen = best judgedScore judged
    rival j = Rival (judgedGuess j) (judgedScore j)
    looking way = if way == Appeared then Afresh else Following
    -- Why the clip isn't made out: what counted against the likeliest way
    -- of seeing it, or that there was none.
    unlikely others = case [(way, j) | j <- others ++ impossibles, InView way _ <- [judgedGuess j]] of
        (way, j) : _ -> Why (looking way) (Unfit (decisive (judgedFindings j)))
        [] -> case following of
            Just _ -> Why Following NothingMatches
            Nothing -> case rejections threeBlobs (length (shownVisible shown) + length (shownMasked shown)) of
                [] -> Why Afresh NoPoseFits
                few -> Why Afresh (Unfit few)
    impossibles = [j | j <- judged, isImpossible (judgedScore j)]

-- | The masked blobs the clip would be made out in, afresh, were the
-- given blobs all there is to go by: those the best scored hypothesis
-- takes for reflectors; none if that is the clip away.
maskedClip :: Known -> Instant -> Shown -> [Blob]
maskedClip known now shown = maybe [] (snd . fst) (best (judgedScore . fst) ((judgeAway undoubted now Nothing, []) : found))
  where
    undoubted = known{knownDoubt = 0}
    found = [(j, laidMasked laid) | laid <- laidOut undoubted now TryMasked shown, Just c <- [laidClip laid], j <- judgeClip undoubted now (Origins Nothing Nothing) c]
