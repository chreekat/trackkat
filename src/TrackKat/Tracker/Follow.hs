-- | Making out the clip by following a pose that still predicts where its
-- reflectors are ('follow'), and how likely the clip is still there when
-- the frame doesn't show it so ('stillThere').
--
-- Nothing is followed to a pose that 'followable' rejects, nor by a match
-- that shows less of the clip than it has for longer than it may.
module TrackKat.Tracker.Follow (
    follow,
    partlySeen,
    stillThere,
) where

import Data.Either (rights)
import Data.Foldable (toList)
import Data.List (sortOn)
import Data.Maybe (catMaybes)

import TrackKat.Frame (Blob)
import TrackKat.Match
import TrackKat.Pose
import TrackKat.Term
import TrackKat.Tracker.Blobs
import TrackKat.Tracker.Judge
import TrackKat.Tracker.Outcome
import TrackKat.Tracker.Sighting
import TrackKat.Tracker.Sizes
import TrackKat.Tracker.Trail (Recent (..))
import TrackKat.World (Instant, inSeconds, (.-.))

-- | Up to how many matches as good as the best are tried when the best
-- is an impossible jump.
jumpRetries :: Int
jumpRetries = 5

-- | The clip followed from a pose at the given time, with what it
-- scores: the best match of the blobs in view to the prediction, unless
-- its pose is an impossible jump; then, of the next few that see as many
-- reflectors, the one whose pose is nearest. (Fewer reflectors can always
-- be fitted by a pose near the last, so they would hide a real jump.)
--
-- When none is followed, the reason is the best match's; or, if there is
-- no match to try, that the best there would be shows too little of the
-- clip for too long, if there is one.
follow :: Known -> Instant -> Recent -> Shown -> Either Why (Score, Sighting)
follow known now r shown =
    case candidates of
        [] -> Left . Why Following $ case allMatches of
            c : _ -> Unfit (rejections stillShowing c)
            [] -> NothingMatches
        c : cs -> case attempt c of
            Right (_, s) -> Right s
            Left reason ->
                let alike = [a | a <- cs, seenCount a == seenCount c]
                 in case sortOn fst (rights (map attempt (take jumpRetries alike))) of
                        (_, s) : _ -> Right s
                        [] -> Left (Why Following reason)
  where
    k = knownCamera known
    m = knownClip known
    sizes = knownSizes known
    reach = knownReach known
    dt = now .-. recentTime r
    allMatches = matches k m (recentPose r) sizes (shownVisible shown)
    candidates = filter (accepts stillShowing) allMatches
    stillShowing = partlySeen sizes now r
    -- A match followed, with how big a jump it is beside the most a head
    -- makes; or why it isn't.
    attempt c = case solveNear k m (reachPivot reach) (recentPose r) (observations sizes (metres (recentPose r)) c) of
        Nothing -> Left NoPoseFits
        Just (pose, fit) ->
            let (turn, move) = poseChange (reachPivot reach) (recentPose r) pose
                jump = max (turn / maxTurn dt) (move / maxMove dt)
                moved = Moved pose fit turn move
                likely = blobsSay sizes pose (matchSights c)
             in if accepts (followable reach dt) moved
                    then
                        Right
                            ( jump
                            ,
                                ( Score (fromIntegral (3 - seenCount c) * nats unseenScore + nats (score (likely <> continuing dt) moved))
                                , Sighting pose fit (matchSights c) (matchUnused c) (length (shownMasked shown)) (matchJudged c ++ findings (followable reach dt <> likely <> continuing dt) moved)
                                )
                            )
                    else Left (Unfit (rejections (followable reach dt) moved))

-- | The clip is followed showing less than all three reflectors only for
-- so long since all three last showed (see 'showingSeconds'), given the
-- reflectors' sizes, the time, and the pose followed.
partlySeen :: Three Size -> Instant -> Recent -> Term Match
partlySeen sizes now r =
    gate
        (Name "partly seen" "a reflector has been out of sight for too long")
        (\c -> now .-. recentAllSeen r <= allowed c)
        (\c -> Measure (inSeconds (now .-. recentAllSeen r)) AtMost (inSeconds (allowed c)) "s")
  where
    allowed c = showingSeconds (showing sizes (metres (recentPose r)) (matchSights c))

-- | How many reflectors a match sees.
seenCount :: Match -> Int
seenCount c = length (catMaybes (toList (matchSights c)))

-- | What a match says of where the reflectors are, for the solver, given
-- their sizes and the distance in metres.
observations :: Three Size -> Double -> Match -> [Observation]
observations sizes z c = catMaybes (toList (observe <$> reflectors <*> sizes <*> matchSights c))
  where
    -- A dim reflector, smaller than usual, counts for less: part of it may
    -- be too dim to see, moving its centroid. One sharing a blob counts for
    -- half, its place being an estimate.
    observe which size = fmap $ \s ->
        let usual = sizeUsual size / (z * z)
            dim = if usual > 0 then min 1 (sightPixels s / usual) else 1
            shared = if sightKind s == Merged then 0.5 else 1
         in Observation which (dim * shared) (sightAt s)

-- | The clip still where a pose predicts it at the given time, though the
-- frame doesn't show it so (see Note [Following or finding afresh] in
-- "TrackKat.Tracker.Choose"): how many of its reflectors have no blob in
-- view near where they are predicted, and what that scores, each of them
-- out of sight; impossible once the clip has shown less than all three for
-- longer than it may.
stillThere :: Intrinsics -> Model -> Instant -> Recent -> [Blob] -> (Int, Score)
stillThere k m now r visible
    | now .-. recentAllSeen r > showingSeconds OneOut = (missing, impossible)
    | otherwise = (missing, Score (fromIntegral missing * nats unseenScore))
  where
    -- Reflectors with a blob near their place, but no more of them than
    -- there are such blobs: one blob isn't two reflectors seen.
    places = toList (seenAt k m (recentPose r) <$> reflectors)
    missing = 3 - min (length [() | q <- places, any (nearTo q) visible]) (length [() | b <- visible, any (`nearTo` b) places])
