-- | Making out the clip by the layout of the big blobs in a frame: with
-- nothing to go on ('findAfresh'), and as a hypothesis beside following
-- when a pose still predicts ('findHere').
--
-- Whichever blobs are tried, three are the clip only if 'asClip' says so:
-- nothing found here is at a pose, or of blobs, that 'clipLike' rejects.
module TrackKat.Old.Tracker.Layout (
    asClip,
    findAfresh,
    Layout (..),
    findHere,
) where

import Data.Either (isRight, lefts, rights)
import Data.Foldable (toList)
import Data.List (sortOn)

import TrackKat.Frame (Blob (..))
import TrackKat.Lights (heaviestIn)
import TrackKat.Old.Match
import TrackKat.Old.Tracker.Blobs
import TrackKat.Old.Tracker.Judge
import TrackKat.Old.Tracker.Outcome
import TrackKat.Old.Tracker.Sighting
import TrackKat.Old.Tracker.Sizes (everSeen)
import TrackKat.Old.Tracker.Trail (Recent (..))
import TrackKat.Pose
import TrackKat.Term
import TrackKat.World (Instant, (.-.))

-- | Up to how many of the heaviest big blobs, when there are more than
-- three, are searched for the only three laid out like the clip; and up
-- to how many of the heaviest blobs in masked areas are tried in place of
-- big blobs missing.
clipSearch, maskedSearch :: Int
clipSearch = 5
maskedSearch = 4

-- | The clip, if the given blobs are its three reflectors, laid out as
-- the clip is, at a pose a head can be at: the pose with the blobs taken
-- for each reflector, its RMS reprojection error in pixels, and what the
-- terms that say so found; or why they aren't. The pose is solved from
-- where the clip was last tracked, if anywhere.
asClip :: Known -> [Blob] -> Either Reason (Found, Double, [Finding])
asClip known three = case identify (knownClip known) at three of
    Nothing -> Left NoPoseFits
    Just (l, r, top) -> case solvePose (knownCamera known) (knownClip known) (knownLast known) (at l, at r, at top) of
        Nothing -> Left NoPoseFits
        Just (pose, fit) ->
            let found = Found pose (Three l r top)
                isClip = clipLike (knownReach known) (knownSizes known)
             in if accepts isClip found
                    then Right (found, fit, findings isClip found)
                    else Left (Unfit (rejections isClip found))

-- | With nothing to predict from, the clip as three big blobs by their
-- layout, and the masked blobs taken as reflectors. With fewer than three
-- big blobs unmasked, the heaviest in each of a few masked areas are
-- tried too, those that make the three most alike in size first: a clip
-- held still can be learnt as lights (see Note [Still lights] in
-- "TrackKat.Lights"). With more, the only three that would be taken for
-- the clip, if just three would.
--
-- When none is found, the reason is the three big blobs' own, if there
-- were just three; or else that there weren't.
findAfresh :: Known -> Shown -> Either Why (Sighting, [Blob])
findAfresh known (Shown areas hidden shown) = case rights attempts of
    found : _ -> Right found
    [] -> Left . Why Afresh $ case (rejections threeBig (length bigShown), lefts attempts) of
        ([], reason : _) -> reason
        (count, _) -> Unfit count
  where
    biggest = maximum (0 : map blobPoints shown)
    bigShown = bigAmong shown
    -- Three blobs to try, and those of them masked.
    choices = case compare (length bigShown) 3 of
        EQ -> [(bigShown, [])]
        LT -> [(bigShown ++ extra, extra) | extra <- sortOn alike (picks (3 - length bigShown) (take maskedSearch (filter (bigBeside biggest) (heaviestIn areas hidden))))]
        GT -> [(three, []) | Just three <- [onlyClipLike known shown]]
    alike extra = let ps = map blobPoints (bigShown ++ extra) in fromIntegral (foldr max 0 ps) / fromIntegral (max 1 (foldr min maxBound ps)) :: Double
    attempts = map attempt choices
    attempt (three, extra) = do
        (Found pose blobs, fit, judged) <- asClip known three
        Right (Sighting pose fit (whole <$> blobs) (length shown + length extra - 3) (length hidden - length extra) judged, extra)

-- | Of more than three big blobs, the only three of the heaviest few
-- that would be taken for the clip, if just three would: the clip beside
-- something else. Not before the clip has been seen: nothing then tells
-- it from a fire and its flames laid out like it.
onlyClipLike :: Known -> [Blob] -> Maybe [Blob]
onlyClipLike known bs
    | everSeen (knownSizes known)
    , length (bigAmong bs) > 3 = case filter (isRight . asClip known) (picks 3 (take clipSearch (bigAmong bs))) of
        [three] -> Just three
        _ -> Nothing
    | otherwise = Nothing

-- | Finding the clip afresh wants just three big blobs: with fewer, or
-- more, it is found only as 'findAfresh' says.
threeBig :: Term Int
threeBig =
    within
        ( Name "big blobs" "fewer than three lights big enough to be reflectors are in view"
        , Name "big blobs" "more than three lights are in view, and no three of them stand out as the clip"
        )
        ""
        (3, 3)
        fromIntegral

-- | The clip found by the layout of the big blobs, with a pose still
-- predicting.
data Layout a = Layout
    { layoutScore :: Score
    , layoutWay :: Way
    -- ^ how its pose stands to the pose predicting, and to one left behind
    , layoutJump :: [Finding]
    -- ^ what its jump was judged to be, against what following allows
    , layoutFound :: a
    -- ^ what it makes of the frame
    }
    deriving (Show)

-- | The clip by the layout of the big blobs in view at the given time,
-- with the first pose given still predicting, and perhaps one left behind
-- that does too; or why the blobs aren't the clip. Nothing if the blobs
-- don't allow trying: there aren't three big ones, or none is worth
-- trying against the score and sights given, which are following's.
-- Masked blobs aren't tried.
--
-- Of more than three big blobs, each three of the heaviest few that
-- would be taken for the clip is a hypothesis, and the best scored is the
-- one told: with a pose to go on, the sizes the reflectors had and where
-- the clip was say which three, where finding afresh without one wants
-- just one such three.
findHere :: Known -> Instant -> Recent -> Maybe Recent -> Shown -> Maybe (Score, Three (Maybe Sight)) -> Maybe (Either Reason (Layout Sighting))
findHere known now r left shown taken = case (lefts tried, sortOn (negate . nats . layoutScore) (rights tried)) of
    (_, best : _) -> Just (Right best)
    (reason : _, []) -> Just (Left reason)
    ([], []) -> Nothing
  where
    big = bigAmong (shownVisible shown)
    threes = case compare (length big) 3 of
        EQ -> [big]
        GT | everSeen (knownSizes known) -> picks 3 (take clipSearch big)
        _ -> []
    tried = [foundHere known now r left shown three | three <- threes, maybe True (worthTrying three) taken]
    -- Not the three blobs following took, which is the same hypothesis;
    -- nor three whose sizes alone, at the distance the clip was, score no
    -- better than following did, since where they put the clip can only
    -- cost them more.
    worthTrying three (followed, sights) =
        not (sightsOf three sights) && case identify (knownClip known) at three of
            Just (l, rt, top) -> score (usualSize (knownSizes known)) (recentPose r, whole <$> Three l rt top) > followed
            Nothing -> False

-- | Three blobs as the clip, with a pose still predicting, as 'findHere'
-- tells it.
foundHere :: Known -> Instant -> Recent -> Maybe Recent -> Shown -> [Blob] -> Either Reason (Layout Sighting)
foundHere known now r left shown three = do
    (Found pose blobs, fit, judged) <- asClip known three
    let dt = now .-. recentTime r
        reach = knownReach known
        sights = whole <$> blobs
        -- The pose as moved from another, so long before: whether
        -- that isn't an impossible jump, how likely it is, as continuing
        -- it or as anywhere, whichever makes it likelier, and what the
        -- jump was judged to be.
        from was since =
            let (turn, move) = poseChange (reachPivot reach) (recentPose was) pose
                moved = Moved pose fit turn move
                likelier = if score (continuing since) moved >= score (anywhere reach dt) moved then continuing since else anywhere reach dt
             in (accepts (jumpable since) moved, about (const moved) likelier, findings (jumpable since) moved)
        (way, prior, jump) = case (from r dt, (\was -> from was (now .-. recentTime was)) <$> left) of
            ((True, continued, jumped), _) -> (Refound, continued, jumped)
            (_, Just (True, continued, jumped)) -> (Resumed, continued, jumped)
            ((False, _, jumped), _) -> (FoundAfresh, anywhere reach dt, jumped)
        placed = Moved pose fit 0 0
        likely = blobsSay (knownSizes known) pose sights <> prior
    Right
        Layout
            { layoutScore = score likely placed
            , layoutWay = way
            , layoutJump = jump
            , layoutFound = Sighting pose fit sights (length (shownVisible shown) - 3) (length (shownMasked shown)) (judged ++ findings likely placed)
            }

-- | Whether the blobs are just those the sights take for the reflectors,
-- each one of its own.
sightsOf :: [Blob] -> Three (Maybe Sight) -> Bool
sightsOf three sights = and [or [sightKind s == Whole && sightAt s == at b | Just s <- toList sights] | b <- three]
