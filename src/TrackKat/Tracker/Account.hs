-- | The one account every hypothesis about a frame is scored by: where a
-- head can be, how it got there, and what the frame shows if it is.
--
-- See Note [One account].
module TrackKat.Tracker.Account (
    -- * What is known going into a frame
    Reach (..),
    Known (..),
    Origin (..),
    Origins (..),
    metres,
    expecting,

    -- * Hypotheses
    Candidate (..),
    Picking (..),
    Way (..),
    Guess (..),

    -- * Judging them
    Judged,
    judgedGuess,
    judgedScore,
    judgedFrame,
    judgedSeen,
    judgedFindings,
    Part (..),
    judgedParts,
    judgeClip,
    judgeAway,

    -- * The parts of the account
    whereItIs,
    Came (..),
    howItCame,
    room,
    roomAtMost,
    movedAtLeast,
    Room (..),
    followingRoom,
    anywhereRoom,
    turnSpread,
    moveSpread,
    asideSpread,
    nearest,
    farthest,
) where

import Data.Foldable (toList)
import Data.Maybe (catMaybes)

import TrackKat.Linear (V3, dot, logDeterminant, norm, normalize, (*^), (^-^))
import TrackKat.Pose
import TrackKat.Sight
import TrackKat.Term
import TrackKat.Tracker.Presence
import TrackKat.Tracker.Reflectors
import TrackKat.Tracker.Sizes
import TrackKat.World (Instant, Seconds, World (..), during, frameTime, inDegrees, inMillimetres, inPixels, world, (.-.))

-- Note [One account]
-- ~~~~~~~~~~~~~~~~~~
-- A frame is some blobs. What to make of it is a choice among hypotheses:
-- that the clip is at some pose, these blobs its reflectors and the rest
-- strays, having continued from where it was or come into view afresh; or
-- that it isn't to be seen at all, and every blob is a stray. Each is
-- scored the same way, by how much likelier it makes the frame than every
-- blob being a stray with no clip to explain any, in nats (see
-- "TrackKat.Term"), and the best scored is taken
-- ("TrackKat.Tracker.Choose"). The score is a sum of three parts, each a
-- sum of named terms, and nothing else is added to it anywhere: a
-- 'Judged' hypothesis is only made here.
--
--  * Where the head is ('whereItIs'). A pose outside the camera's range
--    can't be the clip's: those are the only gates, being what the camera
--    can't show. Otherwise a head is likelier near where it was
--    recentered: turned from it by the world's 'headTurnSpread', and aside
--    of it by half its 'headReachAside', the farthest a head goes being
--    two spreads.
--
--  * How it got there ('howItCame'). Continuing a pose from a moment
--    before, it turned and moved as far as it did in that time, each a
--    gaussian of the world's 'headTurnSpeed' and 'headMoveSpeed' over the
--    time. Coming into view afresh, nothing says where. Either way the
--    pose is one the frame picked out of all those it could have been,
--    and the more there were to pick from the less likely any one: that
--    is 'room', below. And the clip is in view where it was followed,
--    which is likely a moment after it was made out; or it has left where
--    it was followed, which is as unlikely then as its going from view
--    (see Note [Coming and going] in "TrackKat.Tracker.Presence", and
--    'comeAfresh').
--
--  * What the frame shows, given the pose ('whatShows', in
--    "TrackKat.Tracker.Reflectors"): each reflector's blob in its place and of
--    its size, or its being out of sight.
--
-- The clip being away ('judgeAway') is scored by the same parts: where
-- it was, if a pose still says; that it has gone from view since it was
-- made out; and of what the frame shows nothing, every blob being a
-- stray.
--
-- Room. A hypothesis that says "the head is somewhere near where it was"
-- and one that says "the head is somewhere in the room" both end up
-- naming one pose, the one that fits the blobs. They aren't equally
-- likely for it: the second had far more poses to choose from, and would
-- have fitted almost any three blobs. What a hypothesis is worth is its
-- likelihood summed over every pose it allows, and to a good
-- approximation that is the likelihood at the best pose times the share
-- of the allowed poses that fit as well: the volume the blobs leave over
-- the volume there was. The log of that share is 'room'. It is worked out
-- from how far each of a pose's six freedoms could have gone (the spreads
-- of 'followingRoom' or 'anywhereRoom') and how finely the reflectors
-- seen fix each ('poseInformation'), and needs no tuning: it is some 6
-- nats a frame after a pose and some 25 with nothing known, and the 19
-- between them, with what it costs the clip to have left where it was
-- ('comeAfresh'), is all that makes a pose that continues the last
-- likelier than the same pose come afresh. A reflector unseen fixes less,
-- so costs less room: what the frame can't say stays where it was.

-- | What says where a head can be.
data Reach = Reach
    { reachPivot :: V3
    -- ^ the point of the clip's axes the head turns about
    , reachCentre :: Maybe Pose
    -- ^ the clip's pose at the last recenter
    }

-- | What is known of the camera and the clip going into a frame.
data Known = Known
    { knownCamera :: Intrinsics
    , knownClip :: Model
    , knownReach :: Reach
    , knownSizes :: Three Size
    -- ^ what is learnt of each reflector's size, as of when the clip was
    -- last made out
    , knownShowed :: Maybe (Three Presence, Instant)
    -- ^ whether each reflector showed when the clip was last made out,
    -- and when that was
    , knownMadeOut :: Maybe Instant
    -- ^ when the clip was last made out, if ever
    , knownDoubt :: Double
    -- ^ how much likelier the frames have lately been with the clip's
    -- reflectors seen elsewhere than where they were followed, in nats
    -- (see Note [Doubt] in "TrackKat.Tracker.Choose")
    , knownLast :: Maybe Pose
    -- ^ the clip's pose as last tracked, however long ago
    }

-- | A pose a hypothesis may continue, and how long before the frame it
-- was.
data Origin = Origin
    { originPose :: Pose
    , originSince :: Seconds
    }

-- | The poses a frame's hypotheses may continue: the last tracked, while
-- it predicts, and one left a moment ago for a pose found afresh.
data Origins = Origins
    { originLast :: Maybe Origin
    , originLeft :: Maybe Origin
    }

-- | How far a pose is from the camera, in metres.
metres :: Pose -> Double
metres pose = norm (poseT pose) / 1000

-- | What is expected of the reflectors in a frame at a time, were the
-- clip at a pose: where each is, and what is learnt of how big it is and
-- whether it shows, as of then.
expecting :: Known -> Instant -> Pose -> Expect
expecting known now pose =
    Expect
        { expectAt = seenAt (knownCamera known) (knownClip known) pose <$> reflectors
        , expectMetres = metres pose
        , expectSizes = maybe id (\(_, at) -> fmap (faded (now .-. at))) (knownShowed known) (knownSizes known)
        , expectShowed = (\(showed, at) -> (showed, max (frameTime world) (now .-. at))) <$> knownShowed known
        }

-- | The clip made out of a frame's blobs: at a pose, each reflector seen
-- as given.
data Candidate = Candidate
    { candidatePose :: Pose
    , candidateFit :: Double
    -- ^ RMS reprojection error, pixels
    , candidateSights :: Three (Maybe Sight)
    , candidateStrays :: Int
    -- ^ blobs in view that aren't its reflectors
    , candidateMasked :: Int
    -- ^ blobs ignored as part of a still light
    , candidatePicking :: Picking
    }

-- | How a candidate's blobs were picked out.
data Picking
    = -- | near where a pose predicts the reflectors
      ByPrediction
    | -- | by their layout, three of the heaviest
      ByLayout
    deriving (Eq, Show)

-- | How the clip came to be where a hypothesis has it.
data Way
    = -- | from where it was last tracked, a moment ago
      Continued
    | -- | from where it was tracked a moment before a pose found afresh
      -- elsewhere
      Resumed
    | -- | into view, with nothing said of where it was
      Appeared
    deriving (Eq, Show)

-- | A hypothesis about a frame, in brief: the clip made out, or away.
data Guess = InView Way Picking | OutOfView
    deriving (Eq, Show)

-- | A part of the account (see Note [One account]).
data Part = Where | How | Shows
    deriving (Eq, Show)

-- | A hypothesis as the account scores it. There is no other way to make
-- one than 'judgeClip' and 'judgeAway'.
data Judged = Judged
    { judgedGuess :: Guess
    , judgedScore :: !Score
    , judgedFrame :: !Score
    -- ^ what of the score is the frame's own: where the pose is, what
    -- shows, and how far it turned and moved if it continues a pose,
    -- without the room it had or whether the clip was in view
    , judgedSeen :: Maybe Candidate
    -- ^ what it makes of the frame, if it makes out the clip
    , judgedParts :: [(Part, [Finding])]
    -- ^ what each term found, part by part; worked out only if looked at
    }

-- | What each term of a hypothesis's score found.
judgedFindings :: Judged -> [Finding]
judgedFindings = concatMap snd . judgedParts

-- | The nearest and farthest, in mm, the clip can be from the camera: the
-- world's 'cameraNearest' and 'cameraFarthest'.
nearest, farthest :: Double
nearest = inMillimetres (cameraNearest world)
farthest = inMillimetres (cameraFarthest world)

-- | The spread, in mm, of how far to the side, or up or down, of where it
-- was recentered a head is: half the world's 'headReachAside', the
-- farthest a head goes being two spreads.
asideSpread :: Double
asideSpread = inMillimetres (headReachAside world) / 2

-- | Where a head can be: in the camera's range, and likelier the nearer
-- where it was recentered, in turning and to the side (toward or away
-- from the camera it goes as it likes: people lean in, and sit back after
-- a break).
whereItIs :: Reach -> Term Pose
whereItIs (Reach pivot centre) =
    within
        ( Name "near" "too close to the camera"
        , Name "far" "too far from the camera"
        )
        "mm"
        (nearest, farthest)
        (norm . poseT)
        <> maybe mempty reach centre
  where
    reach ref =
        gaussian (Name "turned" "turned further from where you recentered than heads mostly turn") "degrees" (inDegrees (headTurnSpread world)) (\pose -> fst (poseChange pivot ref pose))
            <> gaussian (Name "aside" "farther to the side of where you recentered than heads mostly go") "mm" asideSpread (aside ref)
    aside ref pose =
        let fromCamera = normalize (poseAt ref pivot)
            shifted = poseAt pose pivot ^-^ poseAt ref pivot
            back = dot shifted fromCamera
         in norm (shifted ^-^ back *^ fromCamera)

-- | The spreads of how far a head turns (degrees) and its pivot moves (mm)
-- in the given time: the world's 'headTurnSpeed' and 'headMoveSpeed' for
-- that long, or for a frame if that is longer.
turnSpread, moveSpread :: Seconds -> Double
turnSpread dt = inDegrees (headTurnSpeed world `during` max dt (frameTime world))
moveSpread dt = inMillimetres (headMoveSpeed world `during` max dt (frameTime world))

-- | How far each of a pose's freedoms could have gone, as spreads: in
-- turning, each of three ways, in degrees; to the side and up or down, in
-- mm; and toward or away from the camera, in mm.
data Room = Room
    { roomTurn :: Double
    , roomAside :: Double
    , roomAlong :: Double
    }
    deriving (Eq, Show)

-- | The room a head has with nothing known of where it was: the spreads
-- of 'whereItIs', and along the line of sight the spread of a gaussian as
-- high at its peak as the camera's range is long.
anywhereRoom :: Room
anywhereRoom = Room (inDegrees (headTurnSpread world)) asideSpread ((farthest - nearest) / sqrt (2 * pi))

-- | The room a head has that was at a pose so long before: 'turnSpread'
-- and 'moveSpread', and never more than it has anywhere.
followingRoom :: Seconds -> Room
followingRoom dt = Room (min (turnSpread dt) (roomTurn anywhereRoom)) (min (moveSpread dt) (roomAside anywhereRoom)) (min (moveSpread dt) (roomAlong anywhereRoom))

-- | The log of the share of the poses a head had room for that fit what
-- was seen as well as the one that fits best (see Note [One account]):
-- nothing if what was seen fixes nothing, and less the more it fixes of
-- what there was room for. Given what was seen as 'poseInformation' has
-- it.
room :: Room -> [[Double]] -> Double
room (Room turn aside along) information =
    negate (logDeterminant [[(if i == j then 1 else 0) + si * sj * x / (noise * noise) | (j, sj, x) <- zip3 [0 :: Int ..] spreads row] | (i, si, row) <- zip3 [0 ..] spreads information]) / 2
  where
    spreads = [turn, turn, turn, aside, aside, along]
    noise = inPixels (centroidNoise world)

-- | The most 'room' can come to for a pose so many metres from the
-- camera, whatever the pose, with reflectors seen that count so much
-- each (see 'sightWeight'): what is left of the room to the side and up
-- or down by reflectors no farther than the clip reaches beyond its
-- pose. Fixing the rest can only leave less.
roomAtMost :: Intrinsics -> Model -> Room -> Double -> [Double] -> Double
roomAtMost k m (Room _ aside _) z weights =
    negate (log (1 + (aside * focal k / ((1000 * z + reach) * noise)) ^ (2 :: Int) * sum (map (^ (2 :: Int)) weights)))
  where
    reach = maximum (map norm [mLeft m, mRight m, mTop m]) + tabRadius (mTabs m)
    noise = inPixels (centroidNoise world)

-- | The least 'howItCame' can take off for turning and moving, of a pose
-- that continues another from so long before, given the point the head
-- turns about, when it puts reflectors so far in the image from where the
-- other pose has them: for each, the pixels between, and its mm in front
-- of the camera at the other pose. A reflector is no farther from the
-- pivot than the clip reaches, so it moves no further than the head turns
-- about the pivot at that reach and the pivot moves; and it moves so far
-- in the image only by moving a share of its distance from the camera.
movedAtLeast :: Intrinsics -> Model -> V3 -> Seconds -> [(Double, Double)] -> Double
movedAtLeast k m pivot dt offs = maximum (0 : map least offs)
  where
    least (pixels, depth) =
        let shifted = max 0 (pixels * max 0 depth / (steepest * focal k + pixels) - 2 * tabRadius (mTabs m))
         in shifted * shifted / (2 * ((lever * turning) ^ (2 :: Int) + moveSpread dt ^ (2 :: Int)))
    lever = maximum [norm (p ^-^ pivot) | p <- [mLeft m, mRight m, mTop m]] + tabRadius (mTabs m)
    turning = min (turnSpread dt) (roomTurn anywhereRoom) * pi / 180
    -- How much further a point moves in the image, for its distance and
    -- how far it moves, off to the side than straight ahead: at most
    -- this, out to twice the picture's corners.
    steepest = sqrt (1 + (2 * 400 / focal k) ^ (2 :: Int))

-- | How a pose came to be, as 'howItCame' judges it.
data Came = Came
    { cameTurn :: Double
    -- ^ degrees turned from the pose continued, if one is
    , cameMove :: Double
    -- ^ mm its pivot moved from that pose's
    , cameRoom :: Double
    -- ^ what 'room' comes to for it
    }

-- | How likely the clip came to a pose in the given way: continuing a pose
-- from so long before, or with nothing known of where it was; and how
-- likely it is in view at all, given whether it was when last judged and
-- how long ago.
howItCame :: Maybe Seconds -> Double -> Maybe Seconds -> Term Came
howItCame continued doubt madeOut =
    maybe mempty moved continued
        <> likelihood (Name "room" "nothing says where the clip would be") cameRoom
        <> (if doubt > 0 then likelihood (Name "doubt" "the pose followed has fitted worse than another") (const doubt) else mempty)
        <> maybe (comeAfresh madeOut) (const (inView madeOut Present)) continued

-- | How likely a head turned and moved as far as a pose is from one it
-- continues, so long before.
moved :: Seconds -> Term Came
moved dt =
    gaussian (Name "turning" "the head would have turned faster than heads mostly turn") "degrees" (min (turnSpread dt) (roomTurn anywhereRoom)) cameTurn
        <> gaussian (Name "moving" "the head would have moved faster than heads mostly move") "mm" (moveSpread dt) cameMove

-- | How likely the clip is in view, or isn't, given how long ago it was
-- last made out, if ever: as it comes and goes over that time, or as it
-- usually is.
inView :: Maybe Seconds -> Presence -> Term a
inView madeOut to =
    likelihood
        ( case to of
            Present -> Name "in view" "the clip would have just come into view"
            Gone -> Name "out of view" "the clip would have just gone from view"
        )
        (const (maybe (usually clipComings to) (\since -> passing clipComings since Present to) madeOut))

-- | How likely the clip is in view with nothing said of where it was,
-- given how long ago it was last made out, if ever. Never made out, it is
-- as likely in view as it usually is. Made out, it isn't where it was
-- followed, which is as unlikely as its having gone from view since: for
-- a clip to be elsewhere is for it to have gone from where it was. (What
-- doubt there is of where it was followed is for 'howItCame' to add.)
comeAfresh :: Maybe Seconds -> Term a
comeAfresh Nothing = inView Nothing Present
comeAfresh (Just since) = likelihood (Name "left" "the clip would have just left where it was followed") (const (passing clipComings since Present Gone))

-- | How long before a time the clip was last made out, if ever: a frame
-- at the least.
lastMadeOut :: Known -> Instant -> Maybe Seconds
lastMadeOut known now = (\at -> max (frameTime world) (now .-. at)) <$> knownMadeOut known

-- | The clip made out as a candidate has it, judged as each way it could
-- have come there: continuing the last pose, if one predicts; resuming one
-- left behind, if one was; and afresh.
judgeClip :: Known -> Instant -> Origins -> Candidate -> [Judged]
judgeClip known now origins candidate =
    catMaybes
        [ judgedAs Continued <$> originLast origins
        , judgedAs Resumed <$> originLeft origins
        , Just (judged Appeared Nothing (Came 0 0 (room anywhereRoom information)))
        ]
  where
    pose = candidatePose candidate
    sights = candidateSights candidate
    reach = knownReach known
    expect = expecting known now pose
    whereTerm = whereItIs reach
    showsTerm = whatShows expect
    placed = score whereTerm pose
    showing = score showsTerm sights
    information =
        poseInformation
            (knownCamera known)
            (knownClip known)
            (reachPivot reach)
            pose
            [Observation which (sightWeight (sizeAt (expectMetres expect) size) s) (sightAt s) | (which, size, Just s) <- toList ((,,) <$> reflectors <*> expectSizes expect <*> sights)]
    judgedAs way (Origin from since) =
        let (turn, move) = poseChange (reachPivot reach) from pose
         in judged way (Just since) (Came turn move (room (followingRoom since) information))
    judged way since came =
        let howTerm = howItCame since (if way == Appeared && all (maybe False ((== Whole) . sightKind)) sights then knownDoubt known else 0) (lastMadeOut known now)
            total = placed `plus` score howTerm came `plus` showing
            kept = placed `plus` showing `plus` maybe possible (\dt -> score (moved dt) came) since
         in Judged
                (InView way (candidatePicking candidate))
                total
                kept
                (Just candidate)
                [(Where, findings whereTerm pose), (How, findings howTerm came), (Shows, findings showsTerm sights)]

-- | The clip away, or anyhow not to be made out, and every blob a stray:
-- where it was, if the given pose still says, and that it went from view
-- or stayed away.
judgeAway :: Known -> Instant -> Maybe Pose -> Judged
judgeAway known now from =
    Judged OutOfView (maybe possible (score whereTerm) from `plus` score howTerm ()) (maybe possible (score whereTerm) from) Nothing [(Where, maybe [] (findings whereTerm) from), (How, findings howTerm ())]
  where
    whereTerm = whereItIs (knownReach known)
    howTerm = inView (lastMadeOut known now) Gone

plus :: Score -> Score -> Score
plus (Score a) (Score b) = Score (a + b)
