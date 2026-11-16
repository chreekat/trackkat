-- | The physical facts tracking's heuristics rest on, each stated once: how
-- a head moves, what the camera sees, how reflectors look.
--
-- See Note [One world].
module TrackKat.World (
    -- * Quantities
    Quantity,
    Deg,
    MM,
    Px,
    Sec,
    Per,
    Degrees,
    Millimetres,
    Pixels,
    Seconds,
    DegreesPerSecond,
    MillimetresPerSecond,
    FramesPerSecond,
    degrees,
    millimetres,
    pixels,
    seconds,
    perSecond,
    inDegrees,
    inMillimetres,
    inPixels,
    inSeconds,
    inPerSecond,
    times,
    during,

    -- * Moments
    Instant,
    clockAt,
    inClockSeconds,
    (.-.),
    (.+^),

    -- * The world
    World (..),
    world,
    frameTime,
) where

-- Note [One world]
-- ~~~~~~~~~~~~~~~~
-- Tracking is a pile of judgements: is this pose somewhere a head can be,
-- did the head just move faster than heads move, is this blob a reflector
-- or a lamp. Each needs a number, and several modules need the same fact
-- (how far a head turns, how much a blob's centre is out).
-- docs/heuristics.md lists every number.
--
-- 'World' states the primary facts once, with their units. A module's own
-- constant that is the same fact is that field; one that is a multiple of
-- it (a limit at three spreads, a cost of six noises squared) says so
-- where it is defined, so that changing the fact changes all that follows
-- from it. Constants that are a module's own business (how long it
-- remembers, how sure it must be) stay in the module.
--
-- Units are phantom types on a plain number, 'Quantity', with nothing
-- but what is needed to say a fact and take it apart again: the modules
-- compute in bare numbers, and unwrap a fact where they name it, so
-- nothing is computed here that a module might round differently.
--
-- The world is a value, not a class or a parameter threaded through
-- tracking: there is one, 'world'. When settings should come to change a
-- fact (a camera with another range, a user who turns further), tracking
-- takes a 'World' where it now names 'world'.

-- | A number of some unit.
newtype Quantity u = Quantity Double
    deriving (Eq, Ord)

instance Show (Quantity u) where
    showsPrec d (Quantity x) = showsPrec d x

-- | The unit of degrees of turning.
data Deg

-- | The unit of millimetres.
data MM

-- | The unit of pixels of the camera's image.
data Px

-- | The unit of seconds.
data Sec

-- | The unit of one unit per another.
data Per u t

-- | An angle.
type Degrees = Quantity Deg

-- | A distance in the world.
type Millimetres = Quantity MM

-- | A distance in the camera's image.
type Pixels = Quantity Px

-- | A length of time.
type Seconds = Quantity Sec

-- | How fast something turns.
type DegreesPerSecond = Quantity (Per Deg Sec)

-- | How fast something moves.
type MillimetresPerSecond = Quantity (Per MM Sec)

-- | Frames are counted, not measured: a bare number of them a second.
type FramesPerSecond = Quantity (Per () Sec)

-- | A bare number taken as so many degrees.
degrees :: Double -> Degrees
degrees = Quantity

-- | A bare number taken as so many mm.
millimetres :: Double -> Millimetres
millimetres = Quantity

-- | A bare number taken as so many pixels.
pixels :: Double -> Pixels
pixels = Quantity

-- | A bare number taken as so many seconds.
seconds :: Double -> Seconds
seconds = Quantity

-- | So much of something a second.
perSecond :: Double -> Quantity (Per u Sec)
perSecond = Quantity

-- | An angle as a bare number of degrees, to compute with.
inDegrees :: Degrees -> Double
inDegrees (Quantity x) = x

-- | A distance as a bare number of mm.
inMillimetres :: Millimetres -> Double
inMillimetres (Quantity x) = x

-- | A distance in the image as a bare number of pixels.
inPixels :: Pixels -> Double
inPixels (Quantity x) = x

-- | A time as a bare number of seconds.
inSeconds :: Seconds -> Double
inSeconds (Quantity x) = x

-- | A rate as a bare number of its unit a second.
inPerSecond :: Quantity (Per u Sec) -> Double
inPerSecond (Quantity x) = x

-- | A multiple of a quantity.
times :: Double -> Quantity u -> Quantity u
times k (Quantity x) = Quantity (k * x)

-- | How much a rate comes to over a time.
during :: Quantity (Per u Sec) -> Seconds -> Quantity u
during (Quantity rate) (Quantity t) = Quantity (rate * t)

-- Note [Moments and lengths of time]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A moment ('Instant') and a length of time ('Seconds') are both so many
-- seconds, and are different things: when a frame was seen, and how long
-- since the last. What makes sense of them is what the types allow. The
-- time from one moment to another is a length ('.-.'); a moment and a
-- length make another moment ('.+^'); lengths compare, and can be told
-- as numbers. Two moments are never added, and a moment is never taken
-- for how long something took, which as bare numbers they could be.
--
-- A moment is told as a number only at an edge: read off a clock, written
-- to a log.

-- | A moment, as a clock that never goes back tells it: so many seconds
-- since the clock began. See Note [Moments and lengths of time].
--
-- For moments @a@, @b@, @c@ and a length @d@, to within rounding:
--
-- > a .-. a == seconds 0
-- > (a .+^ d) .-. a == d
-- > a .+^ (b .-. a) == b
-- > inSeconds (c .-. a) == inSeconds (c .-. b) + inSeconds (b .-. a)
--
-- and exactly, @a <= b@ just when @b .-. a >= seconds 0@.
newtype Instant = Instant Double
    deriving (Eq, Ord)

instance Show Instant where
    showsPrec d (Instant t) = showsPrec d t

-- | The moment a clock tells as the given seconds.
clockAt :: Double -> Instant
clockAt = Instant

-- | A moment as the seconds its clock tells, for saying it.
inClockSeconds :: Instant -> Double
inClockSeconds (Instant t) = t

-- | How long after the second moment the first is.
(.-.) :: Instant -> Instant -> Seconds
Instant later .-. Instant earlier = Quantity (later - earlier)

infixl 6 .-.

-- | The moment so long after another.
(.+^) :: Instant -> Seconds -> Instant
Instant t .+^ Quantity d = Instant (t + d)

infixl 6 .+^

-- | The facts. Spreads are standard deviations: most of what happens is
-- within one, and little beyond two.
data World = World
    { frameRate :: FramesPerSecond
    -- ^ frames the camera sends
    , cameraNearest :: Millimetres
    , cameraFarthest :: Millimetres
    -- ^ the nearest and farthest the clip can be from the camera and be
    -- tracked: nearer, its reflectors run together or leave the view;
    -- farther, they are too small and dim
    , centroidNoise :: Pixels
    -- ^ how far a clearly seen reflector's centre is from where the head's
    -- pose puts it, at the most: a blob's centroid is good to a fraction of
    -- a pixel, and the clip's shape and tabs to the rest
    , headStepTurn :: Degrees
    , headStepMove :: Millimetres
    -- ^ the most the pose games see turns, and moves, from one frame to
    -- the next before it is taken for a jump: not how fast a head goes
    -- (that is 'headTurnSpeed' and 'headMoveSpeed', and this is 1800
    -- degrees and nearly 5 m a second at 'frameRate') but an allowance
    -- well beyond it
    , headTurnSpeed :: DegreesPerSecond
    , headMoveSpeed :: MillimetresPerSecond
    -- ^ the spreads of how fast a head turns, and its pivot moves: a quick
    -- glance or lean is at about one, and the fastest a head goes, some 600
    -- degrees and 1 m a second, at two
    , headTurnSpread :: Degrees
    -- ^ the spread of how far a head is turned from where it was centred
    , headReachAside :: Millimetres
    -- ^ the farthest a seated head moves to the side, or up or down, of
    -- where it was centred, as the camera sees it; toward or away from the
    -- camera it goes anywhere in the camera's range
    , bodySway :: Millimetres
    -- ^ the spread of how far, over half a second, a turning head is from
    -- turning about a point fixed in it, as the body sways
    , neckWander :: Millimetres
    -- ^ how far a neck is from having a single pivot over half a second,
    -- at the least, the body quite still
    , pivotSpread :: Millimetres
    -- ^ the spread, in each direction, of where the neck pivot is from the
    -- clip, from head to head
    , neckBehindClip :: Millimetres
    , neckWithin :: Millimetres
    -- ^ a neck pivot is at least so far behind the reflectors' plane, and
    -- no further than so from the clip
    , headStillFor :: Seconds
    -- ^ longer than a head is held still
    , sameSizeFactor :: Double
    -- ^ blobs of one reflector differ in size by up to this factor, as the
    -- head leans and turns over a few seconds: within it, two blobs may be
    -- the same thing; and the spread, as a factor, of one of a clip's
    -- reflectors' size beside another's
    , reflectorPixels :: Double
    -- ^ the pixels a reflector lights at 1 m, about, before any has been
    -- seen: on the user's camera the lower ones light some 245 and the top
    -- one 180
    , reflectorPixelsFactor :: Double
    -- ^ the spread of that, as a factor, from clip to clip and from one
    -- setting of the camera to another
    , sizeNoise :: Double
    -- ^ the spread of a blob's pixels from one frame to the next, as a
    -- fraction of them, the reflector and its distance the same
    , sizeMemory :: Seconds
    -- ^ the time over which what was learnt of a reflector's size fades,
    -- the reflector unseen
    , partShowing :: Double
    -- ^ the chance that a reflector in sight shows only part of itself:
    -- dimming at a slant, or half behind something
    , inPieces :: Double
    -- ^ the chance that a reflector in sight shows as two blobs
    , strayBlobs :: Double
    -- ^ how many blobs that aren't the clip's reflectors a frame shows, on
    -- average, anywhere in the picture, besides still lights masked
    , reflectorShowsFor :: Seconds
    , reflectorHidesFor :: Seconds
    -- ^ how long at a time, on average, one of the clip's reflectors shows,
    -- the clip in view, and how long it then stays out of sight: fading at
    -- a slant, or behind a hand or another reflector
    , clipInViewFor :: Seconds
    , clipAwayFor :: Seconds
    -- ^ how long at a time, on average, the clip is in the camera's view,
    -- and how long it then stays away
    }
    deriving (Show)

-- | The world as trackkat takes it to be: a TrackIR 5 on a monitor, and
-- someone seated in front of it.
world :: World
world =
    World
        { frameRate = perSecond 120
        , cameraNearest = millimetres 250
        , cameraFarthest = millimetres 2500
        , centroidNoise = pixels 1
        , headStepTurn = degrees 15
        , headStepMove = millimetres 40
        , headTurnSpeed = perSecond 300
        , headMoveSpeed = perSecond 500
        , headTurnSpread = degrees 40
        , headReachAside = millimetres 400
        , bodySway = millimetres 5
        , neckWander = millimetres 2
        , pivotSpread = millimetres 40
        , neckBehindClip = millimetres 30
        , neckWithin = millimetres 300
        , headStillFor = seconds 300
        , sameSizeFactor = 1.5
        , reflectorPixels = 200
        , reflectorPixelsFactor = 6
        , sizeNoise = 0.1
        , sizeMemory = seconds 10
        , partShowing = 0.01
        , inPieces = 0.05
        , strayBlobs = 1
        , reflectorShowsFor = seconds 10
        , reflectorHidesFor = seconds 0.5
        , clipInViewFor = seconds 60
        , clipAwayFor = seconds 10
        }

-- | The time from one frame to the next, at the world's 'frameRate'.
frameTime :: World -> Seconds
frameTime w = Quantity (1 / inPerSecond (frameRate w))
