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

    -- * The world
    World (..),
    world,
) where

-- Note [One world]
-- ~~~~~~~~~~~~~~~~
-- Tracking is a pile of judgements: is this pose somewhere a head can be,
-- did the head just move faster than heads move, is this blob a reflector
-- or a lamp. Each needs a number, and the numbers grew module by module,
-- so that the same fact (how far a head turns, how much a blob's centre is
-- out) was stated several times, in different currencies and sometimes
-- with different values. docs/heuristics.md lists them all.
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
-- compute in bare numbers as before, and unwrap a fact where they name
-- it. Nothing is computed here that could round differently from what the
-- modules computed before.
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

-- | Units: degrees of turning, mm, pixels of the camera's image, seconds,
-- and one per another.
data Deg

data MM

data Px

data Sec

data Per u t

type Degrees = Quantity Deg

type Millimetres = Quantity MM

type Pixels = Quantity Px

type Seconds = Quantity Sec

type DegreesPerSecond = Quantity (Per Deg Sec)

type MillimetresPerSecond = Quantity (Per MM Sec)

-- | Frames are counted, not measured: a bare number of them a second.
type FramesPerSecond = Quantity (Per () Sec)

degrees :: Double -> Degrees
degrees = Quantity

millimetres :: Double -> Millimetres
millimetres = Quantity

pixels :: Double -> Pixels
pixels = Quantity

seconds :: Double -> Seconds
seconds = Quantity

-- | So much of something a second.
perSecond :: Double -> Quantity (Per u Sec)
perSecond = Quantity

inDegrees :: Degrees -> Double
inDegrees (Quantity x) = x

inMillimetres :: Millimetres -> Double
inMillimetres (Quantity x) = x

inPixels :: Pixels -> Double
inPixels (Quantity x) = x

inSeconds :: Seconds -> Double
inSeconds (Quantity x) = x

inPerSecond :: Quantity (Per u Sec) -> Double
inPerSecond (Quantity x) = x

-- | A multiple of a quantity.
times :: Double -> Quantity u -> Quantity u
times k (Quantity x) = Quantity (k * x)

-- | How much a rate comes to over a time.
during :: Quantity (Per u Sec) -> Seconds -> Quantity u
during (Quantity rate) (Quantity t) = Quantity (rate * t)

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
    -- ^ the most a pose followed from one frame to the next turns, and its
    -- pivot moves: not how fast a head goes (that is 'headTurnSpeed' and
    -- 'headMoveSpeed', and this is 1800 degrees and nearly 5 m a second at
    -- 'frameRate') but an allowance, set below what blobs matched to the
    -- wrong reflectors make a head seem to do
    , headTurnRate :: DegreesPerSecond
    , headMoveRate :: MillimetresPerSecond
    -- ^ how much more that allowance is for each second between two
    -- sightings of the clip
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
    -- the same thing
    }
    deriving (Show)

-- | The world as trackkat takes it to be: a TrackIR 5 on a monitor, and
-- someone seated in front of it. Every value but a head's speed
-- ('headTurnSpeed', 'headMoveSpeed') is the one tracking used before the
-- facts were gathered here.
world :: World
world =
    World
        { frameRate = perSecond 120
        , cameraNearest = millimetres 250
        , cameraFarthest = millimetres 2500
        , centroidNoise = pixels 1
        , headStepTurn = degrees 15
        , headStepMove = millimetres 40
        , headTurnRate = perSecond 100
        , headMoveRate = perSecond 200
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
        }
