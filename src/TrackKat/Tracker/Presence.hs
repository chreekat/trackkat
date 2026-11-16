-- | Something that comes and goes: a reflector that shows or is out of
-- sight, the clip that is in the camera's view or away.
--
-- See Note [Coming and going].
module TrackKat.Tracker.Presence (
    Presence (..),
    Comings (..),
    passing,
    usually,
    reflectorComings,
    clipComings,
) where

import TrackKat.World (Seconds, World (..), inSeconds, world)

-- Note [Coming and going]
-- ~~~~~~~~~~~~~~~~~~~~~~~
-- A reflector goes out of sight now and then, and then stays out for a
-- while: it doesn't flicker in and out afresh each frame. So what it costs
-- a hypothesis that a reflector isn't seen can't be a price per frame, nor
-- a time limit: it is a price for going, paid once, and a rate for staying
-- gone.
--
-- That is what two states with a rate of leaving each give: something
-- 'Present' goes at one over the time it usually stays, and something
-- 'Gone' comes back at one over the time it usually stays away. The chance
-- of being in a state some time after being in another ('passing') follows
-- from the two times alone, whatever the time between: over many short
-- steps it comes to the same as over one long one, so nothing here
-- depends on how many frames a second there are. Going, between two
-- frames, is unlikely by how short a frame is beside the time it usually
-- stays; and each frame gone thereafter is likely, less likely only by
-- the frame's share of the time it usually stays away.

-- | Whether something that comes and goes is there.
data Presence = Present | Gone
    deriving (Eq, Show)

-- | How something comes and goes: for how long at a time, on average, it
-- stays, and stays away.
data Comings = Comings
    { staysFor :: Seconds
    , awayFor :: Seconds
    }
    deriving (Eq, Show)

-- | The log of the chance that something is in the second state a time
-- after it was in the first (see Note [Coming and going]).
passing :: Comings -> Seconds -> Presence -> Presence -> Double
passing comings dt from to = log $ case (from, to) of
    (Present, Present) -> there + away * kept
    (Present, Gone) -> away * (1 - kept)
    (Gone, Present) -> there * (1 - kept)
    (Gone, Gone) -> away + there * kept
  where
    leaving = 1 / inSeconds (staysFor comings)
    returning = 1 / inSeconds (awayFor comings)
    there = returning / (leaving + returning)
    away = leaving / (leaving + returning)
    -- How much of where it was is still told so long after.
    kept = exp (negate ((leaving + returning) * max 0 (inSeconds dt)))

-- | The log of the chance that something is in a state, with nothing known
-- of how it was: the share of the time it is.
usually :: Comings -> Presence -> Double
usually comings to = log $ case to of
    Present -> stay / (stay + gone)
    Gone -> gone / (stay + gone)
  where
    stay = inSeconds (staysFor comings)
    gone = inSeconds (awayFor comings)

-- | How one of the clip's reflectors comes and goes, the clip in view: the
-- world's 'reflectorShowsFor' and 'reflectorHidesFor'.
reflectorComings :: Comings
reflectorComings = Comings (reflectorShowsFor world) (reflectorHidesFor world)

-- | How the clip comes into the camera's view and goes: the world's
-- 'clipInViewFor' and 'clipAwayFor'.
clipComings :: Comings
clipComings = Comings (clipInViewFor world) (clipAwayFor world)
