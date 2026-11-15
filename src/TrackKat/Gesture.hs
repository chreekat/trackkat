-- | Telling apart the ways a key or button is pressed, so that one can do
-- several things: pressed and let go, held down, or pressed twice.
--
-- See Note [What a press comes to].
module TrackKat.Gesture (
    Gesture (..),
    Edge (..),
    Signal (..),
    holdAfter,
    doubleWithin,
    Recognizer,
    newRecognizer,
    edge,
    tick,
    forget,
) where

import Data.List (partition)

import TrackKat.World (Instant, Seconds, seconds, (.-.))

-- Note [What a press comes to]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Each press of a key or button comes to one thing, by when it went down
-- and came up and by which gestures the key has anything bound to:
--
--  * With a hold bound, a press still down 'holdAfter' it went down is a
--    hold, which begins then and ends when it comes up.
--
--  * With a double bound, a press that goes down within 'doubleWithin' of
--    the one before going down, that one being let go and not a hold,
--    makes the two a double, made the moment it goes down.
--
--  * Any other press is a tap, made as soon as it can be nothing else: the
--    moment it goes down, with neither a hold nor a double bound; else
--    once it has come up, if a hold is bound, and its time for a second
--    press is up, if a double is.
--
-- So a key with only taps bound acts as soon as it is touched, and a tap
-- waits only for what the key's other bindings could still make of it.
--
-- What a press comes to depends only on the times of the presses and
-- releases. The clock's ticks only bring it sooner: a hold begins, and a
-- tap that waited is made, at the first tick or edge of its key from the
-- time it is settled.
--
-- A hold remembers what it began, so it ends that, whatever is bound by
-- then.

-- | A way of pressing a key or button.
data Gesture
    = -- | pressed and let go
      Tap
    | -- | held down for 'holdAfter'
      Hold
    | -- | pressed twice within 'doubleWithin'
      Double
    deriving (Eq, Show, Enum, Bounded)

-- | A key or button going down, or coming up.
data Edge = WentDown | CameUp
    deriving (Eq, Show)

-- | What presses come to, as what is bound to the gesture they make.
data Signal a
    = -- | a tap or a double
      Made a
    | HoldBegan a
    | HoldEnded a
    deriving (Eq, Show)

-- | How long a press is down before it is a hold.
holdAfter :: Seconds
holdAfter = seconds 0.3

-- | How long after a press goes down a second one makes a double of them.
doubleWithin :: Seconds
doubleWithin = seconds 0.3

-- | The presses not yet settled, of keys of some kind bound to things of
-- some kind: at most one for each key.
newtype Recognizer k a = Recognizer [Pressing k a]
    deriving (Eq, Show)

data Pressing k a = Pressing !k !(Stage a)
    deriving (Eq, Show)

-- | How far a key's press has come.
data Stage a
    = -- | down since then, and not yet a hold or a tap
      Down !Instant
    | -- | a hold, still down, and what it began
      Held [a]
    | -- | went down then and came up, and a second press may yet follow
      Lifted !Instant
    | -- | still down, with nothing more to come of it
      Spent
    deriving (Eq, Show)

-- | With no press unsettled.
newRecognizer :: Recognizer k a
newRecognizer = Recognizer []

-- | What a key's gestures are bound to.
type Bound a = [(Gesture, a)]

to :: Gesture -> Bound a -> [a]
to gesture bound = [a | (g, a) <- bound, g == gesture]

has :: Gesture -> Bound a -> Bool
has gesture = any ((== gesture) . fst)

-- | Take a key going down or coming up at a moment, given what each key's
-- gestures are bound to: what it makes. See Note [What a press comes to].
-- A key that goes down while down is taken to have come up unseen.
edge :: (Eq k) => (k -> Bound a) -> Instant -> Edge -> k -> Recognizer k a -> (Recognizer k a, [Signal a])
edge bindings now e key (Recognizer pressings) =
    (Recognizer (strictly (others ++ [Pressing key stage | Just stage <- [after]])), settled ++ made)
  where
    (own, others) = partition (\(Pressing k _) -> k == key) pressings
    bound = bindings key
    (before, settled) = case own of
        Pressing _ stage : _ -> settle bound now stage
        [] -> (Nothing, [])
    (after, made) = case e of
        WentDown -> press bound now before
        CameUp -> release bound now before

-- | Take the clock to a moment, given what each key's gestures are bound
-- to: the holds that begin and the taps that waited, of the presses whose
-- time has come.
tick :: (k -> Bound a) -> Instant -> Recognizer k a -> (Recognizer k a, [Signal a])
tick bindings now (Recognizer pressings) =
    (Recognizer (strictly [Pressing key stage | (key, (Just stage, _)) <- settled]), concatMap (snd . snd) settled)
  where
    settled = [(key, settle (bindings key) now stage) | Pressing key stage <- pressings]

-- | Start afresh with the keys chosen, as when what is bound to them
-- changes, or their edges are no longer told: their holds end, and
-- nothing else comes of their presses under way.
forget :: (k -> Bool) -> Recognizer k a -> (Recognizer k a, [Signal a])
forget chosen (Recognizer pressings) =
    (Recognizer (strictly kept), concat [map HoldEnded begun | Pressing _ (Held begun) <- forgotten])
  where
    (forgotten, kept) = partition (\(Pressing k _) -> chosen k) pressings

-- | A list evaluated to its end, so that a recognizer kept holds on to no
-- more than its presses.
strictly :: [a] -> [a]
strictly xs = foldr seq xs xs

-- | What the time passing to a moment makes of a press.
settle :: Bound a -> Instant -> Stage a -> (Maybe (Stage a), [Signal a])
settle bound now stage = case stage of
    Down since
        | has Hold bound -> if now .-. since >= holdAfter then (Just (Held (to Hold bound)), map HoldBegan (to Hold bound)) else waiting
        | has Double bound && now .-. since < doubleWithin -> waiting
        | otherwise -> (Just Spent, map Made (to Tap bound))
    Lifted since
        | has Double bound && now .-. since < doubleWithin -> waiting
        | otherwise -> (Nothing, map Made (to Tap bound))
    Held _ -> waiting
    Spent -> waiting
  where
    waiting = (Just stage, [])

-- | A key going down, its press before settled to that moment.
press :: Bound a -> Instant -> Maybe (Stage a) -> (Maybe (Stage a), [Signal a])
press bound now before = case before of
    Nothing
        | has Hold bound || has Double bound -> (Just (Down now), [])
        | otherwise -> (Nothing, map Made (to Tap bound))
    Just (Lifted _) -> (Just Spent, map Made (to Double bound))
    Just _ ->
        let (up, ended) = release bound now before
            (down, made) = press bound now up
         in (down, ended ++ made)

-- | A key coming up, its press before settled to that moment.
release :: Bound a -> Instant -> Maybe (Stage a) -> (Maybe (Stage a), [Signal a])
release bound now before = case before of
    Just (Down since)
        | has Double bound && now .-. since < doubleWithin -> (Just (Lifted since), [])
        | otherwise -> (Nothing, map Made (to Tap bound))
    Just (Held begun) -> (Nothing, map HoldEnded begun)
    Just Spent -> (Nothing, [])
    Just (Lifted _) -> (before, [])
    Nothing -> (Nothing, [])
