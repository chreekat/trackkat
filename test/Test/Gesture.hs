-- | Telling taps, holds and doubles apart: what presses come to, and when.
-- See Note [What a press comes to] in "TrackKat.Gesture".
module Test.Gesture (tests) where

import Data.List (mapAccumL)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Test.Gen
import TrackKat.Gesture
import TrackKat.World (Instant, Seconds, clockAt, seconds, times, (.+^), (.-.))

tests :: TestTree
tests =
    testGroup
        "Gesture"
        [ testProperty "each press comes to what the rules say of its key's presses alone, at the first tick or edge of the key from when that is settled" $
            \(Bound ways) (Story story) ->
                let happened = finished story
                 in conjoin
                        [ counterexample (show key) $
                            [(at, signal) | (at, signals) <- told (bindings ways) happened, signal <- signals, keyOf signal == key]
                                === [(at, signal) | (at, signal) <- ruled key (ways key) (momentsOf key happened) (pressesOf key happened), gestureOf signal `elem` ways key]
                        | key <- keys
                        ]
        , testProperty "a key with only taps bound makes one the moment it goes down, every time, and nothing else" $
            \(Story story) ->
                conjoin
                    [ signals === [Made (key, Tap) | Goes WentDown key <- [step]]
                    | ((_, step), (_, signals)) <- zip (timeline story) (told (bindings (const [Tap])) (timeline story))
                    ]
        , testProperty "nothing comes of a key with nothing bound, and nothing is remembered of it" $
            \(Story story) ->
                conjoin
                    [ counterexample (show after) (null signals && after == newRecognizer)
                    | (after, signals) <- run (const ([] :: [(Gesture, ())])) (timeline story)
                    ]
        , testProperty "once every key is up and no press is unsettled, nothing is remembered" $
            \(Bound ways) (Story story) ->
                fst (last (run (bindings ways) (finished story))) === newRecognizer
        , testProperty "a hold that begins ends once, as its key comes up or is forgotten, whatever is bound meanwhile" $
            \(Rebinding story) ->
                let signals = rebound story
                 in conjoin
                        [ counterexample (show key) $
                            [signal | signal <- signals, keyOf signal == key, gestureOf signal == Hold]
                                === take (length [() | HoldBegan (k, _) <- signals, k == key] * 2) (cycle [HoldBegan (key, Hold), HoldEnded (key, Hold)])
                        | key <- keys
                        ]
        ]

-- | One of three keys.
data Key = KeyA | KeyB | KeyC
    deriving (Eq, Show, Enum, Bounded)

keys :: [Key]
keys = [minBound .. maxBound]

-- | The gestures of each key that have something bound: here, bound to
-- the key and the gesture themselves, so that signals say what was made
-- of which key.
bindings :: (Key -> [Gesture]) -> Key -> [(Gesture, (Key, Gesture))]
bindings ways key = [(gesture, (key, gesture)) | gesture <- ways key]

keyOf :: Signal (Key, Gesture) -> Key
keyOf = fst . bound

gestureOf :: Signal (Key, Gesture) -> Gesture
gestureOf = snd . bound

bound :: Signal a -> a
bound (Made a) = a
bound (HoldBegan a) = a
bound (HoldEnded a) = a

-- | Something that happens to the recognizer.
data Step = Clock | Goes Edge Key
    deriving (Eq, Show)

-- | A recognizer taken through what happens, from none: after each
-- thing, the recognizer and what was made.
run :: (Eq a) => (Key -> [(Gesture, a)]) -> [(Instant, Step)] -> [(Recognizer Key a, [Signal a])]
run bindings' = snd . mapAccumL (\r happening -> let after = step r happening in (fst after, after)) newRecognizer
  where
    step r (now, Clock) = tick bindings' now r
    step r (now, Goes way key) = edge bindings' now way key r

-- | What was made at each thing that happened, told by its place among
-- them.
told :: (Eq a) => (Key -> [(Gesture, a)]) -> [(Instant, Step)] -> [(Int, [Signal a])]
told bindings' = zip [0 ..] . map snd . run bindings'

-- | The ticks and a key's own edges among what happened: their places and
-- times.
momentsOf :: Key -> [(Instant, Step)] -> [(Int, Instant)]
momentsOf key happened = [(at, now) | (at, (now, step)) <- zip [0 ..] happened, step == Clock || step `elem` [Goes WentDown key, Goes CameUp key]]

-- | A key's presses among what happened: the place and time of each going
-- down and coming up.
pressesOf :: Key -> [(Instant, Step)] -> [((Int, Instant), (Int, Instant))]
pressesOf key happened = pairs [(at, now) | (at, (now, Goes _ k)) <- zip [0 ..] happened, k == key]
  where
    pairs (down : up : rest) = (down, up) : pairs rest
    pairs _ = []

-- | What a key's presses come to, by Note [What a press comes to] in
-- "TrackKat.Gesture" read over all of them at once, with every gesture
-- bound as 'bindings' has it; and the place of the moment each thing is
-- made at, the first of the key's moments from when it is settled. Only
-- whether a hold and a double are bound tells on it.
ruled :: Key -> [Gesture] -> [(Int, Instant)] -> [((Int, Instant), (Int, Instant))] -> [(Int, Signal (Key, Gesture))]
ruled key ways moments = go
  where
    go [] = []
    go (((downAt, down), (upAt, up)) : rest)
        | Hold `elem` ways
        , up .-. down >= holdAfter =
            (firstFrom downAt (\now -> now .-. down >= holdAfter), HoldBegan (key, Hold)) : (upAt, HoldEnded (key, Hold)) : go rest
        | Double `elem` ways
        , ((secondAt, second), _) : later <- rest
        , second .-. down < doubleWithin =
            (secondAt, Made (key, Double)) : go later
        | otherwise =
            ( firstFrom
                (if Hold `elem` ways then upAt else downAt)
                (\now -> Double `notElem` ways || now .-. down >= doubleWithin)
            , Made (key, Tap)
            )
                : go rest
    firstFrom place settled = case [at | (at, now) <- moments, at >= place, settled now] of
        at : _ -> at
        [] -> error "ruled: never settled"

-- | A length of time near those that tell the gestures apart: from much
-- less than 'holdAfter' and 'doubleWithin', through just under, just at
-- and just over them, to much more. Simplest the shortest.
newtype Gap = Gap Seconds
    deriving (Eq, Show)

gaps :: [Seconds]
gaps = [seconds 0.01, seconds 0.1, seconds 0.2] ++ [times k near | near <- [holdAfter, doubleWithin], k <- [0.99, 1, 1.01]] ++ [seconds 0.5, seconds 2]

instance Arbitrary Gap where
    arbitrary = Gap <$> frequency [(3, elements (take 3 gaps)), (2, elements (drop 3 gaps))]
    shrink (Gap gap) = Gap <$> earlier gaps gap

-- | Something that happens to keys: the clock ticks, or a key goes down
-- if it is up, and comes up if it is down.
data Happening = Ticks | Turns Key
    deriving (Eq, Show)

-- | Keys pressed and let go, and the clock ticking, one thing after
-- another with a gap before each. Simpler of fewer things, shorter gaps,
-- and ticks for keys.
newtype Story = Story [(Gap, Happening)]
    deriving (Show)

instance Arbitrary Story where
    arbitrary = Story <$> listOf ((,) <$> arbitrary <*> frequency [(1, pure Ticks), (2, Turns <$> elements keys)])
    shrink (Story story) = Story <$> shrinkList (\(gap, happening) -> [(gap, Ticks) | happening /= Ticks] ++ [(gap', happening) | gap' <- shrink gap]) story

-- | When each thing of a story happens, from the clock's start, and which
-- way each key goes.
timeline :: [(Gap, Happening)] -> [(Instant, Step)]
timeline = snd . mapAccumL happen (clockAt 0, [])
  where
    happen (before, down) (Gap gap, happening) =
        let now = before .+^ gap
         in case happening of
                Ticks -> ((now, down), (now, Clock))
                Turns key
                    | key `elem` down -> ((now, filter (/= key) down), (now, Goes CameUp key))
                    | otherwise -> ((now, key : down), (now, Goes WentDown key))

-- | A story's timeline carried on until it is settled: the keys still
-- down come up, and the clock ticks long after.
finished :: [(Gap, Happening)] -> [(Instant, Step)]
finished story = timeline (story ++ [(Gap (seconds 0.01), Turns key) | key <- keys, odd (length [() | (_, Turns k) <- story, k == key])] ++ [(Gap (seconds 10), Ticks)])

-- | Which of each key's gestures have something bound. Simplest none of
-- any, and simpler of fewer.
newtype Bound = Bound (Key -> [Gesture])

instance Show Bound where
    show (Bound ways) = show [(key, ways key) | key <- keys]

instance Arbitrary Bound where
    arbitrary = Bound . among <$> vectorOf (length keys) (sublistOf [Tap, Hold, Double])
    shrink (Bound ways) = Bound . among <$> shrinkTo (map (const []) keys) (traverse' (shrinkList (const []))) (map ways keys)
      where
        -- Each list with one of its members simpler.
        traverse' simpler xs = [take i xs ++ [x'] ++ drop (i + 1) xs | (i, x) <- zip [0 ..] xs, x' <- simpler x]

among :: [[Gesture]] -> Key -> [Gesture]
among each key = each !! fromEnum key

-- | A story among which the keys' bindings change. Simpler as its story
-- is, with fewer changes.
newtype Rebinding = Rebinding [Either (Key, [Gesture]) (Gap, Happening)]
    deriving (Show)

instance Arbitrary Rebinding where
    arbitrary = do
        Story story <- arbitrary
        changes <- listOf ((,) <$> elements keys <*> sublistOf [Tap, Hold, Double])
        Rebinding <$> shuffle (map Left changes ++ map Right story)
    shrink (Rebinding story) = Rebinding <$> shrinkList (either (const []) (\(gap, happening) -> [Right (gap, Ticks) | happening /= Ticks])) story

-- | What is made through a story among which bindings change, starting
-- with every gesture of every key bound, and ending with every key
-- forgotten. A key left with nothing bound is forgotten, and its edges
-- are not told, as trackkat doesn't watch it.
rebound :: [Either (Key, [Gesture]) (Gap, Happening)] -> [Signal (Key, Gesture)]
rebound story = concat made ++ snd (forget (const True) final)
  where
    ((final, _, _), made) = mapAccumL happen (newRecognizer, const [Tap, Hold, Double], (clockAt 0, [])) story
    happen (r, ways, clock) (Left (key, bound')) =
        let ways' k = if k == key then bound' else ways k
            (r', ended) = forget (null . ways') r
         in ((r', ways', clock), ended)
    happen (r, ways, (before, down)) (Right (Gap gap, happening)) =
        let now = before .+^ gap
         in case happening of
                Ticks -> let (r', signals) = tick (bindings ways) now r in ((r', ways, (now, down)), signals)
                Turns key ->
                    let way = if key `elem` down then CameUp else WentDown
                        down' = if key `elem` down then filter (/= key) down else key : down
                        (r', signals) = if null (ways key) then (r, []) else edge (bindings ways) now way key r
                     in ((r', ways, (now, down')), signals)
