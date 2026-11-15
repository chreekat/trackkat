-- | When trackkat looks for a newer version, and what it says of what it
-- finds. See Note [What looking for updates promises] in
-- "TrackKat.Updater".
module Test.Updater (tests) where

import Data.List (isInfixOf, mapAccumL)
import Data.Maybe (isJust)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Test.Gen (earlier, shrinkWithin)
import Test.Instances ()
import TrackKat.Config (Updates (..))
import TrackKat.Display
import TrackKat.Log
import TrackKat.Release
import TrackKat.Updater

tests :: TestTree
tests =
    testGroup
        "Updater"
        [ testProperty "unasked, trackkat looks just when updates are on and four hours have passed since any look that reached GitHub or was asked for" $
            \(Rounds rounds) ->
                conjoin
                    [ counterexample (show look) $
                        property (lookUpdates look == UpdatesOn && all ((>= updateEvery) . (lookAt look -)) (counted before))
                    | (before, look) <- withEarlier (looks rounds)
                    , lookWhy look == Due
                    ]
                    .&&. conjoin
                        [ counterexample (show (at, updates)) $
                            isJust (dueLook updates at u) === (updates == UpdatesOn && all ((>= updateEvery) . (at -)) (counted (looksBefore at rounds)))
                        | (at, updates, u) <- roundStarts rounds
                        ]
        , testProperty "an unasked look that finds no network tells the user nothing, and is tried again whenever updates are next on" $
            \(Rounds rounds) (At at) (Later later) ->
                let (u, effects) = answered self Due at (Left (Offline "no route")) (waitingAfter rounds) (reached rounds)
                 in userLines effects === []
                        .&&. [s | ShowStatus s <- effects] === []
                        .&&. length [() | Log (ForDeveloper _) <- effects] === 1
                        .&&. dueLook UpdatesOn (at + later) u === Just Due
        , testProperty "a look the user asked for always answers, in the log and in the window's strip" $
            \(Rounds rounds) (Found found) (Waiting waiting) ->
                let (_, effects) = answered self Asked 0 found waiting (reached rounds)
                 in counterexample (show effects) $
                        length (userLines effects) === 1
                            .&&. length [() | ShowStatus _ <- effects] === 1
                            .&&. userLines (begun Asked) === ["Looking for a newer trackkat."]
                            .&&. begun Due === []
        , testProperty "a failure that lasts is logged for the user by the first unasked look to meet it, and after that only for the developer, until a look works" $
            \(Rounds rounds) ->
                conjoin
                    [ counterexample (show look) $ case (lookWhy look, lookFound look) of
                        (Due, Left (Offline _)) -> property True
                        (Due, Left _) ->
                            let since = takeWhile (not . worked) (reverse before)
                                met = any (failed . lookFound) since
                             in (length (userLines (lookEffects look)), length [() | Log (ForDeveloper _) <- lookEffects look])
                                    === (if met then (0, 1) else (1, 0))
                        _ -> property True
                    | (before, look) <- withEarlier (looks rounds)
                    ]
        , testProperty "the strip has its tick just when no newer version was found and none waits for a restart" $
            \(Rounds rounds) (Found found) (Waiting waiting) why ->
                let (_, effects) = answered self why 0 found waiting (reached rounds)
                    upToDate = found == Right Nothing && not (isUpdate waiting)
                 in counterexample (show effects) $
                        conjoin [property (versionFolder self `isInfixOf` s) | ShowStatus (UpToDate s) <- effects]
                            .&&. (not (null [() | ShowStatus (UpToDate _) <- effects])) === upToDate
        , testProperty "the strip offers what there is to offer, naming the version, and the tray only a restart to update" $
            \(Waiting waiting) ->
                case (waiting, offerNotice "C:\\trackkat" waiting) of
                    (NoOffer, Nothing) -> property True
                    (OfferInstall v, Just n) -> property (versionFolder v `isInfixOf` noticeText n) .&&. noticeTray n === Nothing
                    (OfferInstalled v, Just n) -> property (versionFolder v `isInfixOf` noticeText n) .&&. noticeTray n === Nothing
                    (OfferUpdate v, Just n) -> property (versionFolder v `isInfixOf` noticeText n) .&&. fmap (versionFolder v `isInfixOf`) (noticeTray n) === Just True
                    other -> counterexample (show other) False
        ]
  where
    isUpdate (OfferUpdate _) = True
    isUpdate _ = False
    failed (Left (Offline _)) = False
    failed (Left _) = True
    failed (Right _) = False
    worked = either (const False) (const True) . lookFound

userLines :: [Effect] -> [String]
userLines effects = [line | Log (ForUser line) <- effects]

-- | This copy's version.
self :: Version
self = version "0.1.5"

version :: String -> Version
version text = maybe (error ("not a version: " ++ text)) id (parseVersion text)

-- | A round of the thread that keeps trackkat up to date: how long after
-- the round before it begins, in seconds, whether updates are on, what a
-- look would find and the window then offer, and whether the user asks for
-- a look during it, with how long after its beginning that look answers
-- and what it finds.
data Round = Round
    { roundAfter :: Double
    , roundUpdates :: Updates
    , roundFound :: Either LookFailure (Maybe Version)
    , roundWaiting :: Offer
    , roundAsked :: Maybe (Double, Either LookFailure (Maybe Version))
    }
    deriving (Show)

-- | A look made: when the wait for the next runs from, why it was made,
-- whether updates were on, what it found and what it asked for.
data Look = Look
    { lookAt :: Double
    , lookWhy :: Looking
    , lookUpdates :: Updates
    , lookFound :: Either LookFailure (Maybe Version)
    , lookEffects :: [Effect]
    }
    deriving (Show)

-- | Rounds as trackkat runs them: a look when one is due as the round
-- begins, then one if the user asks. What is remembered after them, and
-- for each round when it began, whether updates were on, what was
-- remembered as it began, and the looks made in it.
run :: [Round] -> (Updater, [(Double, Updates, Updater, [Look])])
run rounds = (remembered, rows)
  where
    ((remembered, _), rows) = mapAccumL go (newUpdater, 0) rounds
    go (u, clock) r =
        let at = clock + roundAfter r
            (u1, due) = case dueLook (roundUpdates r) at u of
                Just why ->
                    let (u', effects) = answered self why at (roundFound r) (roundWaiting r) u
                     in (u', [Look at why (roundUpdates r) (roundFound r) effects])
                Nothing -> (u, [])
            (u2, asked, end) = case roundAsked r of
                Just (after, found) ->
                    let (u', effects) = answered self Asked (at + after) found (roundWaiting r) u1
                     in (u', [Look (at + after) Asked (roundUpdates r) found effects], at + after)
                Nothing -> (u1, [], at)
         in ((u2, end), (at, roundUpdates r, u, due ++ asked))

-- | What is remembered after rounds.
reached :: [Round] -> Updater
reached = fst . run

-- | What the window offers after rounds.
waitingAfter :: [Round] -> Offer
waitingAfter rounds = case reverse rounds of
    r : _ -> roundWaiting r
    [] -> NoOffer

-- | The looks rounds make, oldest first.
looks :: [Round] -> [Look]
looks rounds = concat [made | (_, _, _, made) <- snd (run rounds)]

-- | When each round begins, whether updates are on then, and what is
-- remembered as it begins.
roundStarts :: [Round] -> [(Double, Updates, Updater)]
roundStarts rounds = [(at, updates, u) | (at, updates, u, _) <- snd (run rounds)]

-- | The looks made before a round beginning at the time given.
looksBefore :: Double -> [Round] -> [Look]
looksBefore at rounds = concat [made | (began, _, _, made) <- snd (run rounds), began < at]

-- | Each with those before it, oldest first.
withEarlier :: [a] -> [([a], a)]
withEarlier xs = [(take i xs, x) | (i, x) <- zip [0 ..] xs]

-- | When the looks that put off the next were: all but unasked ones that
-- found no network.
counted :: [Look] -> [Double]
counted made = [lookAt l | l <- made, not (lookWhy l == Due && offline (lookFound l))]
  where
    offline (Left (Offline _)) = True
    offline _ = False

-- | Rounds ten minutes to some hours apart, with updates mostly on, looks
-- finding anything, and the user asking now and then; simpler of fewer.
newtype Rounds = Rounds [Round]
    deriving (Show)

instance Arbitrary Rounds where
    arbitrary = Rounds <$> scale (`div` 2) (listOf arbitrary)
    shrink (Rounds rounds) = Rounds <$> shrink rounds

-- | Simpler with updates on, nothing asked, and nothing newer found.
instance Arbitrary Round where
    arbitrary =
        Round
            <$> oneof [pure 600, choose (600, 3 * updateEvery), elements [updateEvery - 0.001, updateEvery]]
            <*> frequency [(4, pure UpdatesOn), (1, pure UpdatesOff)]
            <*> elements finds
            <*> elements offers
            <*> frequency [(3, pure Nothing), (1, Just <$> ((,) <$> choose (0, 600) <*> elements finds))]
    shrink r =
        [r{roundAsked = Nothing} | isJust (roundAsked r)]
            ++ [r{roundUpdates = UpdatesOn} | roundUpdates r == UpdatesOff]
            ++ [r{roundFound = Right Nothing} | roundFound r /= Right Nothing]
            ++ [r{roundWaiting = NoOffer} | roundWaiting r /= NoOffer]
            ++ [r{roundAfter = 600} | roundAfter r /= 600]

-- | What a look can find: no network, nothing published just now, a
-- failure, a newer version, or none. Simplest none.
newtype Found = Found (Either LookFailure (Maybe Version))
    deriving (Show)

instance Arbitrary Found where
    arbitrary = Found <$> elements finds
    shrink (Found found) = [Found (Right Nothing) | found /= Right Nothing]

finds :: [Either LookFailure (Maybe Version)]
finds =
    [ Left (Offline "no route")
    , Left (Unavailable "HTTP status 404")
    , Left (LookFailed "the zip doesn't check")
    , Right (Just (version "0.1.9+abc1234"))
    , Right Nothing
    ]

-- | What the window can offer; simplest nothing.
newtype Waiting = Waiting Offer
    deriving (Show)

instance Arbitrary Waiting where
    arbitrary = Waiting <$> elements offers
    shrink (Waiting offered) = Waiting <$> earlier offers offered

offers :: [Offer]
offers = [NoOffer, OfferInstall self, OfferInstalled (version "0.1.7"), OfferUpdate (version "0.1.9")]

-- | When a look answers, in seconds, up to ten times as long as trackkat
-- waits between looks.
newtype At = At Double
    deriving (Show)

instance Arbitrary At where
    arbitrary = At <$> choose (0, 10 * updateEvery)
    shrink (At at) = At <$> shrinkWithin (0, 10 * updateEvery) 0 at

-- | How many seconds later, up to a thousand.
newtype Later = Later Double
    deriving (Show)

instance Arbitrary Later where
    arbitrary = Later <$> choose (0, 1000)
    shrink (Later later) = Later <$> shrinkWithin (0, 1000) 0 later
