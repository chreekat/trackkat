-- | The game served through NaturalPoint's NPClient, remembered across
-- restarts.
module Test.ServedGame (tests) where

import Control.Monad (forM_)
import Data.Either (isLeft)
import Data.List (isInfixOf)
import Data.Maybe (isNothing)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Test.Check
import Test.Gen (earlier, forOneOf, shrinkFrom)
import Test.Instances ()
import TrackKat.ServedGame

tests :: TestTree
tests = group "ServedGame" $ do
    -- The game served through NaturalPoint's NPClient, remembered across
    -- restarts.
    let dcs = ServedGame 4242 (Just 133412345678901234) 262182 (Just 1006) TrackingStarted
        unknown = dcs{servedStarted = Nothing, servedProfile = Nothing, servedTracking = TrackingStopped}
    forM_ [dcs, unknown] $ \g -> checkShowing "served game round trip" (show g) $ parseServedGame (renderServedGame g) == Right g
    check "served game file read" $
        parseServedGame "process = 9\n# a comment\n\nstarted=unknown\nwindow = 7\nprofile = none # none\ntracking = no\n"
            == Right (ServedGame 9 Nothing 7 Nothing TrackingStopped)
    let without name = unlines [l | l <- lines (renderServedGame dcs), takeWhile (/= ' ') l /= name]
    forM_ ["process", "started", "window", "profile", "tracking"] $ \name ->
        checkShowing "served game without" (name) $ isLeft (parseServedGame (without name))
    check "served game files that don't parse" $
        all
            (isLeft . parseServedGame . (renderServedGame dcs ++))
            ["process = 1\n", "junk\n"]
            && all
                (\(name, value) -> isLeft (parseServedGame (without name ++ name ++ " = " ++ value ++ "\n")))
                [("process", "0"), ("process", "x"), ("started", "-"), ("profile", "65536"), ("profile", "-1"), ("tracking", "maybe")]
    check "served game whose process has exited" $ isNothing (stillServed dcs ProcessGone (Just 4242))
    check "served game still running" $ stillServed dcs (ProcessRunning (Just 133412345678901234)) Nothing == Just dcs
    check "served game's process ID reused" $ isNothing (stillServed dcs (ProcessRunning (Just 133412345678909999)) (Just 4242))
    check "served game running, Windows not saying when it started, its window still its own" $
        stillServed dcs (ProcessRunning Nothing) (Just 4242) == Just dcs
            && stillServed unknown (ProcessRunning (Just 1)) (Just 4242) == Just unknown
    check "served game's process ID reused, Windows not saying when either started" $
        all
            (isNothing . stillServed dcs (ProcessRunning Nothing))
            [Nothing, Just 77]
            && isNothing (stillServed unknown (ProcessRunning (Just 1)) Nothing)
    tree properties

properties :: TestTree
properties =
    testGroup
        "properties"
        [ testProperty "a served game is read back as written" $
            \g ->
                parseServedGame (renderServedGame g) === Right g
        , testProperty "a game is still served just when it runs as the same process" $
            \g -> forOneOf [Nothing, servedStarted g, Just 1] $ \started -> forOneOf [Nothing, Just (servedProcess g), Just (servedProcess g + 1)] $ \owner ->
                let same = case (servedStarted g, started) of
                        (Just was, Just now) -> was == now
                        -- Windows not saying when either started, its window
                        -- being its own tells.
                        _ -> owner == Just (servedProcess g)
                 in stillServed g (ProcessRunning started) owner === (if same then Just g else Nothing)
                        .&&. stillServed g ProcessGone owner === Nothing
        , testProperty "the file is touched just when the game served has changed: written to read back as the game, or deleted when there is none" $
            \g (Count seen) (Count changes) ->
                conjoin
                    [ case note seen changes game of
                        Nothing -> changes === seen
                        Just Delete -> property (changes /= seen) .&&. game === Nothing
                        Just (Write text) -> property (changes /= seen) .&&. fmap Right game === Just (parseServedGame text)
                    | game <- [Nothing, Just g]
                    ]
        , testProperty "a game served again is told by its name if it has one, its profile if not, and else its process" $
            \g (Known name) ->
                let line = describeServedAgain g name
                    told = case (servedProfile g, name) of
                        (Just i, Just n) -> n ++ " (game " ++ show i ++ ")"
                        (Just i, Nothing) -> "game " ++ show i
                        (Nothing, _) -> "process " ++ show (servedProcess g)
                 in counterexample line (told `isInfixOf` line)
        ]

-- | How many times something has happened, up to five.
newtype Count = Count Int
    deriving (Show)

instance Arbitrary Count where
    arbitrary = Count <$> chooseInt (0, 5)
    shrink (Count n) = Count <$> shrinkFrom 0 n

-- | The name a game goes by, if trackkat knows one.
newtype Known = Known (Maybe String)
    deriving (Show)

instance Arbitrary Known where
    arbitrary = Known <$> elements [Nothing, Just "DCS World"]
    shrink (Known name) = Known <$> earlier [Nothing, Just "DCS World"] name
