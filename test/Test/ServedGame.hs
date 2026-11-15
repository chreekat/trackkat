-- | The game served through NaturalPoint's NPClient, remembered across
-- restarts.
module Test.ServedGame (tests) where

import Control.Monad (forM_)
import Data.Either (isLeft)
import Data.Maybe (isNothing)
import Test.Tasty (TestTree)

import Test.Check
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
