-- | trackkat's command line.
module Test.CommandLine (tests) where

import Data.Char (isDigit)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import TrackKat.CommandLine

tests :: TestTree
tests =
    testGroup
        "CommandLine"
        [ testProperty "the arguments that start trackkat again as it was started, after a process, read back as that" $
            forAllShrink genRun shrinkRun $ \(start, window, after) ->
                parseCommand (startArgs start window ++ afterArgs after) === Just (Run start window after)
        , testProperty "started by hand the window opens, and otherwise only if asked" $
            forAll (elements [ByHand, ByGame, AtLogin]) $ \start ->
                parseCommand (startArgs start WindowHidden)
                    === Just (Run start (if start == ByHand then WindowShown else WindowHidden) Nothing)
        , testProperty "a process is named in digits alone" $
            forAllShrink (oneof [listOf (elements "0123456789"), listOf (elements "0123456789 -+x.")]) shrink $ \pid ->
                let digits = not (null pid) && all isDigit pid
                 in case parseCommand ["--after", pid] of
                        Just (Run ByHand WindowShown (Just n)) -> property digits .&&. afterArgs (Just n) === ["--after", dropZeros pid]
                        Nothing -> property (not digits || length (dropZeros pid) > 18)
                        other -> counterexample (show other) False
        , testProperty "anything else on the command line is refused, wherever it comes" $
            forAllShrink genRun shrinkRun $ \(start, window, after) ->
                forAll (elements ["--help", "-window", "--after", "trackkat.conf", ""]) $ \word ->
                    let args = startArgs start window ++ afterArgs after
                     in forAll (chooseInt (0, length args)) $ \i ->
                            -- A word after --after is taken for its process.
                            (i == 0 || args !! (i - 1) /= "--after") ==>
                                parseCommand (take i args ++ [word] ++ drop i args) === Nothing
        ]
  where
    dropZeros pid = case dropWhile (== '0') pid of
        "" -> "0"
        rest -> rest

-- | How trackkat is started to track: the ways the command line can ask,
-- so by hand always with its window.
genRun :: Gen (Start, WindowAtStart, Maybe Int)
genRun = do
    start <- elements [ByHand, ByGame, AtLogin]
    window <- if start == ByHand then pure WindowShown else elements [WindowShown, WindowHidden]
    after <- oneof [pure Nothing, Just <$> chooseInt (0, maxBound)]
    pure (start, window, after)

-- | Started by hand, after no process, or one with a smaller number.
shrinkRun :: (Start, WindowAtStart, Maybe Int) -> [(Start, WindowAtStart, Maybe Int)]
shrinkRun (start, window, after) =
    [(ByHand, WindowShown, after) | start /= ByHand]
        ++ [(start, window, Nothing) | after /= Nothing]
        ++ [(start, window, Just n) | Just pid <- [after], n <- shrink pid, n >= 0]
