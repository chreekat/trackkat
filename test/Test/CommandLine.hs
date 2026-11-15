-- | trackkat's command line.
module Test.CommandLine (tests) where

import Data.Char (isDigit)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Test.Gen (earlier, shrinkFrom, shrinkParts)
import Test.Instances ()
import TrackKat.CommandLine

tests :: TestTree
tests =
    testGroup
        "CommandLine"
        [ testProperty "the arguments that start trackkat again as it was started, after a process, read back as that" $
            \(Asked start window after) ->
                parseCommand (startArgs start window ++ afterArgs after) === Just (Run start window after)
        , testProperty "started by hand the window opens, and otherwise only if asked" $
            \start ->
                parseCommand (startArgs start WindowHidden)
                    === Just (Run start (if start == ByHand then WindowShown else WindowHidden) Nothing)
        , testProperty "a process is named in digits alone" $
            \(Named pid) ->
                let digits = not (null pid) && all isDigit pid
                 in case parseCommand ["--after", pid] of
                        Just (Run ByHand WindowShown (Just n)) -> property digits .&&. afterArgs (Just n) === ["--after", dropZeros pid]
                        Nothing -> property (not digits || length (dropZeros pid) > 18)
                        other -> counterexample (show other) False
        , testProperty "anything else on the command line is refused, wherever it comes" $
            \(Asked start window after) (Stray word) ->
                let args = startArgs start window ++ afterArgs after
                 in forAllShrink (chooseInt (0, length args)) shrink $ \i ->
                        -- A word after --after is taken for its process.
                        (i == 0 || args !! (i - 1) /= "--after") ==>
                            parseCommand (take i args ++ [word] ++ drop i args) === Nothing
        ]
  where
    dropZeros pid = case dropWhile (== '0') pid of
        "" -> "0"
        rest -> rest

-- | How trackkat is started to track: the ways the command line can ask,
-- so by hand always with its window. Simplest started by hand after no
-- process, and simpler with its window, or after a process with a smaller
-- number.
data Asked = Asked Start WindowAtStart (Maybe Int)
    deriving (Show)

instance Arbitrary Asked where
    arbitrary = do
        start <- arbitrary
        window <- if start == ByHand then pure WindowShown else elements [WindowShown, WindowHidden]
        Asked start window <$> oneof [pure Nothing, Just <$> chooseInt (0, maxBound)]
    shrink =
        shrinkParts
            [ \(Asked start _ after) -> [Asked ByHand WindowShown after | start /= ByHand]
            , \(Asked start window after) -> [Asked start WindowShown after | window /= WindowShown]
            , \(Asked start window after) -> [Asked start window Nothing | after /= Nothing] ++ [Asked start window (Just n) | Just pid <- [after], n <- shrinkFrom 0 pid]
            ]

-- | What might be given as a process's name: digits, or digits among a
-- little else. Simpler of fewer letters, and with a nought for one.
newtype Named = Named String
    deriving (Show)

instance Arbitrary Named where
    arbitrary = Named <$> oneof [listOf (elements "0123456789"), listOf (elements "0123456789 -+x.")]
    shrink (Named pid) = Named <$> shrinkList (\letter -> ['0' | letter /= '0']) pid

-- | A word the command line has no place for.
newtype Stray = Stray String
    deriving (Show)

instance Arbitrary Stray where
    arbitrary = Stray <$> elements strays
    shrink (Stray word) = Stray <$> earlier strays word

strays :: [String]
strays = ["--help", "-window", "--after", "trackkat.conf", ""]
