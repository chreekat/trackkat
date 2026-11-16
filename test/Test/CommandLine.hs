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
            \(Asked start window recorded after) ->
                parseCommand (startArgs start window recorded ++ afterArgs after) === Just (Run start window recorded after)
        , testProperty "started by hand the window opens, and otherwise only if asked" $
            \start ->
                parseCommand (startArgs start WindowHidden Unrecorded)
                    === Just (Run start (if start == ByHand then WindowShown else WindowHidden) Unrecorded Nothing)
        , testProperty "a trackkat that takes over from one recording records too, to a file of its own naming" $
            \(Asked _ _ recorded _) ->
                continued recorded === (if recorded == Unrecorded then Unrecorded else RecordedByDate)
        , testProperty "a process is named in digits alone" $
            \(Named pid) ->
                let digits = not (null pid) && all isDigit pid
                 in case parseCommand ["--after", pid] of
                        Just (Run ByHand WindowShown Unrecorded (Just n)) -> property digits .&&. afterArgs (Just n) === ["--after", dropZeros pid]
                        Nothing -> property (not digits || length (dropZeros pid) > 18)
                        other -> counterexample (show other) False
        , testProperty "anything else on the command line is refused, wherever it comes" $
            \(Asked start window recorded after) (Stray word) ->
                let args = startArgs start window recorded ++ afterArgs after
                 in forAllShrink (chooseInt (0, length args)) shrink $ \i ->
                        -- A word after --after is taken for its process, and
                        -- one after --record for its file.
                        (i == 0 || args !! (i - 1) `notElem` ["--after", "--record"]) ==>
                            parseCommand (take i args ++ [word] ++ drop i args) === Nothing
        ]
  where
    dropZeros pid = case dropWhile (== '0') pid of
        "" -> "0"
        rest -> rest

-- | How trackkat is started to track: the ways the command line can ask,
-- so by hand always with its window, and recorded to no file whose name
-- begins as an option does. Simplest started by hand, unrecorded, after
-- no process; and simpler with its window, recorded to a file of its own
-- naming, or after a process with a smaller number.
data Asked = Asked Start WindowAtStart Recorded (Maybe Int)
    deriving (Show)

instance Arbitrary Asked where
    arbitrary = do
        start <- arbitrary
        window <- if start == ByHand then pure WindowShown else elements [WindowShown, WindowHidden]
        Asked start window <$> elements recordings <*> oneof [pure Nothing, Just <$> chooseInt (0, maxBound)]
    shrink =
        shrinkParts
            [ \(Asked start _ recorded after) -> [Asked ByHand WindowShown recorded after | start /= ByHand]
            , \(Asked start window recorded after) -> [Asked start WindowShown recorded after | window /= WindowShown]
            , \(Asked start window recorded after) -> [Asked start window simpler after | simpler <- earlier recordings recorded]
            , \(Asked start window recorded after) -> [Asked start window recorded Nothing | after /= Nothing] ++ [Asked start window recorded (Just n) | Just pid <- [after], n <- shrinkFrom 0 pid]
            ]

recordings :: [Recorded]
recordings = [Unrecorded, RecordedByDate, RecordedTo "flight.txt", RecordedTo "C:\\Users\\Zoë\\a flight.txt", RecordedTo "-", RecordedTo "5"]

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
