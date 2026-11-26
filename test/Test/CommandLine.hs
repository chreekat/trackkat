-- | trackkat's command line.
module Test.CommandLine (tests) where

import Data.Char (isDigit)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
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
        , testProperty "a program started with arguments gets them as given, however they are written" $
            \(Arguments args) ->
                let exe = "C:\\Program Files\\trackkat\\trackkat.exe"
                 in splitCommandLine (commandLine exe args) === exe : args
        , testCase "command lines split as Microsoft's examples do" $
            map splitCommandLine
                [ "p \"a b c\" d e"
                , "p \"ab\\\"c\" \"\\\\\" d"
                , "p a\\\\\\b d\"e f\"g h"
                , "p a\\\\\\\"b c d"
                , "p a\\\\\\\\\"b c\" d e"
                , "p a\"b\"\" c d"
                ]
                @?= [ ["p", "a b c", "d", "e"]
                    , ["p", "ab\"c", "\\", "d"]
                    , ["p", "a\\\\\\b", "de fg", "h"]
                    , ["p", "a\\\"b", "c", "d"]
                    , ["p", "a\\\\b c", "d", "e"]
                    , ["p", "ab\" c d"]
                    ]
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

-- | Arguments of the letters that command lines treat specially, among a
-- few others. Simpler of fewer, shorter arguments, of plainer letters.
newtype Arguments = Arguments [String]
    deriving (Show)

instance Arbitrary Arguments where
    arbitrary = Arguments <$> listOf (listOf (elements letters))
    shrink (Arguments args) = Arguments <$> shrinkList (shrinkList (\l -> earlier letters l)) args

letters :: String
letters = "a \"\\\tZ\233\1078"

-- | A command line split into its program and arguments, by the rules of
-- Microsoft's C runtime and CommandLineToArgvW: the program up to a space,
-- or in quotes; then arguments split at spaces and tabs outside quotes,
-- where backslashes before a quote are halved, an odd one left escaping
-- the quote, and two quotes inside quotes are one.
splitCommandLine :: String -> [String]
splitCommandLine line = case line of
    '"' : rest -> let (p, after) = break (== '"') rest in p : arguments (drop 1 after)
    _ -> let (p, after) = break (`elem` " \t") line in p : arguments after
  where
    arguments s = case dropWhile (`elem` " \t") s of
        "" -> []
        s' -> argument False "" s'
    argument quoted acc s = case s of
        '\\' : _
            | (slashes, '"' : rest) <- span (== '\\') s ->
                let n = length slashes
                    kept = replicate (n `div` 2) '\\'
                 in if odd n then argument quoted ('"' : kept ++ acc) rest else quote quoted (kept ++ acc) rest
        '"' : rest -> quote quoted acc rest
        c : rest
            | not quoted, c `elem` " \t" -> reverse acc : arguments rest
            | otherwise -> argument quoted (c : acc) rest
        "" -> [reverse acc]
    quote quoted acc s = case s of
        '"' : rest | quoted -> argument True ('"' : acc) rest
        _ -> argument (not quoted) acc s
