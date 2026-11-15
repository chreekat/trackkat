-- | Decimal numbers as trackkat's text files write them.
module Test.Number (tests) where

import Data.Char (isDigit)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Test.Gen (close, genWritten, shrinkWithin, shrinkWritten)
import TrackKat.Number

tests :: TestTree
tests =
    testGroup
        "Number"
        [ testProperty "a number read from up to four decimals is read back the same once written" $
            \(Written x) ->
                parseNumber (renderNumber x) === Just x
        , testProperty "a number is written to the nearest ten-thousandth, with no more decimals than it needs" $
            \(UpToAMillion x) ->
                let text = renderNumber x
                    decimals = drop 1 (dropWhile (/= '.') text)
                 in counterexample text $
                        conjoin
                            [ maybe (property False) (close 5.0001e-5 x) (parseNumber text)
                            , property (length decimals <= 4)
                            , property (null decimals || last decimals /= '0')
                            , property (last text /= '.')
                            ]
        , testProperty "a number written exactly is read back as the very same number" $ \x ->
            parseExact (renderExact x) === Just x
        , -- Earlier recordings' cameras were written so.
          testProperty "a number written as show writes one is read as that number" $ \x ->
            parseExact (show (x :: Double)) === Just x
        , testProperty "a number of an everyday size is written exactly as show writes it" $
            \(Everyday x) ->
                renderExact x === show x .&&. renderExact (negate x) === show (negate x)
        , testProperty "a whole number is read back as written, in digits alone" $ \(NonNegative n) ->
            parseNatural (renderNatural n) === Just n .&&. property (all isDigit (renderNatural n))
        , testProperty "a number written, read and written again is written the same" $
            \(UpToAMillion x) ->
                fmap renderNumber (parseNumber (renderNumber x)) === Just (renderNumber x)
        , testProperty "digits, with a point before some of them or none, are a number, negative after a minus, and nothing else is" $
            \(Scrawl text) ->
                let (whole, rest) = span isDigit text
                    decimal = case rest of
                        "" -> not (null whole)
                        '.' : more -> not (null more) && all isDigit more
                        _ -> False
                 in (parseNumber text /= Nothing) === decimal .&&. parseNumber ('-' : text) === fmap negate (parseNumber text)
        ]

-- | A number as trackkat's text files give one, of up to four decimals and
-- up to ten thousand either way (see 'genWritten').
newtype Written = Written Double
    deriving (Show)

instance Arbitrary Written where
    arbitrary = Written <$> genWritten (-100000000, 100000000)
    shrink (Written x) = Written <$> shrinkWritten x

-- | Any number up to a million either way.
newtype UpToAMillion = UpToAMillion Double
    deriving (Show)

instance Arbitrary UpToAMillion where
    arbitrary = UpToAMillion <$> choose (-1e6, 1e6)
    shrink (UpToAMillion x) = UpToAMillion <$> shrinkWithin (-1e6, 1e6) 0 x

-- | A number of an everyday size: from a tenth to short of ten million,
-- between which show writes a number without an exponent.
newtype Everyday = Everyday Double
    deriving (Show)

instance Arbitrary Everyday where
    arbitrary = Everyday <$> choose (0.1, 9999999)
    shrink (Everyday x) = Everyday <$> shrinkWithin (0.1, 9999999) 1 x

-- | Text of what numbers are written with, and a little else; simpler
-- shorter.
newtype Scrawl = Scrawl String
    deriving (Show)

instance Arbitrary Scrawl where
    arbitrary = Scrawl <$> listOf (elements "0123456789.e ")
    shrink (Scrawl text) = Scrawl <$> shrinkList (const []) text
