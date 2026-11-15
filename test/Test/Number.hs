-- | Decimal numbers as trackkat's text files write them.
module Test.Number (tests) where

import Data.Char (isDigit)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Test.Gen (close, genWritten)
import TrackKat.Number

tests :: TestTree
tests =
    testGroup
        "Number"
        [ testProperty "a number read from up to four decimals is read back the same once written" $
            forAllShrink (genWritten (-100000000, 100000000)) shrinkNothing $ \x ->
                parseNumber (renderNumber x) === Just x
        , testProperty "a number is written to the nearest ten-thousandth, with no more decimals than it needs" $
            forAll (choose (-1e6, 1e6)) $ \x ->
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
            forAll (choose (0.1, 9999999)) $
                \x -> renderExact x === show x .&&. renderExact (negate x) === show (negate x)
        , testProperty "a whole number is read back as written, in digits alone" $ \(NonNegative n) ->
            parseNatural (renderNatural n) === Just n .&&. property (all isDigit (renderNatural n))
        , testProperty "a number written, read and written again is written the same" $
            forAll (choose (-1e6, 1e6)) $ \x ->
                fmap renderNumber (parseNumber (renderNumber x)) === Just (renderNumber x)
        , testProperty "digits, with a point before some of them or none, are a number, negative after a minus, and nothing else is" $
            forAll (listOf (elements "0123456789.e ")) $ \text ->
                let (whole, rest) = span isDigit text
                    decimal = case rest of
                        "" -> not (null whole)
                        '.' : more -> not (null more) && all isDigit more
                        _ -> False
                 in (parseNumber text /= Nothing) === decimal .&&. parseNumber ('-' : text) === fmap negate (parseNumber text)
        ]
