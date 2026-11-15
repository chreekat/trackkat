-- | Decimal numbers as trackkat's text files write them.
module TrackKat.Number (
    parseNumber,
    renderNumber,
    parseExact,
    renderExact,
    parseNatural,
    renderNatural,
) where

import Control.Monad (guard)
import Data.Char (digitToInt, intToDigit, isDigit)
import Data.Ratio ((%))
import Numeric (floatToDigits)
import Text.Printf (printf)

-- | A decimal number such as @-12@, @0.5@ or @.25@.
parseNumber :: String -> Maybe Double
parseNumber ('-' : rest) = negate <$> parseNumber rest
parseNumber str = case break (== '.') str of
    (whole, frac)
        | all isDigit whole
        , fracDigits <- drop 1 frac
        , all isDigit fracDigits
        , not (null whole && null fracDigits)
        , frac == "" || not (null fracDigits) ->
            Just (digits whole + digits fracDigits / 10 ^^ length fracDigits)
    _ -> Nothing
  where
    digits = foldl (\acc d -> acc * 10 + fromIntegral (digitToInt d)) 0

-- | The number nearest a decimal such as @660.2235@ or @-0.5@, with digits
-- either side of its point, or, as small and large ones were once written,
-- @5.0e-2@.
parseExact :: String -> Maybe Double
parseExact ('-' : rest) = do
    guard (take 1 rest /= "-")
    negate <$> parseExact rest
parseExact str = do
    let (mantissa, power) = break (== 'e') str
        (whole, point) = break (== '.') mantissa
        decimals = drop 1 point
    guard (not (null whole) && (null point || not (null decimals)))
    digits <- parseNatural (whole ++ decimals)
    tens <- case power of
        "" -> Just 0
        'e' : '-' : by -> negate <$> upTo3 by
        'e' : by -> upTo3 by
        _ -> Nothing
    pure (fromRational (digits % 10 ^ length decimals * 10 ^^ tens))
  where
    -- Numbers have exponents of up to three digits.
    upTo3 by = guard (length by <= 3) *> parseNatural by

-- | A number, not infinite or not a number, in the fewest digits that
-- 'parseExact' reads back as the very same number, with at least one either
-- side of its point: @660.2235@, @320.0@.
renderExact :: Double -> String
renderExact x
    | x < 0 || isNegativeZero x = '-' : renderExact (negate x)
    | places <= 0 = "0." ++ replicate (negate places) '0' ++ digits
    | otherwise = case splitAt places (digits ++ replicate (places - length digits) '0') of
        (whole, "") -> whole ++ ".0"
        (whole, decimals) -> whole ++ "." ++ decimals
  where
    (ds, places) = floatToDigits 10 x
    digits = map intToDigit ds

-- | The whole number, zero or more, that one or more decimal digits spell.
parseNatural :: String -> Maybe Integer
parseNatural str = do
    guard (not (null str) && all isDigit str)
    pure (foldl (\acc d -> acc * 10 + toInteger (digitToInt d)) 0 str)

-- | A whole number, zero or more, in decimal digits.
renderNatural :: Integer -> String
renderNatural n
    | n < 10 = [intToDigit (fromInteger n)]
    | otherwise = renderNatural (n `div` 10) ++ [intToDigit (fromInteger (n `mod` 10))]

-- | A number with no more decimals than it needs, up to four.
renderNumber :: Double -> String
renderNumber x =
    let fixed = printf "%.4f" x :: String
        trimmed = reverse (dropWhile (== '0') (reverse fixed))
     in if last trimmed == '.' then init trimmed else trimmed
