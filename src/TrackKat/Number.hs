-- | Decimal numbers as trackkat's text files write them.
module TrackKat.Number (
    parseNumber,
    renderNumber,
) where

import Data.Char (digitToInt, isDigit)
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

-- | A number with no more decimals than it needs, up to four.
renderNumber :: Double -> String
renderNumber x =
    let fixed = printf "%.4f" x :: String
        trimmed = reverse (dropWhile (== '0') (reverse fixed))
     in if last trimmed == '.' then init trimmed else trimmed
