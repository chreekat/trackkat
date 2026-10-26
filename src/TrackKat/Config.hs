-- | trackkat's settings file, as @name = value@ lines: hotkey bindings,
-- smoothing and response curves.
module TrackKat.Config (
    Config (..),
    defaultConfig,
    parseConfig,
    renderConfig,
    Binding (..),
    Modifier (..),
    parseBinding,
    renderBinding,
    modifierMask,
    virtualKey,
) where

import Data.Bits ((.|.))
import Data.Char (digitToInt, isDigit, isSpace, toLower)
import Data.List (intercalate, nub)
import Text.Printf (printf)

import TrackKat.Response

data Config = Config
    { recenterKey :: Binding
    , pauseKey :: Binding
    , response :: Response
    }
    deriving (Eq, Show)

-- | TrackIR's own keys; a small deadzone then 1:1 on every axis; moderate
-- smoothing.
defaultConfig :: Config
defaultConfig =
    Config
        { recenterKey = Binding [] "F12"
        , pauseKey = Binding [] "F9"
        , response =
            Response
                { smoothing = Smoothing{minCutoff = 1, beta = 0.05}
                , yawCurve = rotation
                , pitchCurve = rotation
                , rollCurve = rotation
                , xCurve = translation
                , yCurve = translation
                , zCurve = translation
                }
        }
  where
    rotation = Curve [(0.5, 0), (1.5, 1)]
    translation = Curve [(1, 0), (2, 1)]

-- | A key, with modifiers held.
data Binding = Binding [Modifier] String
    deriving (Eq, Show)

data Modifier = Ctrl | Alt | Shift | Win
    deriving (Eq, Show, Enum, Bounded)

-- | Read a settings file. Unmentioned settings keep their defaults; the
-- error names the offending line.
parseConfig :: String -> Either String Config
parseConfig = foldl line (Right defaultConfig) . zip [1 :: Int ..] . lines
  where
    line acc (n, l) =
        acc >>= \c -> case break (== '=') (takeWhile (/= '#') l) of
            (name, '=' : value) -> either (Left . (where_ n ++)) Right (setting c (trim name) (trim value))
            (blank, _)
                | all isSpace blank -> Right c
                | otherwise -> Left (where_ n ++ "expected name = value")
    where_ n = "line " ++ show n ++ ": "
    setting c name value =
        let r = response c
            curve f = (\cv -> c{response = f cv r}) <$> parseCurve value
         in case name of
                "recenter" -> (\b -> c{recenterKey = b}) <$> parseBinding value
                "pause" -> (\b -> c{pauseKey = b}) <$> parseBinding value
                "smoothing" -> (\sm -> c{response = r{smoothing = sm}}) <$> parseSmoothing value
                "yaw" -> curve (\cv x -> x{yawCurve = cv})
                "pitch" -> curve (\cv x -> x{pitchCurve = cv})
                "roll" -> curve (\cv x -> x{rollCurve = cv})
                "x" -> curve (\cv x -> x{xCurve = cv})
                "y" -> curve (\cv x -> x{yCurve = cv})
                "z" -> curve (\cv x -> x{zCurve = cv})
                _ -> Left ("unknown setting " ++ show name)

renderConfig :: Config -> String
renderConfig c =
    unlines
        [ "# trackkat settings."
        , "#"
        , "# Keys are like F12, Ctrl+F9, Alt+Shift+P, NumPad5."
        , "recenter = " ++ renderBinding (recenterKey c)
        , "pause = " ++ renderBinding (pauseKey c)
        , ""
        , "# Smoothing: cutoff in Hz (lower is steadier when still) and"
        , "# responsiveness (higher lags less when moving), or \"off\"."
        , "smoothing = " ++ renderSmoothing (smoothing r)
        , ""
        , "# Response curves, as input:output points: degrees for yaw, pitch and"
        , "# roll, mm for x, y and z. Straight lines join the points, starting"
        , "# from 0:0, and carry on past the last one. The same both sides of"
        , "# centre. For example, a deadzone then 1:1 is 0.5:0 1.5:1, and"
        , "# 10:10 45:120 looks well behind you when you turn your head 45 degrees."
        , "yaw = " ++ renderCurve (yawCurve r)
        , "pitch = " ++ renderCurve (pitchCurve r)
        , "roll = " ++ renderCurve (rollCurve r)
        , "x = " ++ renderCurve (xCurve r)
        , "y = " ++ renderCurve (yCurve r)
        , "z = " ++ renderCurve (zCurve r)
        ]
  where
    r = response c

-- | Read curve points like @0.5:0 1.5:1@, inputs increasing from zero.
parseCurve :: String -> Either String Curve
parseCurve value = do
    points <- traverse point (words value)
    let inputs = map fst points
    if null points
        then Left "a curve needs at least one input:output point"
        else
            if or (zipWith (>=) (0 : inputs) inputs) || any ((< 0) . fst) points
                then Left "curve inputs must be positive and increasing"
                else Right (Curve points)
  where
    point w = case break (== ':') w of
        (i, ':' : o) | Just a <- parseNumber i, Just b <- parseNumber o -> Right (a, b)
        _ -> Left ("expected input:output, not " ++ show w)

renderCurve :: Curve -> String
renderCurve (Curve points) = unwords [renderNumber a ++ ":" ++ renderNumber b | (a, b) <- points]

parseSmoothing :: String -> Either String Smoothing
parseSmoothing value = case words value of
    ["off"] -> Right NoSmoothing
    [a, b]
        | Just mc <- parseNumber a
        , Just be <- parseNumber b
        , mc > 0
        , be >= 0 ->
            Right (Smoothing mc be)
    _ -> Left "smoothing wants a cutoff in Hz and a responsiveness, like 1 0.05, or off"

renderSmoothing :: Smoothing -> String
renderSmoothing NoSmoothing = "off"
renderSmoothing (Smoothing mc be) = renderNumber mc ++ " " ++ renderNumber be

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

-- | Read a binding such as @Ctrl+Shift+F12@: modifiers, then one key.
parseBinding :: String -> Either String Binding
parseBinding s = case splitPlus s of
    [] -> Left "empty key binding"
    parts -> do
        mods <- traverse modifier (init parts)
        let k = last parts
        case lookupKey k of
            Just (canonical, _) -> Right (Binding (nub mods) canonical)
            Nothing -> Left ("unknown key " ++ show k)
  where
    splitPlus str = case break (== '+') str of
        (a, []) -> [trim a | not (all isSpace a)]
        (a, _ : rest) -> trim a : splitPlus rest
    modifier m = case map toLower m of
        "ctrl" -> Right Ctrl
        "control" -> Right Ctrl
        "alt" -> Right Alt
        "shift" -> Right Shift
        "win" -> Right Win
        _ -> Left ("unknown modifier " ++ show m)

renderBinding :: Binding -> String
renderBinding (Binding mods k) = intercalate "+" (map show [m | m <- [minBound ..], m `elem` mods] ++ [k])

-- | The modifier flags Windows' RegisterHotKey takes.
modifierMask :: Binding -> Int
modifierMask (Binding mods _) = foldl (.|.) 0 (map bit mods)
  where
    bit Alt = 0x1
    bit Ctrl = 0x2
    bit Shift = 0x4
    bit Win = 0x8

-- | The Windows virtual-key code of a binding's key.
virtualKey :: Binding -> Int
virtualKey (Binding _ k) = maybe 0 snd (lookupKey k)

-- | A key's canonical name and virtual-key code, ignoring case.
lookupKey :: String -> Maybe (String, Int)
lookupKey k = case [kv | kv@(name, _) <- keys, map toLower name == map toLower k] of
    kv : _ -> Just kv
    [] -> Nothing

keys :: [(String, Int)]
keys =
    [("F" ++ show n, 0x6F + n) | n <- [1 .. 24]]
        ++ [([c], fromEnum c) | c <- ['A' .. 'Z'] ++ ['0' .. '9']]
        ++ [("NumPad" ++ show n, 0x60 + n) | n <- [0 .. 9]]
        ++ [ ("NumPadMultiply", 0x6A)
           , ("NumPadAdd", 0x6B)
           , ("NumPadSubtract", 0x6D)
           , ("NumPadDecimal", 0x6E)
           , ("NumPadDivide", 0x6F)
           , ("Pause", 0x13)
           , ("ScrollLock", 0x91)
           , ("Insert", 0x2D)
           , ("Delete", 0x2E)
           , ("Home", 0x24)
           , ("End", 0x23)
           , ("PageUp", 0x21)
           , ("PageDown", 0x22)
           , ("Space", 0x20)
           , ("Up", 0x26)
           , ("Down", 0x28)
           , ("Left", 0x25)
           , ("Right", 0x27)
           ]

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace
