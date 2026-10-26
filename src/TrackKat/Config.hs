-- | trackkat's settings file, as @name = value@ lines: hotkey bindings,
-- smoothing and response curves.
module TrackKat.Config (
    Config (..),
    defaultConfig,
    parseConfig,
    renderConfig,
    describeChanges,
    Binding (..),
    Modifier (..),
    parseBinding,
    renderBinding,
    modifierMask,
    virtualKey,
) where

import Data.Bits ((.|.))
import Data.Char (isSpace, toLower)
import Data.List (intercalate, nub)

import TrackKat.Number
import TrackKat.Response

data Config = Config
    { recenterKey :: Binding
    , pauseKey :: Binding
    , response :: Response
    }
    deriving (Eq, Show)

-- | TrackIR's own keys; linear curves with gains tuned in DCS; strong
-- smoothing.
defaultConfig :: Config
defaultConfig =
    Config
        { recenterKey = Binding [] "F12"
        , pauseKey = Binding [] "F9"
        , response =
            Response
                { smoothing = knobSmoothing defaultKnobs
                , yawCurve = gain 6
                , pitchCurve = gain 7
                , rollCurve = gain 4
                , xCurve = gain 3
                , yCurve = gain 4
                , zCurve = gain 2.5
                }
        }
  where
    gain g = Curve [(1, g)]

-- | A key, with modifiers held.
data Binding = Binding [Modifier] String
    deriving (Eq, Show)

data Modifier = Ctrl | Alt | Shift | Win
    deriving (Eq, Show, Enum, Bounded)

-- | The smoothing settings as written: on or off, then steadiness and
-- responsiveness, each 0 to 100.
data Knobs = Knobs Bool Double Double

-- | A cutoff of about 1 Hz and a beta of about 0.05.
defaultKnobs :: Knobs
defaultKnobs = Knobs True 90 45

knobSmoothing :: Knobs -> Smoothing
knobSmoothing (Knobs on s r) = if on then tuned s r else NoSmoothing

-- | The knobs that give a smoothing, inverting 'tuned'; off keeps the
-- default knobs.
smoothingKnobs :: Smoothing -> Knobs
smoothingKnobs NoSmoothing = let Knobs _ s r = defaultKnobs in Knobs False s r
smoothingKnobs (Smoothing mc b) = Knobs True (100 * logBase 0.01 (mc / 20)) (100 * logBase 1000 (b / 0.001))

-- | Read a settings file. Unmentioned settings keep their defaults; the
-- error names the offending line. The smoothing settings combine, so their
-- order doesn't matter.
parseConfig :: String -> Either String Config
parseConfig str = do
    (c, k) <- foldl line (Right (defaultConfig, defaultKnobs)) (zip [1 :: Int ..] (lines str))
    pure c{response = (response c){smoothing = knobSmoothing k}}
  where
    line acc (n, l) =
        acc >>= \ck -> case break (== '=') (takeWhile (/= '#') l) of
            (name, '=' : value) -> either (Left . (where_ n ++)) Right (setting ck (trim name) (trim value))
            (blank, _)
                | all isSpace blank -> Right ck
                | otherwise -> Left (where_ n ++ "expected name = value")
    where_ n = "line " ++ show n ++ ": "
    setting (c, k@(Knobs on s r)) name value =
        let resp = response c
            curve f = (\cv -> (c{response = f cv resp}, k)) <$> parseCurve value
         in case name of
                "recenter" -> (\b -> (c{recenterKey = b}, k)) <$> parseBinding value
                "pause" -> (\b -> (c{pauseKey = b}, k)) <$> parseBinding value
                "smoothing" -> case value of
                    "on" -> Right (c, Knobs True s r)
                    "off" -> Right (c, Knobs False s r)
                    _ -> Left "smoothing is on or off"
                "steadiness" -> (\v -> (c, Knobs on v r)) <$> parseKnob name value
                "responsiveness" -> (\v -> (c, Knobs on s v)) <$> parseKnob name value
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
        , "# Smoothing calms the view while your head is still, at the cost of"
        , "# some lag when you move. Both settings run from 0 to 100."
        , "#"
        , "# steadiness: how still the view stays while your head is still. If"
        , "# the view jitters, raise it; if it feels floaty or drifts into place,"
        , "# lower it."
        , "#"
        , "# responsiveness: how closely the view keeps up when you move. If it"
        , "# lags behind your head, raise it; if it jitters as you move slowly,"
        , "# lower it."
        , "#"
        , "# smoothing = off turns smoothing off altogether."
        , "smoothing = " ++ (if on then "on" else "off")
        , "steadiness = " ++ renderNumber s
        , "responsiveness = " ++ renderNumber rs
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
    Knobs on s rs = smoothingKnobs (smoothing r)

-- | A line for the console saying which settings differ in the new config.
describeChanges :: Config -> Config -> String
describeChanges old new = case [n ++ " = " ++ v | (n, v) <- settings new, lookup n (settings old) /= Just v] of
    [] -> "Settings reloaded."
    changes -> "Settings reloaded: " ++ intercalate ", " changes
  where
    settings c =
        let r = response c
         in [("recenter", renderBinding (recenterKey c)), ("pause", renderBinding (pauseKey c))]
                ++ ( case smoothingKnobs (smoothing r) of
                        Knobs False _ _ -> [("smoothing", "off")]
                        Knobs True s rs -> [("smoothing", "on"), ("steadiness", renderNumber s), ("responsiveness", renderNumber rs)]
                   )
                ++ [ (name, renderCurve (f r))
                   | (name, f) <- [("yaw", yawCurve), ("pitch", pitchCurve), ("roll", rollCurve), ("x", xCurve), ("y", yCurve), ("z", zCurve)]
                   ]

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

-- | Read a smoothing knob, a number from 0 to 100.
parseKnob :: String -> String -> Either String Double
parseKnob name value = case parseNumber value of
    Just v | v >= 0, v <= 100 -> Right v
    _ -> Left (name ++ " is a number from 0 to 100")

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
