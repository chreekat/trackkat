-- | trackkat's settings file: hotkey bindings, as @name = value@ lines.
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
import Data.Char (isSpace, toLower)
import Data.List (intercalate, nub)

data Config = Config
    { recenterKey :: Binding
    , pauseKey :: Binding
    }
    deriving (Eq, Show)

-- | TrackIR's own defaults.
defaultConfig :: Config
defaultConfig = Config{recenterKey = Binding [] "F12", pauseKey = Binding [] "F9"}

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
            (name, '=' : value) -> case (trim name, parseBinding (trim value)) of
                ("recenter", Right b) -> Right c{recenterKey = b}
                ("pause", Right b) -> Right c{pauseKey = b}
                (k, Left err) | k `elem` ["recenter", "pause"] -> Left (where_ n ++ err)
                (k, _) -> Left (where_ n ++ "unknown setting " ++ show k)
            (blank, _)
                | all isSpace blank -> Right c
                | otherwise -> Left (where_ n ++ "expected name = value")
    where_ n = "line " ++ show n ++ ": "

renderConfig :: Config -> String
renderConfig c =
    unlines
        [ "# trackkat settings. Keys are like F12, Ctrl+F9, Alt+Shift+P, NumPad5."
        , "recenter = " ++ renderBinding (recenterKey c)
        , "pause = " ++ renderBinding (pauseKey c)
        ]

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
