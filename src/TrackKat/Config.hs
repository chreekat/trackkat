-- | trackkat's settings file, as @name = value@ lines: hotkey and joystick
-- button bindings, smoothing, response curves, the clip's shape and which
-- NPClient games load.
module TrackKat.Config (
    Config (..),
    NPClient (..),
    defaultConfig,
    parseConfig,
    renderConfig,
    describeChanges,
    retiredSettings,
    settingNames,
    settingValues,
    editSettings,
    ClipShape,
    clipShape,
    clipDistances,
    clipModel,
    Binding (..),
    Modifier (..),
    Joystick (..),
    maxButton,
    parseBinding,
    parseBindings,
    renderBinding,
    renderBindings,
    renderJoystick,
    modifierMask,
    virtualKey,
    bindingFromCodes,
) where

import Data.Bits ((.&.), (.|.))
import Data.Char (digitToInt, isDigit, isSpace, toLower)
import Data.Foldable (traverse_)
import Data.List (dropWhileEnd, findIndices, intercalate, nub)
import Data.Maybe (fromMaybe)

import TrackKat.Number
import TrackKat.Pose (Model, triangle)
import TrackKat.Response

data Config = Config
    { recenterBindings :: [Binding]
    , pauseBindings :: [Binding]
    , trimBindings :: [Binding]
    , response :: Response
    , clip :: ClipShape
    , npclient :: NPClient
    }
    deriving (Eq, Show)

-- | Whose NPClient DLL games load: trackkat's own, or that of NaturalPoint's
-- TrackIR software, which is signed, so anti-cheat such as BattlEye lets
-- games load it; trackkat then stands in for NaturalPoint's software.
data NPClient = TrackKatNPClient | TrackIRNPClient
    deriving (Eq, Show)

-- | The headset clip's triangle of reflectors, as distances in mm, left and
-- right being the wearer's: left to right, left to top, right to top. Only
-- 'clipShape' makes one, so its model always exists.
data ClipShape = ClipShape (Double, Double, Double) Model
    deriving (Show)

instance Eq ClipShape where
    a == b = clipDistances a == clipDistances b

-- | A clip from its three distances, if they are positive and form a
-- triangle. The top reflector's post is taken to lean back 45 degrees. The
-- camera faces the wearer, so the model, which goes by the camera's left
-- and right, has the wearer's sides swapped.
clipShape :: Double -> Double -> Double -> Maybe ClipShape
clipShape lr lt rt
    | all (> 0) [lr, lt, rt] = ClipShape (lr, lt, rt) <$> triangle 45 lr rt lt
    | otherwise = Nothing

clipDistances :: ClipShape -> (Double, Double, Double)
clipDistances (ClipShape d _) = d

clipModel :: ClipShape -> Model
clipModel (ClipShape _ m) = m

-- | TrackIR's own keys, and none to trim; linear curves with gains tuned in
-- DCS; strong smoothing; the headset clip trackkat was developed with, its
-- post as it is now bent; trackkat's own NPClient.
defaultConfig :: Config
defaultConfig =
    Config
        { recenterBindings = [Key [] "F12"]
        , pauseBindings = [Key [] "F9"]
        , trimBindings = []
        , response =
            Response
                { smoothing = knobSmoothing defaultKnobs
                , yawCurve = gain 6
                , pitchCurve = gain 7
                , rollCurve = gain 4
                , xCurve = gain 3
                , yCurve = gain 4
                , zCurve = gain 2.5
                , lean = LeanWithView
                }
        , clip = defaultClip
        , npclient = TrackKatNPClient
        }
  where
    gain g = Points [(1, g)]
    defaultClip = case clipShape 70 110 110 of
        Just c -> c
        Nothing -> error "defaultClip"

-- | A key, or a mouse button named as one, with modifiers held; or a
-- joystick button numbered from 1.
data Binding = Key [Modifier] String | JoystickButton Joystick Int
    deriving (Eq, Show)

data Modifier = Ctrl | Alt | Shift | Win
    deriving (Eq, Show, Enum, Bounded)

-- | A game controller: its product name, as Windows reports it, and which
-- of the attached controllers with that name it is, from 1.
data Joystick = Joystick String Int
    deriving (Eq, Show)

-- | The highest joystick button number trackkat watches, as
-- MAX_BUTTONS in cbits/joystick.c.
maxButton :: Int
maxButton = 256

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
            (name, '=' : _) | trim name `elem` retired -> Right ck
            (name, '=' : value) -> either (Left . (where_ n ++)) Right (setting ck (trim name) (trim value))
            (blank, _)
                | all isSpace blank -> Right ck
                | otherwise -> Left (where_ n ++ "expected name = value")
    where_ n = "line " ++ show n ++ ": "

-- | Apply one setting, by name and value as written.
setting :: (Config, Knobs) -> String -> String -> Either String (Config, Knobs)
setting (c, k@(Knobs on s r)) name value =
    let resp = response c
        curve range f = (\cv -> (c{response = f cv resp}, k)) <$> parseCurve range value
     in case name of
            "recenter" -> (\b -> (c{recenterBindings = b}, k)) <$> parseBindings value
            "pause" -> (\b -> (c{pauseBindings = b}, k)) <$> parseBindings value
            "trim" -> (\b -> (c{trimBindings = b}, k)) <$> parseBindings value
            "smoothing" -> case value of
                "on" -> Right (c, Knobs True s r)
                "off" -> Right (c, Knobs False s r)
                _ -> Left "smoothing is on or off"
            "steadiness" -> (\v -> (c, Knobs on v r)) <$> parseKnob name value
            "responsiveness" -> (\v -> (c, Knobs on s v)) <$> parseKnob name value
            "yaw" -> curve yawRange (\cv x -> x{yawCurve = cv})
            "pitch" -> curve pitchRange (\cv x -> x{pitchCurve = cv})
            "roll" -> curve rollRange (\cv x -> x{rollCurve = cv})
            "x" -> curve leanRange (\cv x -> x{xCurve = cv})
            "y" -> curve leanRange (\cv x -> x{yCurve = cv})
            "z" -> curve leanRange (\cv x -> x{zCurve = cv})
            "lean" -> (\l -> (c{response = resp{lean = l}}, k)) <$> parseLean value
            "clip" -> (\cl -> (c{clip = cl}, k)) <$> parseClip value
            "npclient" -> (\n -> (c{npclient = n}, k)) <$> parseNPClient value
            _ -> Left ("unknown setting " ++ show name)

renderConfig :: Config -> String
renderConfig c =
    unlines
        [ "# trackkat settings."
        , "#"
        , "# Settings take effect as soon as the file is saved."
        , "#"
        , "# Keys and buttons, separated by commas, or none. For example:"
        , "#   recenter = F12, Throttle - HOTAS Warthog button 23"
        , "#   pause = Ctrl+F9, Mouse4"
        , "#   trim = NumPad5"
        , "#"
        , "# The easiest way to figure out joystick button names is by using"
        , "# trackkat's window. For a gaming mouse's extra buttons, give them F13"
        , "# to F24 in the mouse's own software and bind those."
        , settingText "recenter" (renderBindings (recenterBindings c))
        , settingText "pause" (renderBindings (pauseBindings c))
        , settingText "trim" (renderBindings (trimBindings c))
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
        , "# Response curves: how far the view turns or moves as your head does,"
        , "# the same both sides of centre. Most simply a multiplier, as 1:6: the"
        , "# view turns 6 degrees for each degree your head turns, so turning"
        , "# 30 degrees looks straight behind you."
        , "#"
        , "# Draw a curve by defining more input:output points: degrees for yaw,"
        , "# pitch and roll, mm for x, y and z. Straight lines join the points,"
        , "# starting from 0:0, and carry on past the last one. For example, a"
        , "# deadzone then 1:1 is 0.5:0 1.5:1."
        , "#"
        , "# A centre precision, from 0 to 100, makes small movements gentler and"
        , "# large ones steeper, while the same head movement still reaches the"
        , "# end of the view. It's best to get a feel for this by playing with it"
        , "# in trackkat's window. For example:"
        , "#   yaw = 1:6, precision 40"
        , "yaw = " ++ renderCurve (yawCurve r)
        , "pitch = " ++ renderCurve (pitchCurve r)
        , "roll = " ++ renderCurve (rollCurve r)
        , "x = " ++ renderCurve (xCurve r)
        , "y = " ++ renderCurve (yCurve r)
        , "z = " ++ renderCurve (zCurve r)
        , ""
        , "# lean = view: leaning moves you relative to where you look in the"
        , "# game, so with the view turned to look behind you, leaning left looks"
        , "# out of the right side. Only turning left and right counts, not"
        , "# looking up or down. lean = screen: leaning moves you the same way"
        , "# whichever way you look."
        , "lean = " ++ renderLean (lean r)
        , ""
        , "# clip: the headset clip's triangle of reflectors, in mm, measured"
        , "# centre to centre, with left and right as you wear it: your left to"
        , "# your right along the headband, your left to the top, then your"
        , "# right to the top. If the clip's post gets bent, measure it again"
        , "# and change this, or the view will be off."
        , "clip = " ++ renderClip (clip c)
        , ""
        , "# npclient = trackir: games load the NPClient of NaturalPoint's TrackIR"
        , "# software, which anti-cheat allows; start trackkat before the game."
        , "npclient = " ++ renderNPClient (npclient c)
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
         in [ ("recenter", renderBindings (recenterBindings c))
            , ("pause", renderBindings (pauseBindings c))
            , ("trim", renderBindings (trimBindings c))
            ]
                ++ ( case smoothingKnobs (smoothing r) of
                        Knobs False _ _ -> [("smoothing", "off")]
                        Knobs True s rs -> [("smoothing", "on"), ("steadiness", renderNumber s), ("responsiveness", renderNumber rs)]
                   )
                ++ [ (name, renderCurve (f r))
                   | (name, f) <- [("yaw", yawCurve), ("pitch", pitchCurve), ("roll", rollCurve), ("x", xCurve), ("y", yCurve), ("z", zCurve)]
                   ]
                ++ [("lean", renderLean (lean r)), ("clip", renderClip (clip c)), ("npclient", renderNPClient (npclient c))]

parseLean :: String -> Either String Lean
parseLean "view" = Right LeanWithView
parseLean "screen" = Right LeanWithScreen
parseLean _ = Left "lean is view or screen"

renderLean :: Lean -> String
renderLean LeanWithView = "view"
renderLean LeanWithScreen = "screen"

parseNPClient :: String -> Either String NPClient
parseNPClient "trackkat" = Right TrackKatNPClient
parseNPClient "trackir" = Right TrackIRNPClient
parseNPClient _ = Left "npclient is trackkat or trackir"

renderNPClient :: NPClient -> String
renderNPClient TrackKatNPClient = "trackkat"
renderNPClient TrackIRNPClient = "trackir"

-- | Read three distances in mm, separated by spaces or commas or both.
parseClip :: String -> Either String ClipShape
parseClip value = case traverse parseNumber (words (map (\ch -> if ch == ',' then ' ' else ch) value)) of
    Just [lr, lt, rt] -> maybe (Left "the clip's three distances don't make a triangle") Right (clipShape lr lt rt)
    _ -> Left "clip is three distances in mm, as you wear it: left to right, left to top and right to top, like 70 110 110"

renderClip :: ClipShape -> String
renderClip cl = let (lr, lt, rt) = clipDistances cl in unwords (map renderNumber [lr, lt, rt])

-- | Notes on the lines of a settings file that set something trackkat no
-- longer has, which 'parseConfig' ignores.
retiredSettings :: String -> [String]
retiredSettings str =
    [ "line " ++ show n ++ ": " ++ name ++ " is no longer a setting; ignoring it."
    | (n, Just (name, _)) <- zip [1 :: Int ..] (map settingLine (lines str))
    , name `elem` retired
    ]

-- | Settings trackkat no longer has, which older settings files still set.
retired :: [String]
retired = ["drift"]

-- | The names of the settings, in the order the default file lists them.
settingNames :: [String]
settingNames = map fst (settingLines (renderConfig defaultConfig))

-- | Each setting's value as a settings file writes it, or as the default
-- file does where this one doesn't set it.
settingValues :: String -> [(String, String)]
settingValues str = [(n, fromMaybe d (lookup n written)) | (n, d) <- settingLines (renderConfig defaultConfig)]
  where
    -- The last line setting a name is the one that counts.
    written = reverse (settingLines str)

-- | The settings a file's lines set, in order, as written.
settingLines :: String -> [(String, String)]
settingLines str = [(n, v) | Just (n, v) <- map settingLine (lines str)]

-- | A line setting a name to a value; nothing follows the = when the value
-- is empty.
settingText :: String -> String -> String
settingText n v = n ++ " =" ++ (if null v then "" else " " ++ v)

settingLine :: String -> Maybe (String, String)
settingLine l = case break (== '=') (takeWhile (/= '#') l) of
    (name, '=' : value) -> Just (trim name, trim value)
    _ -> Nothing

-- | Give settings new values in a settings file, keeping the rest of the
-- file, comments included. A changed setting's value is replaced on the line
-- that sets it (the last, if several do), keeping that line's comment, or a
-- line is added for it at the end. Left names a setting that isn't valid, or
-- the line of the result that isn't.
editSettings :: [(String, String)] -> String -> Either String String
editSettings new str
    | null changed = Right str
    | otherwise = do
        traverse_ (\(n, v) -> either (Left . ((n ++ ": ") ++)) Right (setting (defaultConfig, defaultKnobs) n v)) changed
        let edited = unlines (foldl replace (lines str) changed)
        edited <$ parseConfig edited
  where
    current = settingValues str
    changed = [(n, v) | (n, v) <- new, lookup n current /= Just v]
    replace ls (n, v) = case findIndices ((== Just n) . fmap fst . settingLine) ls of
        [] -> ls ++ [settingText n v]
        is -> [if i == last is then rewrite l else l | (i, l) <- zip [0 :: Int ..] ls]
      where
        rewrite l =
            let (name, rest) = break (== '=') l
                comment = dropWhile (/= '#') rest
             in settingText (dropWhileEnd isSpace name) v ++ (if null comment then "" else " " ++ comment)

-- | Read a curve: points like @0.5:0 1.5:1@, inputs increasing from zero,
-- or a multiplier with a centre precision like @1:6, precision 40@, given
-- the end of the axis's view.
parseCurve :: Double -> String -> Either String Curve
parseCurve range value = case splitOn ',' value of
    [points] -> Points <$> parsePoints points
    [points, extra] -> do
        m <- case parsePoints points of
            Right [(i, o)] | o > 0 -> Right (o / i)
            Right _ -> Left "a centre precision goes with one input:output point, both above 0, like 1:6, precision 40"
            Left err -> Left err
        p <- case words extra of
            ["precision", n] -> parseKnob "precision" n
            _ -> Left ("expected precision and a number, not " ++ show (trim extra))
        Right (Expo (Multiplier m p range))
    _ -> Left "a curve has at most one comma, before its precision"

parsePoints :: String -> Either String [(Double, Double)]
parsePoints value = do
    points <- traverse point (words value)
    let inputs = map fst points
    if null points
        then Left "a curve needs at least one input:output point"
        else
            if or (zipWith (>=) (0 : inputs) inputs) || any ((< 0) . fst) points
                then Left "curve inputs must be positive and increasing"
                else Right points
  where
    point w = case break (== ':') w of
        (i, ':' : o) | Just a <- parseNumber i, Just b <- parseNumber o -> Right (a, b)
        _ -> Left ("expected input:output, not " ++ show w)

renderCurve :: Curve -> String
renderCurve (Points points) = unwords [renderNumber a ++ ":" ++ renderNumber b | (a, b) <- points]
renderCurve (Expo e) = "1:" ++ renderNumber (factor e) ++ ", precision " ++ renderNumber (precision e)

-- | Read a smoothing knob, a number from 0 to 100.
parseKnob :: String -> String -> Either String Double
parseKnob name value = case parseNumber value of
    Just v | v >= 0, v <= 100 -> Right v
    _ -> Left (name ++ " is a number from 0 to 100")

-- | Read bindings separated by commas, such as
-- @F12, Throttle - HOTAS Warthog button 23@, or none from an empty value.
parseBindings :: String -> Either String [Binding]
parseBindings value
    | all isSpace value = Right []
    | otherwise = traverse parseBinding (splitOn ',' value)

renderBindings :: [Binding] -> String
renderBindings = intercalate ", " . map renderBinding

-- | Read a binding: a key or mouse button such as @Ctrl+Shift+F12@ or
-- @Mouse4@, modifiers then one key, or a joystick button such as @Throttle - HOTAS Warthog (2) button 23@.
parseBinding :: String -> Either String Binding
parseBinding s = case joystickButton (trim s) of
    Just (name, number) -> do
        j <- parseJoystick name
        n <- case number of
            Just n | n >= 1, n <= toInteger maxButton -> Right (fromInteger n)
            _ -> Left ("joystick buttons are numbered from 1 to " ++ show maxButton)
        Right (JoystickButton j n)
    Nothing -> case splitPlus s of
        [] -> Left "empty key binding"
        parts -> do
            mods <- traverse modifier (init parts)
            let k = last parts
            case lookupKey k of
                Just (canonical, _) -> Right (Key (nub mods) canonical)
                Nothing
                    | map toLower k `elem` ["mouseleft", "mouseright", "mouse1", "mouse2"] ->
                        Left "the left and right mouse buttons can't be bound; use MouseMiddle, Mouse4 or Mouse5"
                    | otherwise -> Left ("unknown key " ++ show k)
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
renderBinding (Key mods k) = intercalate "+" (map show [m | m <- [minBound ..], m `elem` mods] ++ [k])
renderBinding (JoystickButton j n) = renderJoystick j ++ " button " ++ show n

-- | Split a binding ending in @button N@ into the joystick's name and the
-- button number, if it has one.
joystickButton :: String -> Maybe (String, Maybe Integer)
joystickButton s =
    let (number, beforeNumber) = span isDigit (reverse s)
        (gap, beforeGap) = span isSpace beforeNumber
        (word, name) = splitAt (length "button") beforeGap
     in if map toLower (reverse word) == "button" && all isSpace (take 1 name) && (not (null gap) || null number)
            then Just (trim (reverse name), if null number then Nothing else Just (decimal (reverse number)))
            else Nothing

-- | Read a joystick's name, perhaps followed by which of those with that
-- name it is, as in @Throttle - HOTAS Warthog (2)@.
parseJoystick :: String -> Either String Joystick
parseJoystick name = case reverse name of
    [] -> Left "a joystick button needs the joystick's name, like Throttle - HOTAS Warthog button 23"
    ')' : rest
        | (index@(_ : _), '(' : beforeIndex) <- span isDigit rest
        , let n = decimal (reverse index)
        , n >= 1
        , not (null (trim beforeIndex)) ->
            Right (Joystick (trim (reverse beforeIndex)) (fromInteger n))
    _ -> Right (Joystick name 1)

renderJoystick :: Joystick -> String
renderJoystick (Joystick name index) = name ++ (if index > 1 then " (" ++ show index ++ ")" else "")

-- | The modifier flags Windows' RegisterHotKey takes.
modifierMask :: [Modifier] -> Int
modifierMask mods = foldl (.|.) 0 (map modifierBit mods)

modifierBit :: Modifier -> Int
modifierBit Alt = 0x1
modifierBit Ctrl = 0x2
modifierBit Shift = 0x4
modifierBit Win = 0x8

-- | The binding for RegisterHotKey's modifier flags and a virtual-key code;
-- Nothing if the key has no name here.
bindingFromCodes :: Int -> Int -> Maybe Binding
bindingFromCodes mask vk = case [name | (name, code) <- keys, code == vk] of
    name : _ -> Just (Key [m | m <- [minBound ..], mask .&. modifierBit m /= 0] name)
    [] -> Nothing

-- | The Windows virtual-key code of a key.
virtualKey :: String -> Int
virtualKey k = maybe 0 snd (lookupKey k)

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
           , -- Mouse buttons, by their virtual-key codes.
             ("MouseMiddle", 0x04)
           , ("Mouse4", 0x05)
           , ("Mouse5", 0x06)
           ]

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

splitOn :: Char -> String -> [String]
splitOn c str = case break (== c) str of
    (a, []) -> [a]
    (a, _ : rest) -> a : splitOn c rest

-- | The number a string of decimal digits spells.
decimal :: String -> Integer
decimal = foldl (\acc d -> acc * 10 + toInteger (digitToInt d)) 0
