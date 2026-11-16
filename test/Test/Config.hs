{-# LANGUAGE LambdaCase #-}

-- | The settings file: read, written, described and edited.
module Test.Config (tests) where

import Control.Exception (IOException, try)
import Control.Monad (forM_)
import Data.Either (isLeft)
import Data.List (intercalate, isPrefixOf)
import System.Environment (lookupEnv)
import System.IO (readFile')
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import Test.Tasty.QuickCheck

import Test.Check
import Test.Gen
import Test.Instances ()
import TrackKat.Config
import TrackKat.Response

tests :: TestTree
tests = group "Config" $ do
    let deadzone = Points [(0.5, 0), (1.5, 1)]
        lookBehind = Points [(10, 10), (45, 120)]
        closeTo a b = abs (a - b) < 1e-9
    -- Settings and key bindings.
    check "default config round-trips" $ parseConfig (renderConfig defaultConfig) == Right defaultConfig
    check "bindings" $
        parseConfig "# comment\nrecenter = ctrl + shift+f12\n\npause=Pause # TrackIR style\n"
            == Right defaultConfig{recenterBindings = [tap (Key [Ctrl, Shift] "F12")], pauseBindings = [tap (Key [] "Pause")]}
    check "binding codes" $ case parseBinding "Alt+Ctrl+NumPad5" of
        Right b@(Binding (Key mods k) Tap) -> (modifierMask mods, virtualKey k, renderBinding b) == (3, 0x65, "Ctrl+Alt+NumPad5")
        _ -> False
    let warthog = Joystick "Throttle - HOTAS Warthog"
    check "joystick button" $
        parseConfig "recenter = Throttle - HOTAS Warthog  button 23\npause = F9, Joystick - HOTAS Warthog (2) Button 4, Ctrl+P\n"
            == Right
                defaultConfig
                    { recenterBindings = [tap (JoystickButton (warthog 1) 23)]
                    , pauseBindings = map tap [Key [] "F9", JoystickButton (Joystick "Joystick - HOTAS Warthog" 2) 4, Key [Ctrl] "P"]
                    }
    forM_
        [ [JoystickButton (warthog 1) 1]
        , [JoystickButton (warthog 3) 128, Key [Alt] "F1"]
        , [JoystickButton (Joystick "Controller (Xbox One For Windows)" 1) 7]
        , [JoystickButton (Joystick "T.16000M (HID)" 2) 256, JoystickButton (Joystick "VPC Stick MT-50CM2 + 3D" 1) 31]
        ]
        $ \inputs -> let bs = map tap inputs in checkShowing "bindings round-trip" (renderBindings bs) $ parseBindings (renderBindings bs) == Right bs
    check "rendered button" $ renderBindings (map tap [Key [] "F12", JoystickButton (warthog 2) 23]) == "F12, Throttle - HOTAS Warthog (2) button 23"
    check "button out of range" $ parseConfig "pause = Stick button 257" == Left "line 1: joystick buttons are numbered from 1 to 256"
    check "button zero" $ parseBinding "Stick button 0" == Left "joystick buttons are numbered from 1 to 256"
    check "button without number" $ parseBinding "Stick button" == Left "joystick buttons are numbered from 1 to 256"
    check "button without joystick" $
        parseBinding "button 3" == Left "name the joystick, like Throttle - HOTAS Warthog button 23"
    check "mouse buttons" $
        parseBindings "MouseMiddle, ctrl+mouse4, Alt+Shift+Mouse5" == Right (map tap [Key [] "MouseMiddle", Key [Ctrl] "Mouse4", Key [Alt, Shift] "Mouse5"])
    check "mouse buttons round-trip" $
        let bs = map tap [Key [Ctrl] "Mouse4", Key [] "MouseMiddle", Key [Win] "Mouse5"] in parseBindings (renderBindings bs) == Right bs
    check "mouse button codes" $ map virtualKey ["MouseMiddle", "Mouse4", "Mouse5"] == [4, 5, 6]
    check "mouse button chosen in the window" $ keyFromCodes 0x2 0x05 == Just (Key [Ctrl] "Mouse4")
    forM_ ["MouseLeft", "Ctrl+MouseRight", "mouse1", "Mouse2"] $ \b ->
        checkShowing "left and right mouse buttons refused" (b) $
            parseBinding b == Left "the mouse buttons to bind are MouseMiddle, Mouse4, and Mouse5"
    check "mouse button refused in a config" $
        parseConfig "recenter = MouseLeft\n" == Left "line 1: the mouse buttons to bind are MouseMiddle, Mouse4, and Mouse5"
    check "empty binding in a list" $ parseBindings "F12, " == Left "empty key binding"
    check "unknown key reported" $ parseConfig "recenter = F99\n" == Left "line 1: unknown key \"F99\""
    check "unknown setting reported" $ parseConfig "\nfoo = F1\n" == Left "line 2: unknown setting \"foo\""

    -- Settings for smoothing and curves.
    check "curve and smoothing settings" $
        fmap response (parseConfig "yaw = 10:10 45:120\nsmoothing = off\n")
            == Right ((response defaultConfig){smoothing = NoSmoothing, yawCurve = lookBehind})
    let smoothingOf = fmap (smoothing . response) . parseConfig
        closeSmoothing (Smoothing a b) (Smoothing c d) = closeTo a c && closeTo b d
        closeSmoothing a b = a == b
    check "default config round-trips" $ parseConfig (renderConfig defaultConfig) == Right defaultConfig
    check "smoothing endpoints" $
        closeSmoothing (tuned 0 0) (Smoothing 20 0.001) && closeSmoothing (tuned 100 100) (Smoothing 0.2 1)
    checkShowing "default smoothing" (show (smoothing (response defaultConfig))) $ case smoothing (response defaultConfig) of
        Smoothing mc b -> abs (mc - 0.317) < 0.001 && abs (b - 0.0224) < 0.0001
        NoSmoothing -> False
    check "steadiness and responsiveness" $ smoothingOf "steadiness = 100\nresponsiveness = 0" == Right (tuned 100 0)
    check "smoothing off whatever the order" $ smoothingOf "smoothing = off\nsteadiness = 80" == Right NoSmoothing
    check "smoothing back on" $ smoothingOf "smoothing = off\nsmoothing = on\nsteadiness = 80" == Right (tuned 80 45)
    check "steadiness out of range" $ parseConfig "\nsteadiness = 101" == Left "line 2: steadiness is a number from 0 to 100"
    check "responsiveness not a number" $ parseConfig "responsiveness = lots" == Left "line 1: responsiveness is a number from 0 to 100"
    check "old smoothing form rejected" $ parseConfig "smoothing = 1 0.05" == Left "line 1: smoothing is on or off"
    check "curve inputs must increase" $ parseConfig "pitch = 5:5 3:9" == Left "line 1: curve inputs must be positive and increasing"
    check "curve points must be numbers" $ parseConfig "z = 1:x" == Left "line 1: expected input:output, not \"1:x\""
    let curvesOf = fmap ((\r -> [yawCurve r, pitchCurve r, rollCurve r, xCurve r, yCurve r, zCurve r]) . response) . parseConfig
    check "responses with a curve" $
        curvesOf "yaw = 1:6, curve 40\npitch = 2:14 , curve 0\nroll = 1:4\nx = 1:3, curve 100\ny = 1:4,curve 5\n"
            == Right
                [ Expo (Multiplier 6 40 yawRange)
                , Expo (Multiplier 7 0 pitchRange)
                , Points [(1, 4)]
                , Expo (Multiplier 3 100 leanRange)
                , Expo (Multiplier 4 5 leanRange)
                , Points [(1, 2.5)]
                ]
    check "a curve by its older name, precision, reads the same" $
        curvesOf "yaw = 1:6, precision 40\nx = 1:3,precision 100\n" == curvesOf "yaw = 1:6, curve 40\nx = 1:3,curve 100\n"
    check "curve with several points" $
        parseConfig "yaw = 1:6 2:9, curve 40" == Left "line 1: curve needs one point above 0, like 1:6, curve 40"
    check "curve with no multiplier" $
        parseConfig "yaw = 1:0, curve 40" == Left "line 1: curve needs one point above 0, like 1:6, curve 40"
    check "curve out of range" $ parseConfig "yaw = 1:6, curve 101" == Left "line 1: curve is a number from 0 to 100"
    check "not a curve" $ parseConfig "yaw = 1:6, expo 40" == Left "line 1: expected curve and a number, not \"expo 40\""
    check "curve with no points" $ parseConfig "yaw = , curve 40" == Left "line 1: a curve needs at least one input:output point"
    check "two commas" $ parseConfig "yaw = 1:6, curve 4, 5" == Left "line 1: a response has at most one comma, before its curve"
    let withExpos =
            defaultConfig
                { response =
                    (response defaultConfig)
                        { yawCurve = Expo (Multiplier 6 40 yawRange)
                        , zCurve = Expo (Multiplier 2.5 12.5 leanRange)
                        , rollCurve = deadzone
                        }
                }
    check "curves round-trip" $ parseConfig (renderConfig withExpos) == Right withExpos
    check "curve written" $
        editSettings [("yaw", "1:6, curve 40")] "yaw = 1:6 # mine\n" == Right "yaw = 1:6, curve 40 # mine\n"
    check "curve described, by its newer name" $
        (describeChanges defaultConfig <$> parseConfig "yaw = 1:6, precision 40\n") == Right "Settings reloaded: yaw = 1:6, curve 40"

    let reloaded text = describeChanges defaultConfig <$> parseConfig text
    check "nothing changed" $ reloaded "# just a comment\n" == Right "Settings reloaded."
    checkShowing "changes described" (show (reloaded "pause = Ctrl+F9\nsteadiness = 80\nyaw = 10:10 45:120\n")) $
        reloaded "pause = Ctrl+F9\nsteadiness = 80\nyaw = 10:10 45:120\n"
            == Right "Settings reloaded: pause = Ctrl+F9, steadiness = 80, yaw = 10:10 45:120"
    check "button change described" $
        reloaded "recenter = F12, Stick button 2\n" == Right "Settings reloaded: recenter = F12, Stick button 2"
    check "smoothing turned off" $ reloaded "smoothing = off\nsteadiness = 80\n" == Right "Settings reloaded: smoothing = off"

    check "lean setting" $ fmap (lean . response) (parseConfig "lean = screen\n") == Right LeanWithScreen
    check "lean default" $ lean (response defaultConfig) == LeanWithView
    check "bad lean" $ parseConfig "lean = sideways" == Left "line 1: lean is view or screen"

    -- Whose NPClient games load.
    check "trackkat's NPClient by default" $ npclient defaultConfig == TrackKatNPClient
    check "npclient setting" $ fmap npclient (parseConfig "npclient = trackir\n") == Right TrackIRNPClient
    check "bad npclient" $ parseConfig "npclient = opentrack" == Left "line 1: npclient is trackkat or trackir"
    let withTrackIR = defaultConfig{npclient = TrackIRNPClient}
    check "npclient round-trips" $ parseConfig (renderConfig withTrackIR) == Right withTrackIR
    check "npclient rendered" $ "npclient = trackkat" `elem` lines (renderConfig defaultConfig)
    check "npclient change described" $
        describeChanges defaultConfig withTrackIR == "Settings reloaded: npclient = trackir"
    check "npclient editable" $ editSettings [("npclient", "trackir")] "npclient = trackkat\n" == Right "npclient = trackir\n"

    -- The window's head view, as last clicked to.
    check "head view as games see it by default" $ headView defaultConfig == VirtualView
    check "head view setting" $
        map (fmap headView . parseConfig . ("head view = " ++)) ["virtual", "physical", "camera"]
            == map Right [VirtualView, PhysicalView, CameraView]
    check "bad head view" $ parseConfig "head view = side" == Left "line 1: head view is virtual, physical, or camera"
    let withCamera = defaultConfig{headView = CameraView}
    check "head view round-trips" $ parseConfig (renderConfig withCamera) == Right withCamera
    check "head view change not described" $
        describeChanges defaultConfig withCamera == "Settings reloaded."
    check "head view editable" $
        editSettings [("head view", "physical")] "head view = virtual\n" == Right "head view = physical\n"

    check "trim unbound by default" $ trimBindings defaultConfig == []
    check "trim bindings" $
        fmap trimBindings (parseConfig "trim = Ctrl+F12, Stick button 3\n") == Right (map tap [Key [Ctrl] "F12", JoystickButton (Joystick "Stick" 1) 3])
    check "empty trim" $ parseConfig "trim =\n" == Right defaultConfig && parseConfig "trim =   # none\n" == Right defaultConfig
    check "empty trim rendered" $ "trim =" `elem` lines (renderConfig defaultConfig)
    let withTrim = defaultConfig{trimBindings = [tap (Key [] "F11")]}
    check "trim round-trips" $ parseConfig (renderConfig withTrim) == Right withTrim
    check "trim change described" $ describeChanges defaultConfig withTrim == "Settings reloaded: trim = F11"

    -- Ways of pressing a key or button, written after it.
    let stick = JoystickButton (Joystick "Stick" 1) 4
    check "gestures" $
        parseBindings "F12 hold, Stick button 4 Double,Ctrl + F9  tap, Stick button 4"
            == Right [Binding (Key [] "F12") Hold, Binding stick Double, tap (Key [Ctrl] "F9"), tap stick]
    check "gestures rendered, a tap unsaid" $
        renderBindings [Binding (Key [] "F12") Hold, Binding stick Double, tap stick] == "F12 hold, Stick button 4 double, Stick button 4"
    check "a gesture with no key" $ parseBinding "hold" == Left "unknown key \"hold\""
    check "a gesture is the last word" $ parseBinding "hold F12" == Left "unknown key \"hold F12\""
    check "a joystick named for a gesture" $ parseBinding "Hold button 2 hold" == Right (Binding (JoystickButton (Joystick "Hold" 1) 2) Hold)

    -- Aiming: its keys, and its own curves and smoothing.
    check "aim unbound by default" $ aimBindings defaultConfig == []
    let aiming = aim . response
        aimed = parseConfig "aim = Stick button 4 hold\naim yaw = 1:2\naim pitch = 1:1, curve 50\naim steadiness = 80\nsteadiness = 70\n"
    check "aim settings" $
        fmap aimBindings aimed == Right [Binding stick Hold]
            && fmap aiming aimed == Right (Aim (tuned 80 45) (Points [(1, 2)]) (Expo (Multiplier 1 50 pitchRange)) (Points [(1, 1.5)]))
            && fmap (smoothing . response) aimed == Right (tuned 70 45)
    -- Marking: its keys.
    check "mark unbound by default" $ markBindings defaultConfig == []
    check "mark bindings" $
        fmap markBindings (parseConfig "mark = F8, Stick button 4 double\n") == Right [tap (Key [] "F8"), Binding stick Double]
    check "mark change described" $
        describeChanges defaultConfig defaultConfig{markBindings = [tap (Key [] "F8")]} == "Settings reloaded: mark = F8"
    check "aim's smoothing off, the view's left on" $
        fmap (\c -> (aimSmoothing (aiming c), smoothing (response c))) (parseConfig "aim smoothing = off\n")
            == Right (NoSmoothing, smoothing (response defaultConfig))
    check "bad aim smoothing" $ parseConfig "aim smoothing = some" == Left "line 1: aim smoothing is on or off"
    check "aim steadiness out of range" $ parseConfig "aim steadiness = 101" == Left "line 1: aim steadiness is a number from 0 to 100"
    check "aim change described" $
        (describeChanges defaultConfig <$> aimed)
            == Right "Settings reloaded: aim = Stick button 4 hold, steadiness = 70, aim yaw = 1:2, aim pitch = 1:1, curve 50, aim steadiness = 80"
    check "a file from before aiming says the defaults" $
        parseConfig (unlines (filter (not . isPrefixOf "aim") (lines (renderConfig defaultConfig)))) == Right defaultConfig
    check "aim edited" $ editSettings [("aim", "F8 hold")] "aim = # none\n" == Right "aim = F8 hold # none\n"

    -- Drift is no longer a setting, but files that set it still load,
    -- with a note, and editing leaves the line alone.
    let oldFile = "lean = view\ndrift = on\n"
    check "drift ignored" $ parseConfig oldFile == Right defaultConfig && parseConfig "drift = maybe" == Right defaultConfig
    check "drift noted" $ retiredSettings oldFile == ["line 2: drift is no longer a setting; ignoring it."]
    check "no notes" $ retiredSettings (renderConfig defaultConfig) == []
    check "drift not rendered" $ "drift" `notElem` settingNames
    check "drift left alone" $
        editSettings (settingValues oldFile ++ [("lean", "screen")]) oldFile == Right "lean = screen\ndrift = on\n"
    check "drift not editable" $ isLeft (editSettings [("drift", "off")] oldFile)

    -- Editing the settings file from trackkat's window.
    let userFile = "# mine\npause=Pause # TrackIR style\nsmoothing = off\nsteadiness = 70\npause = F9\n"
    checkShowing "values as written" (show (settingValues userFile)) $
        take 8 (settingValues userFile)
            == [("recenter", "F12"), ("pause", "F9"), ("trim", ""), ("aim", ""), ("mark", ""), ("smoothing", "off"), ("steadiness", "70"), ("responsiveness", "45")]
    check "names in file order" $ settingNames == map fst (settingValues "")
    check "unchanged values leave the file alone" $ editSettings (settingValues userFile) userFile == Right userFile
    checkShowing "edit keeps comments" (show (editSettings [("pause", "Ctrl+F9"), ("yaw", "1:5")] userFile)) $
        editSettings [("pause", "Ctrl+F9"), ("yaw", "1:5")] userFile
            == Right "# mine\npause=Pause # TrackIR style\nsmoothing = off\nsteadiness = 70\npause = Ctrl+F9\nyaw = 1:5\n"
    check "edit keeps a line's comment" $
        editSettings [("pause", "F8")] "pause=Pause # TrackIR style" == Right "pause = F8 # TrackIR style\n"
    check "edit names a bad value" $
        editSettings [("steadiness", "101")] userFile == Left "steadiness: steadiness is a number from 0 to 100"
    check "edit reports the rest of the file" $
        editSettings [("pause", "F8")] "bogus\n" == Left "line 1: expected name = value"
    check "trim edited" $ editSettings [("trim", "Ctrl+F12")] userFile == Right (userFile ++ "trim = Ctrl+F12\n")
    check "trim unbound again" $ editSettings [("trim", "")] "trim = F1 # mine\n" == Right "trim = # mine\n"
    -- The clip's shape.
    check "clip default" $ clipDistances (clip defaultConfig) == (70, 110, 110)
    check "clip default rendered" $ "clip = 70 110 110" `elem` lines (renderConfig defaultConfig)
    let bent = clipShape 70 112 108
    check "clip parsed, with commas or spaces" $
        all (\t -> fmap clip (parseConfig t) == maybe (Left "") Right bent) ["clip = 70 112 108\n", "clip = 70,112,108\n", "clip = 70, 112 ,108\n"]
    check "clip round-trips" $ case bent of
        Just b -> parseConfig (renderConfig defaultConfig{clip = b}) == Right defaultConfig{clip = b}
        Nothing -> False
    check "clip not a triangle" $ parseConfig "clip = 70 10 10\n" == Left "line 1: the clip's three distances don't make a triangle"
    check "clip needs three distances" $ isLeft (parseConfig "clip = 70 110\n") && isLeft (parseConfig "clip = 70 -110 -110\n")
    check "clip change described" $ (describeChanges defaultConfig <$> parseConfig "clip = 70 112 108\n") == Right "Settings reloaded: clip = 70 112 108"
    check "clip edited" $ editSettings [("clip", "70 112 108")] userFile == Right (userFile ++ "clip = 70 112 108\n")
    check "clip edit checked" $ isLeft (editSettings [("clip", "1 2 30")] userFile)
    check "binding from codes" $ keyFromCodes 0x6 0x78 == Just (Key [Ctrl, Shift] "F9")
    check "binding from an unnamed key" $ keyFromCodes 0 0xBA == Nothing
    tree exampleFile
    tree properties

-- | The example settings file in the repository is the one trackkat writes
-- when there is none. With TRACKKAT_ACCEPT set, it is written afresh.
exampleFile :: TestTree
exampleFile =
    testCase (example ++ " is the default settings file; TRACKKAT_ACCEPT=1 cabal test rewrites it") $
        lookupEnv "TRACKKAT_ACCEPT" >>= \case
            Just _ -> writeFile example (renderConfig defaultConfig)
            Nothing -> do
                inRepository <- try (readFile' example)
                assertBool "" (either (\e -> const False (e :: IOException)) (== renderConfig defaultConfig) inRepository)
  where
    example = "trackkat.conf.example"

-- | Settings here are those a settings file can say (see 'genConfig').
properties :: TestTree
properties =
    testGroup
        "properties"
        [ testProperty "settings are read back as written" $
            \(Sayable c) ->
                parseConfig (renderConfig c) === Right c
        , testProperty "written settings set each setting once, in the order of their names, none of them retired" $
            \(Sayable c) ->
                map fst (settingValues (renderConfig c)) === settingNames .&&. retiredSettings (renderConfig c) === []
        , testProperty "nothing is said to have changed just when nothing but the head view has" $
            \(Sayable a) (Sayable b) ->
                let same = a{headView = headView b} == b
                 in describeChanges a a === "Settings reloaded." .&&. (describeChanges a b == "Settings reloaded.") === same
        , testProperty "bindings are read back as written" $
            \bs ->
                parseBindings (renderBindings bs) === Right bs
        , testProperty "modifiers written in any order are the same binding" $
            \(KeyBinding b) ->
                let parts = splitOn '+' (renderKeyOrButton b)
                 in forAnyOrder (init parts) $ \mods -> parseBinding (intercalate " + " (mods ++ mods ++ [last parts])) === Right (tap b)
        , testProperty "a key's binding is the one for its codes" $
            \(KeyBinding b) -> case b of
                Key mods k -> keyFromCodes (modifierMask mods) (virtualKey k) === Just b
                JoystickButton{} -> property True
        , testProperty "a file edited to the values it has is left alone" $
            \(Edited file) ->
                let text = fileText file in editSettings (settingValues text) text === Right text
        , testProperty "a file edited to other settings says those settings, its other lines and comments kept" $
            \(Edited file) (Sayable c) ->
                let text = fileText file
                 in case editSettings (settingValues (renderConfig c)) text of
                        Left err -> counterexample err False
                        Right edited ->
                            counterexample edited $
                                conjoin
                                    [ parseConfig edited === Right c
                                    , take (length (lines text)) (map comment (lines edited)) === map comment (lines text)
                                    , take (length (lines text)) (map named (lines edited)) === map named (lines text)
                                    , property (all (null . comment) (drop (length (lines text)) (lines edited)))
                                    ]
        , testProperty "a file edited to other settings but for one the window doesn't show, as mark, keeps what it says of that one" $
            \(Edited file) (Sayable c) (Unshown name) ->
                let text = fileText file
                    shown = [(n, v) | (n, v) <- settingValues (renderConfig c), n /= name]
                 in case editSettings shown text of
                        Left err -> counterexample err False
                        Right edited ->
                            counterexample edited $
                                filter ((== Just name) . named) (lines edited) === filter ((== Just name) . named) (lines text)
                                    .&&. lookup name (settingValues edited) === lookup name (settingValues text)
        , testProperty "a file edited twice to the same settings is edited once" $
            \(Edited file) (Sayable c) ->
                let values = settingValues (renderConfig c)
                    once' = editSettings values (fileText file)
                 in (once' >>= editSettings values) === once'
        ]
  where
    comment = dropWhile (/= '#')
    -- The name a line sets, if it sets one.
    named line = case break (== '=') (takeWhile (/= '#') line) of
        (name, '=' : _) -> Just (unwords (words name))
        _ -> Nothing
    splitOn c str = case break (== c) str of
        (a, []) -> [a]
        (a, _ : rest) -> a : splitOn c rest

-- | A line of a settings file a user has edited: a comment or nothing, or
-- a setting, as a settings file writes it, perhaps with a comment after.
data Line = Remark String | Setting String String String
    deriving (Eq, Show)

fileText :: [Line] -> String
fileText = unlines . map line
  where
    line (Remark text) = text
    line (Setting name value remark) = name ++ " = " ++ value ++ remark

-- | Some of the settings of some settings, in any order and some of them
-- more than once, among comments; simpler of fewer lines.
newtype Edited = Edited [Line]
    deriving (Show)

instance Arbitrary Edited where
    arbitrary = do
        c <- genConfig
        let settings = [Setting name value | (name, value) <- settingValues (renderConfig c)]
        Edited <$> listOf (oneof [Remark <$> elements ["", "# mine", "#", "  # as Zoë likes it"], elements settings <*> elements ["", " # mine", "   #"]])
    shrink (Edited file) = Edited <$> shrinkList (const []) file

-- | A setting trackkat's window doesn't show, by name; simplest marking's
-- keys.
newtype Unshown = Unshown String
    deriving (Show)

instance Arbitrary Unshown where
    arbitrary = Unshown <$> elements unshown
    shrink (Unshown name) = Unshown <$> earlier unshown name

-- | As field_names in cbits/gui.c leaves them out.
unshown :: [String]
unshown = ["mark", "aim yaw", "aim pitch", "aim roll", "aim smoothing", "aim steadiness", "aim responsiveness", "clip"]

-- | A key or mouse button with modifiers.
newtype KeyBinding = KeyBinding KeyOrButton
    deriving (Show)

-- | A key or button bound as a file binds one with no more said.
tap :: KeyOrButton -> Binding
tap i = Binding i Tap

instance Arbitrary KeyBinding where
    arbitrary = KeyBinding <$> arbitrary `suchThat` isKey
      where
        isKey Key{} = True
        isKey JoystickButton{} = False
    shrink (KeyBinding b) = KeyBinding <$> shrink b
