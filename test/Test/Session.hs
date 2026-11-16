-- | A session of head tracking: what it promises of pausing, trimming,
-- aiming, recentering, the keys and buttons, the settings and the log,
-- whatever happens to it. See Note [What a session promises] in
-- "TrackKat.Session".
module Test.Session (tests) where

import Data.List (mapAccumL)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Test.Tasty.QuickCheck

import Test.Gen
import Test.Instances ()
import TrackKat.Calibration
import TrackKat.Config
import TrackKat.Controls
import TrackKat.Display
import TrackKat.GamePose
import TrackKat.Gesture (Edge (..), holdAfter)
import TrackKat.Linear
import TrackKat.Log
import TrackKat.Pose
import TrackKat.Response
import TrackKat.Session
import TrackKat.Synth (rotYXZ, squareStripes)
import TrackKat.Tracker (Outcome (..), trackerCalibration)
import TrackKat.World (clockAt, inSeconds)

tests :: TestTree
tests =
    testGroup
        "Session"
        [ testCase "what the log says of a session" $
            userLines
                ( effectsOf
                    [ Seek
                    , Connect
                    , See 0 0
                    , Ask ToPause
                    , See 5 0
                    , Ask ToPause
                    , See 5 0
                    , Ask ToTrim
                    , Ask ToRecenter
                    , See 6 0
                    , Ask ToClear
                    , Stop "The camera stopped."
                    ]
                )
                @?= [ "Tracking."
                    , "Recentered."
                    , "Paused: games see your head held still."
                    , "Resumed."
                    , "Trimmed: forward is now yaw +3 pitch -0 roll +0, x +3 y +0 z -0 mm."
                    , "Trim cleared."
                    , "Recentered."
                    , "Calibration cleared: trackkat learns it afresh as you turn your head."
                    , "The camera stopped."
                    , "Trying the camera again in 3 seconds."
                    ]
        , testCase "what the log says of aiming, by the window's switch and by a button held down" $
            userLines
                ( effectsOf
                    [ Connect
                    , See 0 0
                    , Ask ToAim
                    , See 1 0
                    , Ask ToAim
                    , Save defaultConfig{aimBindings = [Binding stick Hold]}
                    , Look [(WentDown, stick)] NoGameAsked
                    , See 1 0
                    , Wait
                    , See 2 0
                    , Look [(CameUp, stick)] NoGameAsked
                    , Ask ToAim
                    , Ask ToRecenter
                    ]
                )
                @?= [ "Tracking."
                    , "Recentered."
                    , "Aiming: sensitivity reduced for finer control."
                    , "Aiming ended."
                    , "Settings reloaded: aim = Stick button 2 hold"
                    , "Aiming: sensitivity reduced for finer control."
                    , "Aiming ended."
                    , "Aiming: sensitivity reduced for finer control."
                    , "Aiming ended."
                    ]
        , testCase "aiming, a turn of the head turns the view a quarter as far, as the default curves have it" $
            let turned aiming = yaw (sLatest (reached ([Connect, See 0 0] ++ [Ask ToAim | aiming == Aiming] ++ [See 0 0] ++ replicate 240 (See 4 0))))
             in assertBool (show (turned NotAiming, turned Aiming)) $
                    abs (turned Aiming) > 5 && abs (turned NotAiming / turned Aiming - 4) < 0.01
        , testProperty "every frame sends games one pose and shows the window one frame, and nothing else sends or shows one" $
            \(Moves moves) ->
                conjoin
                    [ counterexample (show event) $
                        (length [() | Publish _ <- effects], length [() | ShowFrame _ _ <- effects], length [() | ShowBlobs _ _ <- effects])
                            === (if isFrame event then (1, 1, 1) else (0, 0, 0))
                    | (_, event, _, effects) <- trace moves
                    ]
        , testProperty "paused, games are sent the pose held, the view as it was then, whatever happens until the user resumes" $
            \(Moves moves) ->
                conjoin
                    [ counterexample (show event) $ case (sSending before, sSending after) of
                        (Following, Holding held) -> held === sLatest after .&&. property (pauses event)
                        (Holding held, Holding kept) ->
                            kept === held
                                .&&. [p | Publish p <- effects] === [toGamePose held | isFrame event]
                                .&&. [view | ShowFrame _ view <- effects] === [held | isFrame event]
                        (Holding _, Following) -> property (pauses event)
                        (Following, Following) -> [p | Publish p <- effects] === [toGamePose (sLatest after) | isFrame event]
                    | (before, event, after, effects) <- trace moves
                    ]
        , testProperty "pausing shows the window the pose held; it and resuming tell the window and the log, and nothing else does" $
            \(Moves moves) ->
                conjoin
                    [ counterexample (show event) $
                        [o | ShowOutput o <- effects] === case (sSending before, sSending after) of
                            (Following, Holding held) -> [Paused held]
                            (Holding _, Following) -> [Live]
                            _ -> []
                    | (before, event, after, effects) <- trace moves
                    ]
        , testProperty "aiming beginning and ending tell the window and the log, and nothing else does" $
            \(Moves moves) ->
                conjoin
                    [ counterexample (show event) $ [a | ShowAim a <- effects] === [sAim after | sAim after /= sAim before]
                    | (before, event, after, effects) <- trace moves
                    ]
        , testProperty "recentering clears the trim and ends aiming, telling the window and the log if there was either" $
            \(Moves moves) (Recentering recentering) ->
                let (before, _, after, effects) = last (trace (moves ++ [recentering]))
                 in sTrim after === NoTrim
                        .&&. sAim after === NotAiming
                        .&&. [t | ShowTrim t <- effects] === [NoTrim | sTrim before /= NoTrim]
                        .&&. [a | ShowAim a <- effects] === [NotAiming | sAim before == Aiming]
                        .&&. userLines effects === ["Trim cleared." | sTrim before /= NoTrim] ++ ["Aiming ended." | sAim before == Aiming]
        , testProperty "aiming is asked of shaping from when it begins until it ends, and nothing but its switch, its keys and recentering changes it" $
            \(Moves moves) ->
                conjoin
                    [ counterexample (show event) $ sAim after === sAim before
                    | (before, event, after, _) <- trace moves
                    , not (aims event)
                    ]
        , testProperty "trimming takes the pose games are sent as forward, and doesn't move a pose held" $
            \(Moves moves) ->
                let (before, _, after, effects) = last (trace (moves ++ [Ask ToTrim]))
                    view = case sSending before of
                        Holding held -> held
                        Following -> sLatest before
                 in sTrim after === trimTo view
                        .&&. sSending after === sSending before
                        .&&. [t | ShowTrim t <- effects] === [trimTo view]
        , testProperty "with the clip out of sight, recentering centres the view at once" $
            \(Moves moves) ->
                let (before, _, after, _) = last (trace (moves ++ [Ask ToRecenter]))
                 in case sOutcome before of
                        Just Tracked{} -> sLatest after === sLatest before
                        _ -> sLatest after === centred
        , testProperty "whatever changes what the window says of the camera, the output, the trim, aiming or the calibration is logged for the user" $
            \(Moves moves) ->
                conjoin
                    [ counterexample (show event) $ counterexample (show effects) $ not (null (userLines effects))
                    | (_, event, _, effects) <- trace moves
                    , any logged effects
                    ]
        , testProperty "a settings file saved with nothing changed changes nothing and logs nothing: the hotkeys are bound again and the window shown its text" $
            \(Moves moves) view ->
                let before = reached moves
                    same = (sSettings before){headView = view}
                    (after, effects) = step (SettingsSaved "text" same) before
                 in map brief effects === ["BindHotkeys", "ShowSettings \"text\""]
                        .&&. sSettings after === same
                        .&&. unchanged before after
        , testProperty "settings saved are the session's settings, and games and Windows are told of just what changed for them" $
            \(Moves moves) (Sayable new) ->
                let before = reached moves
                    old = sSettings before
                    (after, effects) = step (SettingsSaved "text" new) before
                 in sSettings after === new
                        .&&. [n | PointGames n <- effects] === [npclient new | npclient new /= npclient old]
                        .&&. [w | SetStartup w <- effects] === [startup new | startup new /= startup old]
                        .&&. [c | BindHotkeys c <- effects] === [new]
                        .&&. userLines effects
                            === [describeChanges old new | new /= old{headView = headView new}]
                                ++ ["Tracking the clip's new shape; recentering." | clip new /= clip old]
                                ++ ["Aiming ended." | sAim after /= sAim before]
        , testProperty "games asking to recenter count just when they have since the last look" $
            \(Moves moves) game ->
                let before = reached moves
                    recenters = sRecenters before + (if game == GameAsked then 1 else 0)
                    (polled, effects) = step (ControlsPolled (clockAt 0) [] recenters) before
                    (one, expected) = steps [RecenterAsked | game == GameAsked] before
                 in map brief effects === map brief expected
                        .&&. unchanged one polled
                        .&&. sRecenters polled === recenters
        , testProperty "what games asked while the camera was away is forgotten when it is found" $
            \(Moves moves) (More more) ->
                let before = reached moves
                    recenters = sRecenters before + more
                    (found, _) = step (CameraConnected recenters) before
                    (after, effects) = step (ControlsPolled (clockAt 0) [] recenters) found
                 in map brief effects === [] .&&. unchanged found after
        , testProperty "a key or button with only taps bound does all it is bound to the moment it goes down, and nothing as it comes up" $
            \(Moves moves) (OnBench others) (Benched key) ->
                let bound = keeping (\b -> boundKey b /= key || boundGesture b == Tap) others
                    let_go = ControlsPolled (clockAt 1000) [KeyEdge (clockAt 1000) CameUp key] . sRecenters
                    saved = fst (step (SettingsSaved "text" bound) (reached moves))
                    before = fst (step (let_go saved) saved)
                    hotkeys = [k | k <- [minBound .. maxBound], b <- hotkeyBindings bound k, boundKey b == key]
                    look way = step (ControlsPolled (clockAt 1000) [KeyEdge (clockAt 1000) way key] (sRecenters before))
                    (pressed, effects) = look WentDown before
                    (asked, expected) = steps (map hotkeyAsked hotkeys) before
                    (released, none) = look CameUp pressed
                 in map brief effects === map brief expected
                        .&&. unchanged asked pressed
                        .&&. map brief none === []
                        .&&. unchanged pressed released
        , testProperty "a key or button whose hold aims does so from when it has been down long enough until it comes up, or the settings bind it otherwise" $
            \(Moves moves) (OnBench others) (Benched key) (OnBench rebound) ->
                let unheld = keeping (\b -> boundKey b /= key || boundGesture b /= Hold) others
                    bound = unheld{aimBindings = Binding key Hold : aimBindings unheld}
                    held = reached (moves ++ [Save bound, Wait, Look [(WentDown, key)] NoGameAsked, Wait])
                    (resaved, _) = step (SettingsSaved "text" rebound) held
                    (released, _) = step (ControlsPolled (clockAt 1000) [KeyEdge (clockAt 1000) CameUp key] (sRecenters resaved)) resaved
                 in sAim held === Aiming
                        .&&. (if boundTo rebound key /= boundTo bound key then sAim resaved === NotAiming else property True)
                        .&&. sAim released === NotAiming
        , testProperty "a calibration file that is missing or can't be read starts the session from none, and one that can from what it says" $
            \(Found found) ->
                let (c, effects) = startingCalibration "trackkat.calibration" found
                    none = [noCalibration]
                 in c === defaultCalibration
                        .&&. case found of
                            Nothing -> userLines effects === none .&&. [shown | ShowCalibration shown <- effects] === none
                            Just (Left err) ->
                                userLines effects === ["trackkat.calibration: " ++ err ++ "; starting afresh."] ++ none
                                    .&&. [shown | ShowCalibration shown <- effects] === none
                            Just (Right _) -> length (userLines effects) === 2 .&&. length [() | ShowCalibration _ <- effects] === 1
        , testProperty "clearing the calibration tracks afresh from none, and saves none" $
            \(Moves moves) ->
                let (_, _, after, effects) = last (trace (moves ++ [Ask ToClear]))
                    none = renderCalibration defaultCalibration
                 in renderCalibration (trackerCalibration (sTracker after)) === none
                        .&&. [renderCalibration c | SaveCalibration c <- effects] === [none]
                        .&&. [c | ShowCalibration c <- effects] === [noCalibration]
        ]

-- | Whether an effect changes what the window says of the camera, the
-- output, the trim or the calibration. Looking for the camera is shown
-- without a line.
logged :: Effect -> Bool
logged effect = case effect of
    ShowCamera Searching -> False
    ShowCamera _ -> True
    ShowOutput _ -> True
    ShowTrim _ -> True
    ShowAim _ -> True
    ShowCalibration _ -> True
    ShowClip _ -> True
    _ -> False

-- | The lines effects log for the user.
userLines :: [Effect] -> [String]
userLines effects = [line | Log (ForUser line) <- effects]

-- | An effect in a few words, with what it carries where that is short.
brief :: Effect -> String
brief effect = case effect of
    BindHotkeys _ -> "BindHotkeys"
    SaveCalibration _ -> "SaveCalibration"
    ShowBlobs _ _ -> "ShowBlobs"
    other -> show other

-- | Settings with only those of their bindings that pass.
keeping :: (Binding -> Bool) -> Config -> Config
keeping passes c =
    c
        { recenterBindings = filter passes (recenterBindings c)
        , pauseBindings = filter passes (pauseBindings c)
        , trimBindings = filter passes (trimBindings c)
        , aimBindings = filter passes (aimBindings c)
        }

-- | What a hotkey asks for when its key is tapped.
hotkeyAsked :: Hotkey -> Event
hotkeyAsked RecenterKey = RecenterAsked
hotkeyAsked PauseKey = PauseAsked
hotkeyAsked TrimKey = TrimAsked
hotkeyAsked AimKey = AimAsked

-- | Whether an event can begin aiming or end it: its switch and its keys,
-- recentering, and settings that may stop watching a key held down.
aims :: Event -> Bool
aims event = case event of
    AimAsked -> True
    AimHeld -> True
    AimLetGo -> True
    RecenterAsked -> True
    SettingsSaved{} -> True
    _ -> False

-- | That two sessions send games the same, trim and aim the same, and track
-- with the same calibration.
unchanged :: Session -> Session -> Property
unchanged a b =
    conjoin
        [ sSending a === sSending b
        , sTrim a === sTrim b
        , sAim a === sAim b
        , sLatest a === sLatest b
        , renderCalibration (trackerCalibration (sTracker a)) === renderCalibration (trackerCalibration (sTracker b))
        ]

-- | Something that happens to a session, as a test says it: frames are
-- given their times, and games' requests their count, by 'events'.
data Move
    = -- | trackkat looks for the camera
      Seek
    | -- | the camera is found
      Connect
    | -- | the camera goes away, and why
      Stop String
    | -- | a frame showing the clip, the head turned by this yaw and pitch,
      -- in degrees
      See Double Double
    | -- | a frame showing nothing
      Dark
    | -- | a frame showing one light
      Glint
    | -- | the controls looked at: keys and buttons that went down or came
      -- up, in order, and whether a game asked to recenter
      Look [(Edge, KeyOrButton)] GameAsked
    | -- | longer than a key is down before it is held, and then the
      -- controls looked at
      Wait
    | -- | asked for in the window
      Ask Asked
    | -- | the settings file saved
      Save Config
    | Corrupt
    deriving (Eq, Show)

data GameAsked = GameAsked | NoGameAsked
    deriving (Eq, Show)

data Asked = ToRecenter | ToPause | ToTrim | ToAim | ToCalibrate | ToClear
    deriving (Eq, Show)

-- | The events moves make, frames 120 a second, but for the waits.
events :: [Move] -> [Event]
events = snd . mapAccumL event (0 :: Int, 0 :: Int, 0 :: Int)
  where
    event (frames, waits, recenters) move = case move of
        Seek -> (same, CameraSought)
        Connect -> (same, CameraConnected recenters)
        Stop why -> (same, CameraStopped why)
        See y p -> (next, FrameArrived now (clipAt (Pose (rotYXZ y p 0) (V3 0 0 700))))
        Dark -> (next, FrameArrived now [])
        Glint -> (next, FrameArrived now (squareStripes (300, 200)))
        Look edges game ->
            let recenters' = recenters + (if game == GameAsked then 1 else 0)
             in ((frames, waits, recenters'), ControlsPolled now [KeyEdge now way key | (way, key) <- edges] recenters')
        Wait -> ((frames, waits + 1, recenters), ControlsPolled (at frames (waits + 1)) [] recenters)
        Ask ToRecenter -> (same, RecenterAsked)
        Ask ToPause -> (same, PauseAsked)
        Ask ToTrim -> (same, TrimAsked)
        Ask ToAim -> (same, AimAsked)
        Ask ToCalibrate -> (same, CalibrateAsked)
        Ask ToClear -> (same, ClearCalibrationAsked)
        Save config -> (same, SettingsSaved "text" config)
        Corrupt -> (same, PacketCorrupt "too short")
      where
        same = (frames, waits, recenters)
        next = (frames + 1, waits, recenters)
        now = at frames waits
    at frames waits = clockAt (fromIntegral frames / 120 + fromIntegral waits * 4 / 3 * inSeconds holdAfter)
    clipAt pose = concatMap (squareStripes . project defaultIntrinsics pose) [mLeft model, mRight model, mTop model]
    model = clipModel (clip defaultConfig)

-- | A session from its start through moves: before and after each event,
-- and what the event asked for. A look at the controls is told as what it
-- asked for, one after another, from the session that has looked.
trace :: [Move] -> [(Session, Event, Session, [Effect])]
trace moves = concat (snd (mapAccumL through start (events moves)))
  where
    through s event = case event of
        ControlsPolled now edges recenters -> let (looked, asks) = controlsLooked now edges recenters s in mapAccumL one looked asks
        _ -> (: []) <$> one s event
    one s event = let (s', effects) = step event s in (s', (s, event, s', effects))

start :: Session
start = newSession defaultConfig defaultCalibration

-- | The session moves come to.
reached :: [Move] -> Session
reached moves = fst (steps (events moves) start)

effectsOf :: [Move] -> [Effect]
effectsOf moves = snd (steps (events moves) start)

isFrame :: Event -> Bool
isFrame FrameArrived{} = True
isFrame _ = False

-- | Whether an event can pause or resume: the window's button or the
-- hotkey.
pauses :: Event -> Bool
pauses PauseAsked = True
pauses _ = False

-- | A few moves: mostly frames, of a head turning a little way from one
-- to the next so that it can be followed, with all else among them.
-- Simpler of fewer moves, and simpler ones.
newtype Moves = Moves [Move]
    deriving (Show)

instance Arbitrary Moves where
    arbitrary = Moves <$> scale (`div` 3) (listOf arbitrary)
    shrink (Moves moves) = Moves <$> shrink moves

-- | Simplest a frame showing nothing; and simpler with a head turned
-- less, fewer keys going down or up, and settings nearer the defaults.
instance Arbitrary Move where
    arbitrary =
        frequency
            [ (6, See <$> (fromIntegral <$> chooseInt (-8, 8)) <*> (fromIntegral <$> chooseInt (-5, 5)))
            , (1, pure Dark)
            , (1, pure Glint)
            , (4, Look <$> scale (`div` 10) (listOf ((,) <$> elements [WentDown, CameUp] <*> elements bench)) <*> elements [NoGameAsked, NoGameAsked, GameAsked])
            , (2, pure Wait)
            , (5, Ask <$> elements [ToRecenter, ToPause, ToPause, ToTrim, ToTrim, ToAim, ToAim, ToCalibrate, ToClear])
            , (1, Save <$> genConfig)
            , (1, (\(OnBench config) -> Save config) <$> arbitrary)
            , (1, elements [Seek, Connect, Stop "The camera stopped.", Corrupt])
            ]
    shrink = shrinkTo Dark simpler
      where
        simpler move = case move of
            See y p -> [See 0 0 | (y, p) /= (0, 0)]
            Look edges game -> [Look [] NoGameAsked] ++ [Look edges' game | edges' <- shrinkList (const []) edges] ++ [Look edges NoGameAsked | game == GameAsked]
            Save config -> Save <$> shrinkConfig config
            _ -> []

-- | The keys and buttons the moves press: those the default settings bind,
-- another key, and a joystick's button.
bench :: [KeyOrButton]
bench = [Key [] "F12", Key [] "F9", Key [Ctrl] "F8", stick]

stick :: KeyOrButton
stick = JoystickButton (Joystick "Stick" 1) 2

-- | One of the keys and buttons the moves press; simplest the first.
newtype Benched = Benched KeyOrButton
    deriving (Show)

instance Arbitrary Benched where
    arbitrary = Benched <$> elements bench
    shrink (Benched key) = Benched <$> earlier bench key

-- | The default settings but for what is bound: to each hotkey, some of
-- the keys and buttons the moves press, pressed any of the ways. Simplest
-- with nothing bound, and simpler with fewer and simpler bindings.
newtype OnBench = OnBench Config
    deriving (Show)

instance Arbitrary OnBench where
    arbitrary = do
        let bindings = scale (`div` 8) (listOf (Binding <$> elements bench <*> elements [Tap, Tap, Hold, Double]))
        recenter <- bindings
        pause <- bindings
        trim <- bindings
        aiming <- bindings
        pure (OnBench defaultConfig{recenterBindings = recenter, pauseBindings = pause, trimBindings = trim, aimBindings = aiming})
    shrink (OnBench config) = [OnBench simpler | simpler <- shrinkConfig config, benched simpler]
      where
        benched c = all ((`elem` bench) . boundKey) (concatMap (hotkeyBindings c) [minBound .. maxBound])

-- | A move that recenters: asked for in the window, or by a game.
newtype Recentering = Recentering Move
    deriving (Show)

instance Arbitrary Recentering where
    arbitrary = Recentering <$> elements recenterings
    shrink (Recentering move) = Recentering <$> earlier recenterings move

recenterings :: [Move]
recenterings = [Ask ToRecenter, Look [] GameAsked]

instance Arbitrary GameAsked where
    arbitrary = elements [GameAsked, NoGameAsked]
    shrink = earlier [NoGameAsked, GameAsked]

-- | How many more times games have asked to recenter, up to five.
newtype More = More Int
    deriving (Show)

instance Arbitrary More where
    arbitrary = More <$> chooseInt (0, 5)
    shrink (More n) = More <$> shrinkFrom 0 n

-- | What is found of a calibration file: none, one that can't be read, or
-- what one says. Simplest none.
newtype Found = Found (Maybe (Either String Calibration))

instance Show Found where
    show (Found found) = show (fmap (fmap renderCalibration) found)

instance Arbitrary Found where
    arbitrary = Found <$> elements finds
    shrink (Found found) = Found <$> earlier finds found

finds :: [Maybe (Either String Calibration)]
finds = [Nothing, Just (Left "line 3: no pivot"), Just (Right defaultCalibration)]
