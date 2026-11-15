-- | A session of head tracking: what it promises of pausing, trimming,
-- recentering, the settings and the log, whatever happens to it. See Note
-- [What a session promises] in "TrackKat.Session".
module Test.Session (tests) where

import Data.List (mapAccumL)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck

import Test.Gen
import Test.Instances ()
import TrackKat.Calibration
import TrackKat.Config
import TrackKat.Controls
import TrackKat.Display
import TrackKat.GamePose
import TrackKat.Linear
import TrackKat.Log
import TrackKat.Pose
import TrackKat.Response
import TrackKat.Session
import TrackKat.Synth (rotYXZ, squareStripes)
import TrackKat.Tracker (Outcome (..), trackerCalibration)
import TrackKat.World (clockAt)

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
        , testProperty "pausing and resuming tell the window and the log, and nothing else does" $
            \(Moves moves) ->
                conjoin
                    [ counterexample (show event) $
                        [o | ShowOutput o <- effects] === case (sSending before, sSending after) of
                            (Following, Holding _) -> [Paused]
                            (Holding _, Following) -> [Live]
                            _ -> []
                    | (before, event, after, effects) <- trace moves
                    ]
        , testProperty "recentering clears the trim, telling the window and the log if there was one" $
            \(Moves moves) (Recentering recentering) ->
                let (before, _, after, effects) = last (trace (moves ++ [recentering]))
                 in sTrim after === NoTrim
                        .&&. [t | ShowTrim t <- effects] === [NoTrim | sTrim before /= NoTrim]
                        .&&. userLines effects === ["Trim cleared." | sTrim before /= NoTrim]
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
        , testProperty "whatever changes what the window says of the camera, the output, the trim or the calibration is logged for the user" $
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
        , testProperty "a hotkey held or pressed twice between looks acts once, and games asking to recenter count just when they have since the last look" $
            \(Moves moves) (Held keys) game ->
                let before = reached moves
                    recenters = sRecenters before + (if game == GameAsked then 1 else 0)
                    asked =
                        [RecenterAsked | RecenterKey `elem` keys || game == GameAsked]
                            ++ [PauseAsked | PauseKey `elem` keys]
                            ++ [TrimAsked | TrimKey `elem` keys]
                    (polled, effects) = step (ControlsPolled keys recenters) before
                    (one, expected) = steps asked before
                 in map brief effects === map brief expected
                        .&&. unchanged one polled
                        .&&. sRecenters polled === recenters
        , testProperty "what games asked while the camera was away is forgotten when it is found" $
            \(Moves moves) (More more) ->
                let before = reached moves
                    recenters = sRecenters before + more
                    (found, _) = step (CameraConnected recenters) before
                    (after, effects) = step (ControlsPolled [] recenters) found
                 in map brief effects === [] .&&. unchanged found after
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

-- | That two sessions send games the same, trim the same, and track with the
-- same calibration.
unchanged :: Session -> Session -> Property
unchanged a b =
    conjoin
        [ sSending a === sSending b
        , sTrim a === sTrim b
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
    | -- | the hotkeys pressed, and whether a game asked to recenter
      Keys [Hotkey] GameAsked
    | -- | asked for in the window
      Ask Asked
    | -- | the settings file saved
      Save Config
    | Corrupt
    deriving (Eq, Show)

data GameAsked = GameAsked | NoGameAsked
    deriving (Eq, Show)

data Asked = ToRecenter | ToPause | ToTrim | ToCalibrate | ToClear
    deriving (Eq, Show)

-- | The events moves make, frames 120 a second.
events :: [Move] -> [Event]
events = snd . mapAccumL event (0 :: Int, 0 :: Int)
  where
    event (frames, recenters) move = case move of
        Seek -> ((frames, recenters), CameraSought)
        Connect -> ((frames, recenters), CameraConnected recenters)
        Stop why -> ((frames, recenters), CameraStopped why)
        See y p -> ((frames + 1, recenters), FrameArrived (at frames) (clipAt (Pose (rotYXZ y p 0) (V3 0 0 700))))
        Dark -> ((frames + 1, recenters), FrameArrived (at frames) [])
        Glint -> ((frames + 1, recenters), FrameArrived (at frames) (squareStripes (300, 200)))
        Keys keys game ->
            let recenters' = recenters + (if game == GameAsked then 1 else 0)
             in ((frames, recenters'), ControlsPolled keys recenters')
        Ask ToRecenter -> ((frames, recenters), RecenterAsked)
        Ask ToPause -> ((frames, recenters), PauseAsked)
        Ask ToTrim -> ((frames, recenters), TrimAsked)
        Ask ToCalibrate -> ((frames, recenters), CalibrateAsked)
        Ask ToClear -> ((frames, recenters), ClearCalibrationAsked)
        Save config -> ((frames, recenters), SettingsSaved "text" config)
        Corrupt -> ((frames, recenters), PacketCorrupt "too short")
    at frames = clockAt (fromIntegral frames / 120)
    clipAt pose = concatMap (squareStripes . project defaultIntrinsics pose) [mLeft model, mRight model, mTop model]
    model = clipModel (clip defaultConfig)

-- | A session from its start through moves: before and after each event,
-- and what the event asked for.
trace :: [Move] -> [(Session, Event, Session, [Effect])]
trace moves = snd (mapAccumL (\s e -> let (s', effects) = step e s in (s', (s, e, s', effects))) start (events moves))

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
pauses (ControlsPolled keys _) = PauseKey `elem` keys
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
-- less, fewer keys, and settings nearer the defaults.
instance Arbitrary Move where
    arbitrary =
        frequency
            [ (6, See <$> (fromIntegral <$> chooseInt (-8, 8)) <*> (fromIntegral <$> chooseInt (-5, 5)))
            , (1, pure Dark)
            , (1, pure Glint)
            , (3, Keys <$> sublistOf [RecenterKey, PauseKey, TrimKey] <*> elements [NoGameAsked, NoGameAsked, GameAsked])
            , (5, Ask <$> elements [ToRecenter, ToPause, ToPause, ToTrim, ToTrim, ToCalibrate, ToClear])
            , (1, Save <$> genConfig)
            , (1, elements [Seek, Connect, Stop "The camera stopped.", Corrupt])
            ]
    shrink = shrinkTo Dark simpler
      where
        simpler move = case move of
            See y p -> [See 0 0 | (y, p) /= (0, 0)]
            Keys keys game -> [Keys [] NoGameAsked] ++ [Keys keys' game | keys' <- shrinkList (const []) keys] ++ [Keys keys NoGameAsked | game == GameAsked]
            Save config -> Save <$> shrinkConfig config
            _ -> []

-- | A move that recenters: asked for in the window, by the hotkey, or by a
-- game.
newtype Recentering = Recentering Move
    deriving (Show)

instance Arbitrary Recentering where
    arbitrary = Recentering <$> elements recenterings
    shrink (Recentering move) = Recentering <$> earlier recenterings move

recenterings :: [Move]
recenterings = [Ask ToRecenter, Keys [RecenterKey] NoGameAsked, Keys [] GameAsked]

-- | The hotkeys found pressed at a look, some of them twice, in any order;
-- simpler of fewer.
newtype Held = Held [Hotkey]
    deriving (Show)

instance Arbitrary Held where
    arbitrary = Held <$> (sublistOf [RecenterKey, PauseKey, TrimKey, RecenterKey, PauseKey, TrimKey] >>= shuffle)
    shrink (Held keys) = Held <$> shrinkList (const []) keys

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
