-- | A session of head tracking: the tracker, the settings, and what games
-- are sent, taken through what happens to trackkat, each event to the next
-- state and what it asks to be done. Nothing here does anything: trackkat
-- reads the camera, the hotkeys and its window, and does what is asked.
--
-- See Note [What a session promises].
module TrackKat.Session (
    -- * The session
    Session (..),
    Sending (..),
    newSession,
    startingCalibration,
    centred,

    -- * What happens to it
    Event (..),
    Effect (..),
    step,
    steps,
    controlsLooked,

    -- * In words
    status,
    noCalibration,
) where

import Text.Printf (printf)

import TrackKat.Calibration
import TrackKat.Config
import TrackKat.Controls (Hotkey (..), KeyEdge (..), boundTo)
import TrackKat.Display
import TrackKat.Frame (Blob, Stripe)
import TrackKat.GamePose
import TrackKat.Gesture (Recognizer, Signal (..), edge, forget, newRecognizer, tick)
import TrackKat.Linear (V3)
import TrackKat.Log (Line (..))
import TrackKat.Pose
import TrackKat.Recording (Did (..), resettled, trackerFor)
import TrackKat.Response
import TrackKat.Shape
import TrackKat.Tracker (
    Absence,
    BlobRole,
    Outcome (..),
    Request (..),
    Seen (..),
    Tracker,
    absence,
    describeLightForgotten,
    describeLightIgnored,
    describeLightMoved,
    describeMasked,
    describeSeen,
    describeWhy,
    noAbsence,
    request,
    trackerBlobs,
    trackerCalibration,
    trackerCentre,
    trackerLights,
    trackerModel,
    trackerShape,
 )
import qualified TrackKat.Tracker as Tracker
import TrackKat.World (Instant)

-- Note [What a session promises]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 'step' is all that changes a session, and what it asks for depends only
-- on the session and the event, the time included: so what trackkat does
-- is settled by the events that reach it, in their order. On top of that:
--
--  * Every frame sends games one pose. Paused, it is the pose held, whatever
--    the frame showed, until the user resumes; trimming doesn't move it.
--
--  * Recentering clears the trim, and ends aiming.
--
--  * Whatever changes what the window says of the camera, the output, the
--    trim, aiming or the calibration is logged for the user in the same
--    step. (The window is shown the search for a camera, each frame and
--    the settings without a line.)
--
--  * A settings file saved with nothing changed leaves the session as it
--    was and logs nothing.
--
--  * Aiming is on from when it is asked for until it is ended or the view
--    is recentered, whatever else happens: the view is aimed from the next
--    frame that shows the clip (see Note [Aiming] in "TrackKat.Response"),
--    and where shaping starts afresh, as on a calibration, from the first
--    frame after. Aiming that a key or button held down began ends when
--    the key comes up, or when the settings bind it otherwise.
--
--  * Marks are numbered from 1, in the order asked for, whatever else
--    happens.
--
--  * Whatever is done with the tracker is said, in the order done, for a
--    recording: replayed from how the session started, the recording
--    tracks what the session tracked (see "TrackKat.Recording"). Besides
--    that it is told the settings file as saved, the marks, corrupt
--    packets, and the camera coming and going.
--
-- "Test.Session" states these as properties.

-- | A session of head tracking, which outlives any one connection to the
-- camera.
data Session = Session
    { sTracker :: !Tracker
    , sSettings :: !Config
    -- ^ as the settings file last said
    , sShaper :: !Shaper
    , sOutcome :: !(Maybe Outcome)
    -- ^ what the last frame showed, if there has been one
    , sSending :: !Sending
    , sTrim :: !Trim
    -- ^ where forward is for the view, until the next recenter
    , sAim :: !Aiming
    -- ^ whether aiming is asked for
    , sGestures :: !(Recognizer KeyOrButton Hotkey)
    -- ^ the bound keys' and buttons' presses not yet settled
    , sLatest :: !HeadPose
    -- ^ the view as last shaped: what games are sent unless paused
    , sAbsence :: !Absence
    -- ^ how long the clip has been out of sight, on this connection to
    -- the camera
    , sRecenters :: !Int
    -- ^ how many times games had asked to recenter when last looked at
    , sMarks :: !Int
    -- ^ how many times the user has asked to mark the moment
    }

-- | What games are sent.
data Sending
    = -- | the view, as it moves
      Following
    | -- | this pose, held while paused
      Holding HeadPose
    deriving (Eq, Show)

-- | A session with the settings given and what was learnt in earlier ones.
newSession :: Config -> Calibration -> Session
newSession config c =
    Session
        { sTracker = trackerFor defaultIntrinsics config c
        , sSettings = config
        , sShaper = newShaper
        , sOutcome = Nothing
        , sSending = Following
        , sTrim = NoTrim
        , sAim = NotAiming
        , sGestures = newRecognizer
        , sLatest = centred
        , sAbsence = noAbsence
        , sRecenters = 0
        , sMarks = 0
        }

-- | The calibration to start a session from, and what to say of it, given
-- the calibration file's path and what reading it gave, if it was there.
startingCalibration :: FilePath -> Maybe (Either String Calibration) -> (Calibration, [Effect])
startingCalibration path found = case found of
    Just (Right c) ->
        (c, [say (describeLevel c), say (describePivot Nothing c), ShowCalibration (describeCalibration Nothing c Nothing)])
    Just (Left err) -> (defaultCalibration, say (path ++ ": " ++ err ++ "; starting afresh.") : none)
    Nothing -> (defaultCalibration, none)
  where
    none = [say noCalibration, ShowCalibration noCalibration]

-- | The head at its centre.
centred :: HeadPose
centred = HeadPose 0 0 0 0 0 0

-- | Something that happened to trackkat.
data Event
    = -- | it is about to look for the camera
      CameraSought
    | -- | the camera was found, games having asked to recenter this many
      -- times so far
      CameraConnected Int
    | -- | the camera went away, or wasn't found, and why
      CameraStopped String
    | -- | the controls were looked at at this time: the bound keys' and
      -- buttons' presses and releases since the last look, oldest first,
      -- and how many times games have asked to recenter so far
      ControlsPolled Instant [KeyEdge] Int
    | -- | a frame arrived at this time, in seconds
      FrameArrived Instant [Stripe]
    | -- | a packet from the camera was dropped as corrupt, and why
      PacketCorrupt String
    | RecenterAsked
    | PauseAsked
    | TrimAsked
    | -- | begin aiming, or end it
      AimAsked
    | -- | a key or button that aims while held went down for long enough
      AimHeld
    | -- | and came up
      AimLetGo
    | CalibrateAsked
    | ClearCalibrationAsked
    | -- | mark the moment, for whoever reads a recording of the session
      MarkAsked
    | -- | the settings file was saved: its text, and the settings it gives
      SettingsSaved String Config
    deriving (Show)

-- | Something for trackkat to do.
data Effect
    = Log Line
    | ShowCamera CameraState
    | ShowOutput Output
    | ShowTrim Trim
    | ShowAim Aiming
    | -- | what the window says of the calibration
      ShowCalibration String
    | -- | the clip's reflectors on the head, as 'learntClip' gives them
      ShowClip (Maybe (V3, V3, V3))
    | -- | what a frame showed, and the pose games were sent
      ShowFrame Sighting HeadPose
    | -- | a frame's blobs with what each was taken for, and the areas
      -- masked around still lights
      ShowBlobs [(Blob, BlobRole)] [((Double, Double), Double)]
    | -- | the settings file's text
      ShowSettings String
    | -- | send games this pose
      Publish GamePose
    | -- | save the calibration, in place of any still waiting to be saved
      SaveCalibration Calibration
    | -- | bind the hotkeys as these settings have them
      BindHotkeys Config
    | -- | tell games to load this NPClient
      PointGames NPClient
    | -- | have Windows start trackkat at login, or not
      SetStartup Startup
    | -- | say in a recording of the session, if there is one, what it did
      Record Did
    deriving (Show)

-- | Take a session through events, oldest first.
steps :: [Event] -> Session -> (Session, [Effect])
steps events s = foldl' (\(s', done) e -> (done ++) <$> step e s') (s, []) events

-- | Take a session through an event: the session after it, and what to do,
-- in order.
step :: Event -> Session -> (Session, [Effect])
step event s = case event of
    CameraSought -> (s, [ShowCamera Searching])
    CameraConnected recenters ->
        (s{sRecenters = recenters, sAbsence = noAbsence}, [say "Tracking.", ShowCamera Tracking, Record (Noted "camera connected")])
    CameraStopped problem ->
        (s, [say problem, ShowCamera (CameraDown problem), say "Trying the camera again in 3 seconds.", Record (Noted ("camera stopped: " ++ problem))])
    ControlsPolled now edges recenters -> uncurry (flip steps) (controlsLooked now edges recenters s)
    FrameArrived now stripes -> frame now stripes s
    PacketCorrupt why -> (s, [say ("# dropped a corrupt packet: " ++ why), Record (DroppedCorrupt why)])
    RecenterAsked -> recenter s
    PauseAsked -> case sSending s of
        Holding _ -> (s{sSending = Following}, [ShowOutput Live, say "Resumed."])
        Following -> (s{sSending = Holding (sLatest s)}, [ShowOutput (Paused (sLatest s)), say "Paused: games see your head held still."])
    TrimAsked ->
        let t = trimTo (sent s)
         in (s{sTrim = t}, [ShowTrim t, say ("Trimmed: forward is now " ++ describeTrim t ++ ".")])
    AimAsked -> aimTo (case sAim s of Aiming -> NotAiming; NotAiming -> Aiming) s
    AimHeld -> aimTo Aiming s
    AimLetGo -> aimTo NotAiming s
    CalibrateAsked -> (s{sTracker = request Calibrate (sTracker s)}, [Record (Requested Calibrate)])
    ClearCalibrationAsked ->
        ( s{sTracker = trackerFor defaultIntrinsics (sSettings s) defaultCalibration, sShaper = newShaper}
        ,
            [ Record ClearedCalibration
            , SaveCalibration defaultCalibration
            , say "Calibration cleared: trackkat learns it afresh as you turn your head."
            , ShowCalibration noCalibration
            , ShowClip Nothing
            ]
        )
    MarkAsked -> let n = sMarks s + 1 in (s{sMarks = n}, [Record (Marked n)])
    SettingsSaved text new -> settingsSaved text new s

say :: String -> Effect
say = Log . ForUser

-- | The pose games are sent.
sent :: Session -> HeadPose
sent s = case sSending s of
    Holding held -> held
    Following -> sLatest s

-- | A session that has looked at the controls, and what they ask for, in
-- order: a recenter if games have asked since the last look, then what the
-- keys' and buttons' presses and releases come to by the settings'
-- bindings, and then what the time passing to the look makes of the
-- presses still unsettled (see "TrackKat.Gesture").
controlsLooked :: Instant -> [KeyEdge] -> Int -> Session -> (Session, [Event])
controlsLooked now edges recenters s =
    ( s{sRecenters = recenters, sGestures = settled}
    , [RecenterAsked | recenters /= sRecenters s] ++ concatMap asked (made ++ due)
    )
  where
    bindings = boundTo (sSettings s)
    (pressed, made) =
        foldl' (\(r, done) e -> (done ++) <$> edge bindings (edgeAt e) (edgeWay e) (edgeKey e) r) (sGestures s, []) edges
    (settled, due) = tick bindings now pressed

-- | What a bound key or button asks for by how it was pressed. A hold
-- aims for as long as it lasts, where a tap or a double begins aiming or
-- ends it; for the rest, a hold asks once, as it begins.
asked :: Signal Hotkey -> [Event]
asked signal = case signal of
    Made k -> [once k]
    HoldBegan AimKey -> [AimHeld]
    HoldBegan k -> [once k]
    HoldEnded AimKey -> [AimLetGo]
    HoldEnded _ -> []
  where
    once RecenterKey = RecenterAsked
    once PauseKey = PauseAsked
    once TrimKey = TrimAsked
    once AimKey = AimAsked
    once MarkKey = MarkAsked

-- | Begin aiming, or end it, if that is a change.
aimTo :: Aiming -> Session -> (Session, [Effect])
aimTo wanted s
    | wanted == sAim s = (s, [])
    | otherwise = (s{sAim = wanted}, [ShowAim wanted, say line])
  where
    line = case wanted of
        Aiming -> "Aiming: sensitivity reduced for finer control."
        NotAiming -> "Aiming ended."

-- | Recenter on the next frame that shows the clip, undoing any trim and
-- ending aiming. If the clip is out of sight, games see the view centred
-- straight away.
recenter :: Session -> (Session, [Effect])
recenter s = ((Record (Requested Recenter) : cleared) ++) <$> aimTo NotAiming (lost s{sTracker = request Recenter (sTracker s), sTrim = NoTrim})
  where
    cleared = case sTrim s of
        NoTrim -> []
        Trim _ -> [ShowTrim NoTrim, say "Trim cleared."]
    lost s' = case sOutcome s of
        Just Tracked{} -> s'
        _ -> s'{sShaper = newShaper, sLatest = centred}

-- | Track a frame, shape the view it gives, and send games the view, or
-- the pose held.
frame :: Instant -> [Stripe] -> Session -> (Session, [Effect])
frame now stripes s =
    ( s'
    , Record (GaveFrame stripes)
        : told
        ++ map SaveCalibration (concatMap learnt events)
        ++ [say line | Just line <- [outOfSight]]
        ++ [ Publish (toGamePose (sent s'))
           , ShowFrame sighting (sent s')
           , ShowBlobs (trackerBlobs tracker) (trackerLights tracker)
           ]
    )
  where
    (tracker, outcome, events) = Tracker.step now stripes (sTracker s)
    centre = trackerCentre tracker
    calibration = trackerCalibration tracker
    told =
        concat
            [ say (describeEvent centre e)
                : concat
                    [ [ShowCalibration news, ShowClip (learntClip (trackerModel tracker) centre calibration)]
                    | Just news <- [calibrationNews centre calibration (sureEstimate (trackerShape tracker)) e]
                    ]
            | e <- events
            ]
    -- A new centre or level is a jump the shaper must not smooth.
    shaper
        | any resetsShaping events = newShaper
        | otherwise = sShaper s
    (latest, shaper') = case outcome of
        Tracked{outPose = hp} -> shape (response (sSettings s)) (sTrim s) (sAim s) now hp shaper
        Lost{} -> (sLatest s, shaper)
    (absent, outOfSight) = absence now outcome (sAbsence s)
    s' = s{sTracker = tracker, sShaper = shaper', sOutcome = Just outcome, sLatest = latest, sAbsence = absent}
    sighting = case outcome of
        Tracked{outPose = hp, outSeen = seen} -> InSight hp (seenMissing seen)
        Lost{outBlobs = n} -> OutOfSight n

-- | Apply the settings file as saved. A key or button bound otherwise than
-- it was starts afresh: what its being held down began ends, and nothing
-- more comes of a press of it under way.
settingsSaved :: String -> Config -> Session -> (Session, [Effect])
settingsSaved text new s = (applied ++) <$> steps (concatMap asked ended) s{sSettings = new, sTracker = resettled old new (sTracker s), sGestures = watched}
  where
    applied =
        [Record (TookSettings text), BindHotkeys new]
            ++ [PointGames (npclient new) | npclient new /= npclient old]
            ++ [SetStartup (startup new) | startup new /= startup old]
            -- Clicking through the head view saves it; that is no news.
            ++ [say (describeChanges old new) | new /= old{headView = headView new}]
            ++ [say "Tracking the clip's new shape; recentering." | clip new /= clip old]
            ++ [ShowSettings text]
    old = sSettings s
    (watched, ended) = forget (\k -> boundTo new k /= boundTo old k) (sGestures s)

-- | A trim as the console and log show it.
describeTrim :: Trim -> String
describeTrim NoTrim = "straight ahead"
describeTrim (Trim t) =
    printf "yaw %+.0f pitch %+.0f roll %+.0f, x %+.0f y %+.0f z %+.0f mm" (yaw t) (pitch t) (roll t) (tx t) (ty t) (tz t)

-- | What the window says where the calibration would be, before any is
-- learnt.
noCalibration :: String
noCalibration = "No calibration yet: trackkat learns it as you turn your head."

-- | Refinements are applied slowly, so only a new centre or a requested
-- calibration resets shaping.
resetsShaping :: Tracker.Event -> Bool
resetsShaping Tracker.Recentered = True
resetsShaping (Tracker.Calibrated _) = True
resetsShaping _ = False

-- | Describe an event, given the clip's pose at the centre.
describeEvent :: Maybe Pose -> Tracker.Event -> String
describeEvent _ Tracker.Recentered = "Recentered."
describeEvent _ Tracker.CalibrationStarted = "Calibrating: turn your head left and right."
describeEvent _ Tracker.CalibrationFailed = "Calibration failed: not enough turning, try again."
describeEvent _ (Tracker.Calibrated c) =
    let (down, right) = cameraTilt (estimatedLevel c)
     in printf "Calibrated: camera tipped %.1f degrees down, %.1f to its right." down right
describeEvent _ (Tracker.Refined LevelRefined c) = describeLevel c
describeEvent centre (Tracker.Refined PivotRefined c) = describePivot centre c
describeEvent _ (Tracker.ShapeEstimated est) = describeShape est
describeEvent _ Tracker.Corrected{} = "Corrected a mirror-image pose."
describeEvent _ (Tracker.LightIgnored p why) = describeLightIgnored p why
describeEvent _ (Tracker.LightForgotten p) = describeLightForgotten p
describeEvent _ (Tracker.LightMoved p how) = describeLightMoved p how

-- | A calibration to save.
learnt :: Tracker.Event -> [Calibration]
learnt (Tracker.Calibrated c) = [c]
learnt (Tracker.Refined _ c) = [c]
learnt _ = []

-- | What the calibration box in trackkat's window says after an event, if
-- the event is about calibration or moves the centre, given the clip's pose
-- at the centre, the calibration so far and any sure estimate of the
-- clip's shape.
calibrationNews :: Maybe Pose -> Calibration -> Maybe ShapeEstimate -> Tracker.Event -> Maybe String
calibrationNews centre c estimate Tracker.Recentered = Just (describeCalibration centre c estimate)
calibrationNews centre _ estimate (Tracker.Calibrated c) =
    let (down, right) = cameraTilt (estimatedLevel c)
     in Just (printf "Calibrated: camera tipped %.1f° down and %.1f° to its right." down right ++ "\n" ++ describePivotBriefly centre c ++ maybe "" (("\n" ++) . describeShapeBriefly) estimate)
calibrationNews centre _ estimate (Tracker.Refined _ c) = Just (describeCalibration centre c estimate)
calibrationNews centre c estimate (Tracker.ShapeEstimated _) = Just (describeCalibration centre c estimate)
calibrationNews _ _ _ Tracker.Corrected{} = Nothing
calibrationNews _ _ _ Tracker.LightIgnored{} = Nothing
calibrationNews _ _ _ (Tracker.LightForgotten _) = Nothing
calibrationNews _ _ _ Tracker.LightMoved{} = Nothing
calibrationNews centre _ _ e = Just (describeEvent centre e)

-- | The window's account of a calibration, on two lines, and a third for
-- a sure estimate of the clip's shape.
describeCalibration :: Maybe Pose -> Calibration -> Maybe ShapeEstimate -> String
describeCalibration centre c estimate = describeLevelBriefly c ++ "\n" ++ describePivotBriefly centre c ++ maybe "" (("\n" ++) . describeShapeBriefly) estimate

-- | The status line for a frame's outcome, what games are being sent, and
-- how many games are tracking.
status :: Outcome -> Sending -> Int -> String
status outcome sending games =
    concat
        [ case outcome of
            Tracked{outPose = hp, outSeen = seen} ->
                printf "yaw %+6.1f  pitch %+6.1f  roll %+6.1f   x %+4.0f  y %+4.0f  z %+4.0f mm" (yaw hp) (pitch hp) (roll hp) (tx hp) (ty hp) (tz hp)
                    ++ describeSeen seen
            Lost n masked why _ ->
                "lost the clip (" ++ show n ++ " blob" ++ (if n == 1 then "" else "s") ++ (if masked > 0 then ", " ++ describeMasked masked else "") ++ "): " ++ describeWhy why
        , case sending of
            Holding _ -> "   PAUSED"
            Following -> ""
        , if games > 0 then "   game connected" else ""
        ]
