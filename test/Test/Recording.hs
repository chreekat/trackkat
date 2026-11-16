{-# LANGUAGE LambdaCase #-}

-- | Recording and replaying (see "TrackKat.Recording"): a session recorded
-- as tirprobe records one replays to the very same outcomes, events and
-- poses, and the recordings of real problems in 'recordingRegressions'
-- replay as they should; a session of trackkat, whatever happens to it,
-- replays from its recording to what its tracker came to; and writing a
-- recording never holds up whoever records (see "TrackKat.Recorder").
module Test.Recording (simulations, properties) where

import Control.Concurrent (forkFinally)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Exception (throwIO)
import Control.Monad (replicateM, replicateM_)
import qualified Data.ByteString as B
import Data.ByteString.Builder (Builder, toLazyByteString)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlphaNum)
import Data.Either (isLeft)
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.List (isInfixOf, mapAccumL, nub)
import Data.Word (Word64, Word8)
import System.Directory (getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck

import Test.Gen (earlier, genConfig, shrinkFrom, shrinkParts)
import Test.Instances ()
import Test.Session (Asked (..), Move (..), Moves (..), timeline)
import Test.Simulation (clipBent, headSeen)
import TrackKat.Calibration
import TrackKat.Config
import TrackKat.Frame hiding (Packet (..))

import TrackKat.Pose
import TrackKat.Recorder
import TrackKat.Recording
import TrackKat.Session (Effect (Record), Session (..), newSession)
import qualified TrackKat.Session as Session
import TrackKat.Synth
import TrackKat.Trackers

simulations :: TestTree
simulations =
    testGroup
        "Recording"
        [ withResource recordSession (const (pure ())) $ \session ->
            testGroup
                "a synthetic session"
                [ testCase "the recording's start reads back" $ reading session $ \_ r ->
                    assertBool "" $
                        recStart r == sessionStart{startCalibration = startCalibration (recStart r)}
                            && fmap (renderCalibration . trackerCalibration) (startTracker (recStart r)) == fmap (renderCalibration . trackerCalibration) (startTracker sessionStart)
                , testCase "the recording's entries read back" $ reading session $ \_ r ->
                    assertBool "" $ recEntries r == concatMap sessionEntries recordedSession
                , testCase "the session is not trivial" $ reading session $ \live _ ->
                    assertBool (show (sessionKinds live)) $
                        all (`elem` sessionKinds live) ["Recentered", "CalibrationStarted", "Calibrated", "LightIgnored", "Tracked", "Lost"]
                , testCase "the recording replays to the same outcomes, events and poses" $ reading session $ \live r ->
                    assertBool "" $ not (null live) && [s | Just s <- either (const []) (map steppedOf) (replay r)] == live
                , testCase "the recording replayed with other settings differs" $ reading session $ \live r ->
                    assertBool "" $ case replay r{recStart = (recStart r){startSettings = settingValues (renderConfig defaultConfig{lightMasking = NoMasking})}} of
                        Right rs -> [s | Replayed _ (Just (t, o, evs)) <- rs, let { s = stepped t o evs }] /= live
                        Left _ -> False
                , testCase "the events before a mark" $ reading session $ \_ r ->
                    let beforeMark = either (const []) (eventsBeforeMark 1 7.5) (replay r)
                     in assertBool (show beforeMark) $ not (null [() | (_, Recentered) <- beforeMark])
                , -- Its last line is cut at its newline, the hardest to tell.
                  testCase "a recording cut short, as when the console is closed, replays up to its last whole line" $ do
                    (_, whole) <- session
                    assertBool "" $ case replay =<< parseRecording (BL.take (BL.length whole - 1) whole) of
                        Right rs -> case reverse [e | Replayed e _ <- rs] of
                            Unreadable _ "cut short" : FrameEntry{} : _ -> length rs == length (concatMap sessionEntries recordedSession)
                            _ -> False
                        Left _ -> False
                ]
        , testCase "a session of trackkat, its threads recording it to a file as it goes, replays from the file to what its tracker came to" $ do
            tmp <- getTemporaryDirectory
            let path = tmp </> "trackkat-test-session-recording.txt"
                moves = [Connect, See 0 0, Ask ToMark, See 2 0, Save defaultConfig{lightMasking = NoMasking}, See 4 1, Ask ToClear, Corrupt, See 6 1, Ask ToCalibrate, See 4 0, Stop "The camera stopped.", Ask ToRecenter, See 2 0]
                began = Beginning (renderConfig defaultConfig) Nothing
            (start, session0) <- either (assertFailure . ("the session's start: " ++)) pure (beginning began)
            sessionAt <- newIORef session0
            live <- withRecorder path start (const (pure ())) $ \opened -> do
                rec <- either (\err -> ioError (userError ("the session's file: " ++ err))) pure opened
                recorder <- newSessionRecorder start rec
                came <- concat <$> mapM (taken recorder sessionAt) (zip (cycle [0 ..]) (timeline moves))
                ending <- finish rec
                ending @?= Whole
                pure came
            whole <- B.readFile path
            removePathForcibly path
            fmap replayedTo (replay =<< parseRecording (BL.fromStrict whole)) @?= Right live
            assertBool (show live) (length live == 6)
        , testCase "a recording is replayed by the tracker it says, and by the old one if it doesn't say, as those made before there were two" $
            let saying said = sessionStart{startSettings = [named | named@(name, _) <- startSettings sessionStart, name /= "tracker"] ++ [("tracker", which) | Just which <- [said]]}
             in map (fmap whichTracker . startTracker . saying) [Nothing, Just "old", Just "new"] @?= [Right OldTracker, Right OldTracker, Right NewTracker]
        , testCase "a later version of recording is refused" $
            assertBool "" (isLeft (parseRecording (BL.fromStrict (C.pack "trackkat recording 3\n"))))
        , testGroup
            "recordings of real problems"
            [ testCase (file ++ ": " ++ name) $ do
                r <- readRecording file
                assertBool "" $ case replay =<< r of
                    Right rs -> null [() | Replayed Unreadable{} _ <- rs] && ok rs
                    Left _ -> False
            | (name, file, ok) <- recordingRegressions
            ]
        ]
  where
    steppedOf (Replayed _ s) = (\(t, o, evs) -> stepped t o evs) <$> s
    -- Take a session through an event, recording it; what a frame came to.
    taken recorder sessionAt (i, (at, event)) = do
        s <-
            recordStep recorder at i $
                atomicModifyIORef' sessionAt (\before -> let (after, effects) = Session.step event before in (after, (after, [did | Record did <- effects])))
        pure [cameTo (sTracker s) outcome | Session.FrameArrived{} <- [event], Just outcome <- [sOutcome s]]

-- | Check something of the session as tracked live and of its recording
-- read back.
reading :: IO ([String], BL.ByteString) -> ([String] -> Recording -> Assertion) -> Assertion
reading session holds = do
    (live, whole) <- session
    either (assertFailure . ("the recording reads back: " ++)) (holds live) (parseRecording whole)

-- | Track 'recordedSession' live, recording it to a file as tirprobe does:
-- what each frame stepped to, and the file's bytes. The session is of
-- fewer entries than a recorder's 'backlog', so that however slowly they
-- are written, none is dropped.
recordSession :: IO ([String], BL.ByteString)
recordSession = do
    tmp <- getTemporaryDirectory
    let path = tmp </> "trackkat-test-recording.txt"
    t0 <- orFail "the session's start: " (startTracking sessionStart)
    live <- withRecorder path sessionStart (const (pure ())) $ \opened -> do
        rec <- orFail "the session's file: " opened
        let go _ [] = pure []
            go t ((at, n, stripes, inputs, corrupt) : rest) = do
                mapM_ (record rec . CorruptEntry at) corrupt
                (t', o, evs) <- trackLive rec inputs at n stripes t
                (stepped (trackingTracker t') o evs :) <$> go t' rest
        go t0 recordedSession
    whole <- B.readFile path
    removePathForcibly path
    pure (live, BL.fromStrict whole)
  where
    orFail what = either (\err -> ioError (userError (what ++ err))) pure

-- | What the synthetic session's tracker is made from.
sessionStart :: Start
sessionStart =
    Start
        { startNotes = ["a synthetic session, recorded by Zoë"]
        , startIntrinsics = defaultIntrinsics{focal = 661.5}
        , startSettings = settingValues (renderConfig defaultConfig)
        , startCalibration = SavedCalibration (renderCalibration defaultCalibration)
        , startClock = sessionClock
        }

-- | Recordings of real problems (from tirprobe --record), checked in under
-- test/recordings, each with what replaying it should show: a name for
-- it, the file, and a test. Cut a recording down to the seconds around its
-- mark by deleting frame lines; the start must stay. For example, for a
-- recording whose view turned to its mirror image just before its first
-- mark, which a fix stops:
--
-- > ("no mirror image before the mark", "test/recordings/flip.txt", \rs -> null [() | (_, Corrected{}) <- eventsBeforeMark 1 2 rs])
recordingRegressions :: [(String, FilePath, [Replayed] -> Bool)]
recordingRegressions = []

-- | The events of the frames in the given seconds before a mark, with
-- their times on the monotonic clock in nanoseconds.
eventsBeforeMark :: Int -> Double -> [Replayed] -> [(Word64, Event)]
eventsBeforeMark m seconds rs = case [at | Replayed (InputEntry at (Mark k)) _ <- rs, k == m] of
    marked : _ -> [(at, ev) | Replayed (FrameEntry at _ _) (Just (_, _, evs)) <- rs, at <= marked, fromIntegral (marked - at) <= seconds * 1e9, ev <- evs]
    [] -> []

-- | What a frame stepped to, comparably: outcome, events, the pose tracked
-- and recentered on, and the calibration.
stepped :: Tracker -> Outcome -> [Event] -> String
stepped t o evs = show (o, evs, trackerPose t, trackerCentre t, renderCalibration (trackerCalibration t))

-- | The kinds of outcome and event in a session's steps.
sessionKinds :: [String] -> [String]
sessionKinds = nub . concatMap (\s -> filter (`isInfixOf` s) ["Recentered", "CalibrationStarted", "Calibrated", "LightIgnored", "Tracked", "Lost"])

-- | The monotonic clock, in nanoseconds, at the synthetic session's start.
sessionClock :: Word64
sessionClock = 81723349120417

-- | A synthetic session at 120 frames a second: a head turning left and
-- right in front of a lit fire, out of sight for half a second; Enter
-- pressed after a second, c Enter after two, and m Enter and Enter after
-- eight; one corrupt packet. Each frame's time, counter, stripes, what the
-- user did before it, and any corrupt packet before it.
recordedSession :: [(Word64, Word8, [Stripe], [(Word64, Input)], Maybe String)]
recordedSession =
    [ (at, fromIntegral i, renderDiscs (heads ++ occluded heads (fireDiscs (156, 297) i)), inputs i (at - 1000), corrupt i)
    | i <- [0 .. 1199 :: Int]
    , let at = sessionClock + fromIntegral i * 8333333
          t = fromIntegral i / 120 :: Double
          pose = headSeen (-23) (25 * sin (2 * pi * t / 2.5)) (5 * sin (2 * pi * t / 3.7)) 0
          heads = if t > 6 && t < 6.5 then [] else pointDiscs defaultIntrinsics clipBent (1, 1, 1) pose
    ]
  where
    inputs i pressed = case i of
        120 -> [(pressed, Press Recenter)]
        240 -> [(pressed, Press Calibrate)]
        960 -> [(pressed, Mark 1), (pressed + 5, Press Recenter)]
        _ -> []
    corrupt i = if i == 500 then Just "bad header checksum" else Nothing

-- | The entries a frame of 'recordedSession' is recorded as.
sessionEntries :: (Word64, Word8, [Stripe], [(Word64, Input)], Maybe String) -> [Entry]
sessionEntries (at, n, stripes, inputs, corrupt) =
    [CorruptEntry at why | Just why <- [corrupt]] ++ [InputEntry pressed i | (pressed, i) <- inputs] ++ [FrameEntry at n stripes]

-- | Properties of recordings as text, and of replaying them.
properties :: TestTree
properties =
    testGroup
        "Recording"
        [ testProperty "a recording is read back as written" $
            \(Recorded r) ->
                case parseRecording (written r) of
                    Left err -> counterexample err False
                    Right back ->
                        let (start, start') = (recStart r, recStart back)
                         in conjoin
                                [ startNotes start' === startNotes start
                                , camera start' === camera start
                                , startSettings start' === startSettings start
                                , startClock start' === startClock start
                                , recEntries back === recEntries r
                                , written back === written r
                                ]
        , testProperty "a recording cut short after its start is read up to its last whole line" $
            \(Recorded r) ->
                let whole = written r
                    start = BL.length (toLazyByteString (renderStart (recStart r)))
                 in forAllShrink (choose (start, BL.length whole)) (earlier [BL.length whole, start]) $ \cut ->
                        let kept = BL.take cut whole
                            wholeLines = length (BL.elemIndices 10 (BL.drop start kept))
                            cutShort = cut > start && BL.last kept /= 10
                         in case parseRecording kept of
                                Left err -> counterexample err False
                                Right back ->
                                    take wholeLines (recEntries back) === take wholeLines (recEntries r)
                                        .&&. [why | Unreadable _ why <- drop wholeLines (recEntries back)] === ["cut short" | cutShort]
                                        .&&. length (recEntries back) === wholeLines + (if cutShort then 1 else 0)
        , testProperty "a session replays from its recording to what it came to live" $
            \(Glances session) ->
                case startTracking sessionStart of
                    Left err -> counterexample err False
                    Right t0 ->
                        let (entries, live) = tracked t0 session
                         in case replay =<< parseRecording (written (Recording sessionStart entries)) of
                                Left err -> counterexample err False
                                Right replayed -> [stepped t o evs | Replayed _ (Just (t, o, evs)) <- replayed] === live
        , testProperty "a recording said to be of the first version, which had no settings saved, calibration cleared or notes, reads the same" $
            \(Recorded r) ->
                let whole = written r
                    older = BL.fromStrict (C.pack "trackkat recording 1") <> BL.dropWhile (/= 10) whole
                 in fmap recEntries (parseRecording older) === fmap recEntries (parseRecording whole)
        , testProperty "a setting given a value it can't have ends what is read of a recording" $
            \(Recorded r) ->
                let at = startClock (recStart r)
                    unread = fmap recEntries (parseRecording (written r{recEntries = recEntries r ++ [InputEntry at (Setting "clip" "1 1 5"), InputEntry at (Press Recenter)]}))
                 in fmap (take (length (recEntries r))) unread === Right (recEntries r)
                        .&&. fmap (length . drop (length (recEntries r))) unread === Right 1
        , testProperty "replayed, a setting given a value it can't have changes nothing" $
            \(Glances session) ->
                let entries = either (const []) (\t0 -> fst (tracked t0 session)) (startTracking sessionStart)
                    outcomes es = fmap (\rs -> [stepped t o evs | Replayed _ (Just (t, o, evs)) <- rs]) (replay (Recording sessionStart es))
                 in outcomes (InputEntry sessionClock (Setting "clip" "1 1 5") : entries) === outcomes entries
        , testProperty "a session of trackkat replays from its recording to what its tracker came to, whatever happened to it" $
            \began (Moves before) (SettingsFile saved) (Moves after) ->
                case (beginning began, parseConfig saved) of
                    (Right (start, session0), Right config) ->
                        let (first, rest) = splitAt (length before + 2) (timeline (before ++ [Dark, See 0 0] ++ after ++ [Dark, See 0 0]))
                            happened = first ++ [(maximum (map fst first), Session.SettingsSaved saved config)] ++ rest
                            (entries, live, session) = lived start session0 happened
                            later = maximum (map fst happened) + 6000000000
                         in counterexample (C.unpack (BL.toStrict (written (Recording start entries)))) $
                                case replay =<< parseRecording (written (Recording start entries)) of
                                    Left err -> counterexample err False
                                    Right replayed ->
                                        replayedTo replayed === live
                                            .&&. [probed later t | Replayed _ (Just (t, _, _)) <- take 1 (reverse replayed)] === [probed later (sTracker session)]
                    (failed, _) -> counterexample (either id (const saved) failed) False
        , testProperty "a recorder writes a recording's start and what it is given, in order, and ends whole" $
            \(Recorded r) -> ioProperty $ do
                (sink, text) <- collecting (pure ())
                told <- newIORef []
                ending <- withRecorderTo sink (recStart r) (\why -> modifyIORef' told (why :)) $ \rec -> do
                    mapM_ (record rec) (recEntries r)
                    finish rec
                (,,) <$> text <*> readIORef told <*> pure ending >>= \got -> pure (got === (written r, [], Whole))
        , testCase "a recorder whose sink takes nothing in holds no one up: past its backlog it stops, saying so once, and ends short" $ do
            gate <- newEmptyMVar
            (sink, text) <- collecting (readMVar gate)
            told <- newIORef []
            let start = sessionStart
                entries = [InputEntry (sessionClock + fromIntegral i) (Mark i) | i <- [1 .. backlog + 5]]
            done <- timeout 60000000 $ withRecorderTo sink start (\why -> modifyIORef' told (why :)) $ \rec -> do
                mapM_ (record rec) entries
                standing <- soFar rec
                putMVar gate ()
                (,) standing <$> finish rec
            done @?= Just (Partial, Partial)
            readIORef told >>= (@?= ["the disk can't keep up"])
            got <- text
            fmap recEntries (parseRecording got)
                @?= Right (take backlog entries ++ [NoteEntry (sessionClock + fromIntegral (backlog + 1)) "the recording stops here: the disk couldn't keep up"])
        , testCase "a recorder whose sink fails, as a full disk does, stops there, saying why once, and ends short" $ do
            writes <- newIORef (0 :: Int)
            (sink, text) <- collecting $ do
                n <- atomicModifyIORef' writes (\n -> (n + 1, n))
                if n >= 3 then throwIO (userError "no space left") else pure ()
            told <- newIORef []
            done <- timeout 60000000 $ withRecorderTo sink sessionStart (\why -> modifyIORef' told (why :)) $ \rec -> do
                mapM_ (\i -> record rec (InputEntry sessionClock (Mark i))) [1 .. 10]
                finish rec
            done @?= Just Partial
            readIORef told >>= \whys -> assertBool (show whys) (map ("no space left" `isInfixOf`) whys == [True])
            got <- text
            fmap recEntries (parseRecording got) @?= Right [InputEntry sessionClock (Mark i) | i <- [1, 2]]
        , testCase "a file that can't be written to is no recorder, and says why" $ do
            tmp <- getTemporaryDirectory
            said <- withRecorder (tmp </> "trackkat-test-no-such-folder" </> "recording.txt") sessionStart (const (pure ())) $ \case
                Left why -> pure (Just why)
                Right _ -> pure Nothing
            assertBool (show said) (maybe False (not . null) said)
        , testCase "steps of a session taken by several threads at once are recorded in the order taken" $ do
            (sink, text) <- collecting (pure ())
            taken <- newIORef 0
            ending <- withRecorderTo sink sessionStart (const (pure ())) $ \rec -> do
                session <- newSessionRecorder sessionStart rec
                threads <- replicateM 4 $ do
                    done <- newEmptyMVar
                    _ <-
                        forkFinally
                            (replicateM_ 200 (recordStep session sessionClock 0 (atomicModifyIORef' taken (\n -> (n + 1, ((), [Marked (n + 1)]))))))
                            (putMVar done)
                    pure done
                mapM_ (\done -> takeMVar done >>= either throwIO pure) threads
                finish rec
            ending @?= Whole
            got <- text
            fmap recEntries (parseRecording got) @?= Right [InputEntry sessionClock (Mark i) | i <- [1 .. 800]]
        , testProperty "what a session did is recorded as it was done, its settings said just where the file gives them another value" $
            \(Recorded r) ->
                let start = recStart r
                    at = startClock start
                    settings = startSettings start
                    text values = unlines [name ++ " = " ++ value | (name, value) <- values]
                    changed = [(name, if name == "ignore lights" then (if value == "on" then "off" else "on") else value) | (name, value) <- settings]
                    did =
                        [ TookSettings (text settings)
                        , GaveFrame []
                        , Requested Recenter
                        , TookSettings (text changed)
                        , TookSettings (text changed)
                        , ClearedCalibration
                        , Marked 1
                        , DroppedCorrupt "too short"
                        , Noted "camera connected"
                        ]
                 in concat (snd (mapAccumL (flip (taped at 7)) (startTape start) did))
                        === [ FrameEntry at 7 []
                            , InputEntry at (Press Recenter)
                            , InputEntry at (Setting "ignore lights" (if lookup "ignore lights" settings == Just "on" then "off" else "on"))
                            , InputEntry at ClearCalibration
                            , InputEntry at (Mark 1)
                            , CorruptEntry at "too short"
                            , NoteEntry at "camera connected"
                            ]
        , testProperty "recordings are named by when they began, so as to sort by it, in letters any file system takes" $
            \(Began a) (Began b) ->
                let name (date, time) = recordingName date time
                 in compare (name a) (name b) === compare a b
                        .&&. counterexample (name a) (all (\c -> isAlphaNum c || c `elem` "-.") (name a))
        , testCase "what the log says of a recording and of marks" $ do
            describeRecording "C:\\r.txt" @?= "Recording to C:\\r.txt: about 6 MB a minute, until trackkat quits."
            describeStopped "the disk can't keep up" @?= "Recording stopped: the disk can't keep up. Tracking goes on."
            describeUnrecorded "no such folder" @?= "Can't record: no such folder. Tracking goes on."
            describeRecorded "C:\\r.txt" @?= "Recording saved to C:\\r.txt."
            map (`describeMark` 3) [Just Whole, Just Partial, Nothing]
                @?= ["Mark 3 recorded.", "No mark: the recording has stopped.", "No mark: start trackkat with --record to record."]
            assertBool "" (all ((<= 80) . length) (describeStopped "" : describeUnrecorded "" : describeRecorded "" : describeRecording "" : map (`describeMark` 3) [Just Whole, Just Partial, Nothing]))
        ]
  where
    camera start = let k = startIntrinsics start in (focal k, cx k, cy k)
    tracked _ [] = ([], [])
    tracked t ((inputs, at, n, stripes) : rest) =
        let (entries, (t', o, evs)) = liveFrame inputs at n stripes t
            (more, came) = tracked t' rest
         in (entries ++ more, stepped (trackingTracker t') o evs : came)

-- | What a frame came to, comparably: what it showed, the pose tracked
-- and recentered on, and the calibration.
cameTo :: Tracker -> Outcome -> String
cameTo t o = show (o, trackerPose t, trackerCentre t, renderCalibration (trackerCalibration t))

-- | What each frame replayed came to.
replayedTo :: [Replayed] -> [String]
replayedTo replayed = [cameTo t o | Replayed _ (Just (t, o, _)) <- replayed]

-- | Take a session whose recording starts as given through what happens
-- to it, each event at its time on the monotonic clock: what its
-- recording says, what each of its frames came to, and the session it
-- came to.
lived :: Start -> Session -> [(Word64, Session.Event)] -> ([Entry], [String], Session)
lived start session0 happened = (concat entries, concat came, session)
  where
    ((_, session), (entries, came)) = unzip <$> mapAccumL go (startTape start, session0) (zip (cycle [0 ..]) happened)
    go (tape, s) (i, (at, event)) =
        let (s', effects) = Session.step event s
            (tape', said) = mapAccumL (flip (taped at i)) tape [did | Record did <- effects]
         in ((tape', s'), (concat said, [cameTo (sTracker s') outcome | Session.FrameArrived{} <- [event], Just outcome <- [sOutcome s']]))

-- | What a tracker comes to shown the clip from a later time on the
-- monotonic clock, late enough to end a calibration asked for: what tells
-- of what it was asked for and has yet to do.
probed :: Word64 -> Tracker -> [String]
probed later t0 = snd (mapAccumL shown t0 [later, later + 8333333, later + 16666666])
  where
    shown t at = let (t', o, evs) = step (trackerTime at) clipSeen t in (t', stepped t' o evs)
    clipSeen = concat [stripes | (_, Session.FrameArrived _ stripes) <- timeline [See 0 0]]

-- | How a session of trackkat begins: the settings file's text, and the
-- calibration file's, if there is one. Simplest the default settings and
-- no calibration.
data Beginning = Beginning String (Maybe String)
    deriving (Eq, Show)

instance Arbitrary Beginning where
    arbitrary = Beginning <$> elements settingsFiles <*> elements calibrationFiles
    shrink (Beginning settings calibration) =
        shrinkParts
            [ \(Beginning s c) -> [Beginning s' c | s' <- earlier settingsFiles s]
            , \(Beginning s c) -> [Beginning s c' | c' <- earlier calibrationFiles c]
            ]
            (Beginning settings calibration)

-- | The session that begins so, as the monotonic clock starts, and how
-- its recording starts, unless a file can't be read.
beginning :: Beginning -> Either String (Start, Session)
beginning (Beginning settings calibration) = do
    config <- parseConfig settings
    learnt <- maybe (Right defaultCalibration) parseCalibration calibration
    pure (startOfSession ["a session of trackkat"] settings calibration 0, newSession config learnt)

-- | What a calibration file may say, if there is one: nothing learnt, or
-- a camera tipped down and a neck some way behind the clip.
calibrationFiles :: [Maybe String]
calibrationFiles =
    [ Nothing
    , Just (renderCalibration defaultCalibration)
    , Just "# as learned\nup = 0.05 -0.95 0.3\nturning = 412.5\npivot = 3 92.25 81\npivot windows = 40\npivot information = 2.5 0 0.1 3 0.2 1.5\n"
    ]

-- | A settings file's text, as trackkat writes it or as a user might
-- leave it. Simplest the default file.
newtype SettingsFile = SettingsFile String
    deriving (Show)

instance Arbitrary SettingsFile where
    arbitrary = SettingsFile <$> elements settingsFiles
    shrink (SettingsFile text) = SettingsFile <$> earlier settingsFiles text

-- | Settings files: the default, and some that say a clip, its distances
-- to more decimals than trackkat writes, and whether to ignore lights.
settingsFiles :: [String]
settingsFiles =
    [ renderConfig defaultConfig
    , "clip = 70.00003 110.00002 109.99997\n"
    , "# mine\nignore lights = off\nclip = 69.123456 111.5 108.25  # measured\nmark = F8\n"
    , renderConfig defaultConfig{lightMasking = NoMasking}
    ]

-- | A sink that keeps what it is given, once the action given has let it
-- be written, and what it has kept so far.
collecting :: IO () -> IO (Sink, IO BL.ByteString)
collecting before = do
    kept <- newIORef (mempty :: Builder)
    pure (Sink (\text -> before >> modifyIORef' kept (<> text)) (pure ()) (pure ()), toLazyByteString <$> readIORef kept)

-- | When a recording began: a date and a time of day, this century.
-- Simplest the century's first midnight, and simpler with any part so.
newtype Began = Began ((Integer, Int, Int), (Int, Int, Int))
    deriving (Show)

instance Arbitrary Began where
    arbitrary = do
        date <- (,,) <$> chooseInteger (2000, 2099) <*> chooseInt (1, 12) <*> chooseInt (1, 31)
        Began . (,) date <$> ((,,) <$> chooseInt (0, 23) <*> chooseInt (0, 59) <*> chooseInt (0, 59))
    shrink (Began b) =
        Began
            <$> shrinkParts
                [ \((y, m, d), t) -> [((2000, m, d), t) | y /= 2000]
                , \((y, m, d), t) -> [((y, m', d), t) | m' <- shrinkFrom 1 m]
                , \((y, m, d), t) -> [((y, m, d'), t) | d' <- shrinkFrom 1 d]
                , \(date, (h, m, sec)) -> [(date, (h', m, sec)) | h' <- shrink h]
                , \(date, (h, m, sec)) -> [(date, (h, m', sec)) | m' <- shrink m]
                , \(date, (h, m, sec)) -> [(date, (h, m, sec')) | sec' <- shrink sec]
                ]
                b

-- | A recording's text.
written :: Recording -> BL.ByteString
written r = toLazyByteString (renderStart (recStart r) <> foldMap (renderEntry (recStart r)) (recEntries r))

-- | A recording of anything: notes, any camera, settings a settings file
-- can say, and frames, presses, marks, corrupt packets, settings saved,
-- the calibration cleared and notes at times around the clock's. Simplest of no entries or notes, with the default camera
-- and no calibration; and simpler with one of those so, or fewer. (Its
-- settings and its clock are left as they are.)
newtype Recorded = Recorded Recording
    deriving (Show)

instance Arbitrary Recorded where
    arbitrary = Recorded <$> genRecording
    shrink (Recorded r) = Recorded <$> shrinkRecording r

genRecording :: Gen Recording
genRecording = do
    notes <- listOf text
    k <- Intrinsics <$> arbitrary <*> arbitrary <*> arbitrary
    settings <- settingValues . renderConfig <$> genConfig
    saved <- settingValues . renderConfig <$> genConfig
    calibration <- elements [FreshCalibration, SavedCalibration (renderCalibration defaultCalibration)]
    clock <- chooseBoundedIntegral (0, maxBound `div` 2)
    let time = (clock +) <$> chooseBoundedIntegral (0, 3600 * 1000000000)
        entry =
            oneof
                [ FrameEntry <$> time <*> arbitrary <*> scale (`div` 10) (listOf arbitrary)
                , InputEntry <$> time <*> elements [Press Recenter, Press Calibrate]
                , InputEntry <$> time <*> (Mark <$> arbitrary)
                , CorruptEntry <$> time <*> text
                , InputEntry <$> time <*> (uncurry Setting <$> elements saved)
                , InputEntry <$> time <*> pure ClearCalibration
                , NoteEntry <$> time <*> text
                ]
    Recording (Start notes k settings calibration clock) <$> scale (`div` 4) (listOf entry)
  where
    text = unwords <$> scale (`div` 10) (listOf (elements ["tirprobe", "0.1.142+3f9c2e1", "--record", "Zoë's", "bad", "header", "checksum", "#", "="]))

shrinkRecording :: Recording -> [Recording]
shrinkRecording =
    shrinkParts
        [ \(Recording start entries) -> [Recording start entries' | entries' <- shrinkList (const []) entries]
        , \(Recording start entries) -> [Recording start{startNotes = notes} entries | notes <- shrinkList (const []) (startNotes start)]
        , \(Recording start entries) -> [Recording start{startIntrinsics = defaultIntrinsics} entries | startIntrinsics start /= defaultIntrinsics]
        , \(Recording start entries) -> [Recording start{startCalibration = FreshCalibration} entries | startCalibration start /= FreshCalibration]
        ]

-- | A few frames of a head glancing about, with what the user did before
-- each: the presses and marks, when the frame was seen, its counter and
-- its stripes. Simpler ending sooner.
newtype Glances = Glances [([(Word64, Input)], Word64, Word8, [Stripe])]
    deriving (Show)

instance Arbitrary Glances where
    arbitrary = Glances <$> genSession
    shrink (Glances session) = [Glances (take k session) | k <- [0 .. length session - 1]]

genSession :: Gen [([(Word64, Input)], Word64, Word8, [Stripe])]
genSession = do
    count <- chooseInt (0, 12)
    turns <- vectorOf count ((,) <$> choose (-3, 3) <*> choose (-2, 2))
    inputs <- vectorOf count (frequency [(4, pure []), (1, listOf1 (elements [Press Recenter, Press Calibrate, Mark 1]))])
    let headings = drop 1 (scanl (\(y, p) (dy, dp) -> (y + dy, p + dp)) (0, 0) turns)
    pure
        [ ([(at - 1000, input) | input <- pressed], at, fromIntegral i, renderDiscs (pointDiscs defaultIntrinsics clipBent (1, 1, 1) (headSeen (-23) y p 0)))
        | (i, (y, p), pressed) <- zip3 [0 :: Int ..] headings inputs
        , let at = sessionClock + fromIntegral i * 8333333
        ]
