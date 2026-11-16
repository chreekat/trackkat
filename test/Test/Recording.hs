{-# LANGUAGE LambdaCase #-}

-- | Recording and replaying (see "TrackKat.Recording"): a session recorded
-- as tirprobe records one replays to the very same outcomes, events and
-- poses, and the recordings of real problems in 'recordingRegressions'
-- replay as they should; and writing a recording never holds up whoever
-- records (see "TrackKat.Recorder").
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
import Test.Simulation (clipBent, headSeen)
import TrackKat.Calibration
import TrackKat.Config
import TrackKat.Frame

import TrackKat.Pose
import TrackKat.Recorder
import TrackKat.Recording
import TrackKat.Synth
import TrackKat.Tracker

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
        , testProperty "a recording cut short after its first entry is read up to its last whole line" $
            \(Recorded r) ->
                let whole = written r
                    start = BL.length (toLazyByteString (renderStart (recStart r)))
                    first = BL.length (toLazyByteString (foldMap (renderEntry (recStart r)) (take 1 (recEntries r))))
                 in forAllShrink (choose (start + first, BL.length whole)) (earlier [BL.length whole, start + first]) $ \cut ->
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
            map (`describeMark` 3) [Just Whole, Just Partial, Nothing]
                @?= ["Mark 3 recorded.", "No mark: the recording has stopped.", "No mark: start trackkat with --record to record."]
            assertBool "" (all ((<= 80) . length) (describeStopped "" : describeRecording "" : map (`describeMark` 3) [Just Whole, Just Partial, Nothing]))
        ]
  where
    camera start = let k = startIntrinsics start in (focal k, cx k, cy k)
    tracked _ [] = ([], [])
    tracked t ((inputs, at, n, stripes) : rest) =
        let (entries, (t', o, evs)) = liveFrame inputs at n stripes t
            (more, came) = tracked t' rest
         in (entries ++ more, stepped (trackingTracker t') o evs : came)

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
