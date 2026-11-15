-- | Recording and replaying (see "TrackKat.Recording"): a session recorded
-- as tirprobe records one replays to the very same outcomes, events and
-- poses, and the recordings of real problems in 'recordingRegressions'
-- replay as they should.
module Test.Recording (simulations, properties) where

import qualified Data.ByteString as B
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import Data.Either (isLeft)
import Data.List (isInfixOf, nub)
import Data.Word (Word64, Word8)
import System.Directory (getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase)
import Test.Tasty.QuickCheck

import Test.Frame (genStripe)
import Test.Gen (genConfig)
import Test.Simulation (clipBent, headSeen)
import TrackKat.Calibration
import TrackKat.Config
import TrackKat.Frame

import TrackKat.Pose
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
                        show (recStart r) == show sessionStart{startCalibration = startCalibration (recStart r)}
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
        , testCase "another version of recording is refused" $
            assertBool "" (isLeft (parseRecording (BL.fromStrict (C.pack "trackkat recording 2\n"))))
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
-- what each frame stepped to, and the file's bytes.
recordSession :: IO ([String], BL.ByteString)
recordSession = do
    tmp <- getTemporaryDirectory
    let path = tmp </> "trackkat-test-recording.txt"
    t0 <- either (\err -> ioError (userError ("the session's start: " ++ err))) pure (startTracker sessionStart)
    live <- withRecorder path sessionStart $ \rec -> do
        let go _ [] = pure []
            go t ((at, n, stripes, inputs, corrupt) : rest) = do
                mapM_ (record rec . CorruptEntry at) corrupt
                (t', o, evs) <- trackLive rec inputs at n stripes t
                (stepped t' o evs :) <$> go t' rest
        go t0 recordedSession
    whole <- B.readFile path
    removePathForcibly path
    pure (live, BL.fromStrict whole)

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
            forAllShrink genRecording shrinkRecording $ \r ->
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
            forAllShrink genRecording shrinkRecording $ \r ->
                let whole = written r
                    start = BL.length (toLazyByteString (renderStart (recStart r)))
                    first = BL.length (toLazyByteString (foldMap (renderEntry (recStart r)) (take 1 (recEntries r))))
                 in forAll (choose (start + first, BL.length whole)) $ \cut ->
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
            forAllShrink genSession shrinkSession $ \session ->
                case startTracker sessionStart of
                    Left err -> counterexample err False
                    Right t0 ->
                        let (entries, live) = tracked t0 session
                         in case replay =<< parseRecording (written (Recording sessionStart entries)) of
                                Left err -> counterexample err False
                                Right replayed -> [stepped t o evs | Replayed _ (Just (t, o, evs)) <- replayed] === live
        ]
  where
    camera start = let k = startIntrinsics start in (focal k, cx k, cy k)
    tracked _ [] = ([], [])
    tracked t ((inputs, at, n, stripes) : rest) =
        let (entries, (t', o, evs)) = liveFrame inputs at n stripes t
            (more, came) = tracked t' rest
         in (entries ++ more, stepped t' o evs : came)

-- | A recording's text.
written :: Recording -> BL.ByteString
written r = toLazyByteString (renderStart (recStart r) <> foldMap (renderEntry (recStart r)) (recEntries r))

-- | A recording of anything: notes, any camera, settings a settings file
-- can say, and frames, presses, marks and corrupt packets at times around
-- the clock's.
genRecording :: Gen Recording
genRecording = do
    notes <- listOf text
    k <- Intrinsics <$> arbitrary <*> arbitrary <*> arbitrary
    settings <- settingValues . renderConfig <$> genConfig
    calibration <- elements [FreshCalibration, SavedCalibration (renderCalibration defaultCalibration)]
    clock <- chooseBoundedIntegral (0, maxBound `div` 2)
    let time = (clock +) <$> chooseBoundedIntegral (0, 3600 * 1000000000)
        entry =
            oneof
                [ FrameEntry <$> time <*> arbitrary <*> scale (`div` 10) (listOf genStripe)
                , InputEntry <$> time <*> elements [Press Recenter, Press Calibrate]
                , InputEntry <$> time <*> (Mark <$> arbitrary)
                , CorruptEntry <$> time <*> text
                ]
    Recording (Start notes k settings calibration clock) <$> scale (`div` 4) (listOf entry)
  where
    text = unwords <$> scale (`div` 10) (listOf (elements ["tirprobe", "0.1.142+3f9c2e1", "--record", "Zoë's", "bad", "header", "checksum", "#", "="]))

-- | Recordings of fewer entries and notes, with the default camera.
shrinkRecording :: Recording -> [Recording]
shrinkRecording (Recording start entries) =
    [Recording start entries' | entries' <- shrinkList (const []) entries]
        ++ [Recording start{startNotes = notes} entries | notes <- shrinkList (const []) (startNotes start)]
        ++ [Recording start{startIntrinsics = defaultIntrinsics} entries | camera /= (focal defaultIntrinsics, cx defaultIntrinsics, cy defaultIntrinsics)]
  where
    camera = let k = startIntrinsics start in (focal k, cx k, cy k)

-- | A few frames of a head glancing about, with what the user did before
-- each: the presses and marks, when the frame was seen, its counter and
-- its stripes.
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

-- | Sessions that end sooner.
shrinkSession :: [a] -> [[a]]
shrinkSession session = [take k session | k <- [0 .. length session - 1]]
