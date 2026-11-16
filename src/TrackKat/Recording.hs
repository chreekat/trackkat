{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Recording what the tracker is given, frame by frame, to replay it
-- exactly: through the tracker as it was, in a test, or through a changed
-- one. See Note [Recording format].
module TrackKat.Recording (
    -- * Recordings
    Recording (..),
    Start (..),
    StartingCalibration (..),
    Entry (..),
    Input (..),
    renderStart,
    renderEntry,
    parseRecording,
    readRecording,

    -- * Time
    secondsSince,
    clockSeconds,
    trackerTime,

    -- * Trackers and their settings
    trackerFor,
    resettled,
    Tracking,
    trackingTracker,
    startTracking,
    startTracker,
    liveFrame,

    -- * What a session does
    startOfSession,
    Did (..),
    Tape,
    startTape,
    taped,

    -- * Replaying
    Replayed (..),
    replay,
) where

import Control.Exception (IOException, displayException, try)
import qualified Data.ByteString as B
import Data.ByteString.Builder (Builder, char7, intDec, integerDec, string7, stringUtf8, word8Dec)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char (intToDigit, isDigit, isSpace)
import Data.List (dropWhileEnd)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Word (Word64, Word8)

import TrackKat.Calibration (Calibration, defaultCalibration, parseCalibration)
import TrackKat.Config (Config (..), clipModel, parseConfig, settingValues)
import TrackKat.Frame (Stripe (..))
import TrackKat.Number (parseExact, parseNatural, renderExact)
import TrackKat.Pose (Intrinsics (..), defaultIntrinsics)
import TrackKat.Tracker
import TrackKat.World (Instant, clockAt)

-- Note [Recording format]
-- ~~~~~~~~~~~~~~~~~~~~~~~
-- A recording is text, one line per frame, so it can be read, diffed and
-- cut down by hand. It starts with what the tracker was made from:
--
--   trackkat recording 2
--   # tirprobe 0.1.142+3f9c2e1 --record problem.txt     (comments: notes)
--   camera 660.2235… 320.0 240.0     (focal length and centre, in pixels,
--                                     each in the fewest digits that read
--                                     back as the very same number)
--   setting clip = 70 110 110        (each of trackkat.conf's settings)
--   setting ignore lights = on
--   calibration up = 0 -0.9 0.4      (trackkat.calibration's lines; none
--                                     for a fresh calibration)
--   clock 81723349120417             (the monotonic clock, ns, at time 0)
--
-- and then, in the order the tracker took them, lines that start with a
-- time in seconds since the clock line, to the nanosecond:
--
--   12.345678901 frame 17 155,301,12,38210,4011 156,300,14,…
--   12.351234567 recenter
--   12.351234567 calibrate
--   12.351234567 mark 1
--   12.360000000 corrupt bad header checksum
--   12.400000000 clear calibration
--   12.400000000 setting clip = 70 105 105
--   12.400000000 note camera connected
--
-- A frame has the camera's frame counter and its stripes, each as line,
-- start, points, sum of brightness times x from the start, and sum of
-- brightness ('Stripe'): what the tracker was given, before blobs were
-- made of it, so that a change to finding blobs can be replayed too.
-- Presses (Enter, c Enter) apply before the frame after them, as they did
-- live; a mark is the user saying something just went wrong. Times are
-- whole nanoseconds, as the clock gives them, so the tracker is given the
-- same time on replay to the last bit.
--
-- The last three are what trackkat does that tirprobe doesn't, each
-- applying before the frame after it: the calibration cleared, to track
-- afresh from none; a setting saved with a new value, as trackkat.conf
-- wrote it; and a note, which changes nothing, as of the camera going
-- away and coming back. Version 1 had none of the three, and is read
-- still.
--
-- A new kind of line or header, or a change to one, needs a new version.

-- | The version of the format written.
formatVersion :: Int
formatVersion = 2

-- | The versions read.
versionsRead :: [Integer]
versionsRead = [1, 2]

-- | What the tracker was made from, and when the recording started.
data Start = Start
    { startNotes :: [String]
    -- ^ comments, such as what wrote the recording
    , startIntrinsics :: Intrinsics
    , startSettings :: [(String, String)]
    -- ^ trackkat.conf's settings, by name, as it writes them
    , startCalibration :: StartingCalibration
    , startClock :: Word64
    -- ^ nanoseconds on the monotonic clock that times count from
    }
    deriving (Eq, Show)

-- | What the tracker knew about level and pivot when it started.
data StartingCalibration
    = -- | nothing: 'defaultCalibration'
      FreshCalibration
    | -- | what a trackkat.calibration file says, in its own words
      SavedCalibration String
    deriving (Eq, Show)

-- | Something the user did, while tracking.
data Input
    = Press Request
    | -- | Something just went wrong; marks are numbered from 1.
      Mark Int
    | -- | The calibration cleared, to track afresh from none.
      ClearCalibration
    | -- | A setting saved with a new value: its name and the value, as
      -- trackkat.conf writes them.
      Setting String String
    deriving (Eq, Show)

-- | A line of a recording after its start. Times are nanoseconds on the
-- monotonic clock.
data Entry
    = -- | when the tracker was given a frame, its counter, and its stripes
      FrameEntry !Word64 !Word8 [Stripe]
    | -- | when the user did something; it applies before the next frame
      InputEntry !Word64 Input
    | -- | when a packet from the camera couldn't be read, and why
      CorruptEntry !Word64 String
    | -- | when something worth knowing happened that changes no tracking
      NoteEntry !Word64 String
    | -- | a line that couldn't be read, by number from 1, and why; as when
      -- the program writing it was killed. Nothing follows.
      Unreadable Int String
    deriving (Eq, Show)

-- | A session as recorded: how it started, and what happened, in order.
data Recording = Recording
    { recStart :: Start
    , recEntries :: [Entry]
    }
    deriving (Eq, Show)

-- | Nanoseconds on the monotonic clock in seconds, as
-- 'GHC.Clock.getMonotonicTime' tells them.
clockSeconds :: Word64 -> Double
clockSeconds ns = fromIntegral ns / 1e9

-- | The tracker's time for nanoseconds on the monotonic clock.
trackerTime :: Word64 -> Instant
trackerTime = clockAt . clockSeconds

-- | Seconds since a recording's start.
secondsSince :: Start -> Word64 -> Double
secondsSince s ns = fromIntegral (toInteger ns - toInteger (startClock s)) / 1e9

-- * Writing

-- | A recording's header. See Note [Recording format].
renderStart :: Start -> Builder
renderStart s =
    mconcat $
        [string7 "trackkat recording " <> intDec formatVersion <> nl]
            ++ [stringUtf8 ("# " ++ n) <> nl | n <- startNotes s]
            ++ [string7 "camera " <> string7 (unwords (map renderExact [focal k, cx k, cy k])) <> nl]
            ++ [stringUtf8 (settingLine n v) <> nl | (n, v) <- startSettings s]
            ++ [stringUtf8 ("calibration " ++ l) <> nl | l <- calibrationLines (startCalibration s)]
            ++ [string7 "clock " <> integerDec (toInteger (startClock s)) <> nl]
  where
    k = startIntrinsics s
    calibrationLines FreshCalibration = []
    calibrationLines (SavedCalibration text) = filter (not . blank) (map (dropWhileEnd isSpace . takeWhile (/= '#')) (lines text))
    blank = all isSpace

-- | An entry's line in a recording that started as given.
renderEntry :: Start -> Entry -> Builder
renderEntry s = \case
    FrameEntry t n stripes -> time t <> string7 " frame " <> word8Dec n <> foldMap stripe stripes <> nl
    InputEntry t (Press Recenter) -> time t <> string7 " recenter" <> nl
    InputEntry t (Press Calibrate) -> time t <> string7 " calibrate" <> nl
    InputEntry t (Mark m) -> time t <> string7 " mark " <> intDec m <> nl
    InputEntry t ClearCalibration -> time t <> string7 " clear calibration" <> nl
    InputEntry t (Setting n v) -> time t <> char7 ' ' <> stringUtf8 (oneLine (settingLine n v)) <> nl
    CorruptEntry t why -> time t <> string7 " corrupt " <> stringUtf8 (oneLine why) <> nl
    NoteEntry t text -> time t <> string7 " note " <> stringUtf8 (oneLine text) <> nl
    Unreadable n why -> stringUtf8 ("# line " ++ show n ++ ": " ++ oneLine why) <> nl
  where
    time t =
        let d = toInteger t - toInteger (startClock s)
            (whole, frac) = abs d `quotRem` 1000000000
            nanoseconds = [intToDigit (fromInteger (frac `quot` 10 ^ place `rem` 10)) | place <- [8, 7 .. 0 :: Int]]
         in (if d < 0 then char7 '-' else mempty) <> integerDec whole <> char7 '.' <> string7 nanoseconds
    stripe (Stripe line start points sumX total) =
        char7 ' ' <> intDec line <> char7 ',' <> intDec start <> char7 ',' <> intDec points <> char7 ',' <> intDec sumX <> char7 ',' <> intDec total
    oneLine = map (\c -> if c == '\n' || c == '\r' then ' ' else c)

nl :: Builder
nl = char7 '\n'

-- | A setting's line, in a recording's start or after it.
settingLine :: String -> String -> String
settingLine n v = "setting " ++ n ++ " =" ++ (if null v then "" else " " ++ v)

-- * Reading

-- | Read a recording. The start must be complete; a line after it that
-- can't be read, or is cut short, without the newline that ends every
-- line, or that gives a setting a value it can't have, ends the entries
-- with 'Unreadable'. Entries are read as
-- they are used, so a long recording can be replayed without holding it
-- all.
parseRecording :: BL.ByteString -> Either String Recording
parseRecording text = case zip [1 :: Int ..] (wholeLines text) of
    [] -> Left "empty: not a trackkat recording"
    (_, Nothing) : _ -> Left "cut short: not a trackkat recording"
    (_, Just first) : rest
        | ["trackkat", "recording", version] <- BL.words first
        , maybe False (`elem` versionsRead) (parseNatural (BL.unpack version)) ->
            header emptyHeader rest
        | take 2 (BL.words first) == ["trackkat", "recording"] ->
            Left ("a version " ++ BL.unpack (BL.unwords (drop 2 (BL.words first))) ++ " recording; this reads versions up to " ++ show formatVersion)
        | otherwise -> Left "not a trackkat recording: it should start with \"trackkat recording\""
  where
    header h [] = finish h []
    header _ ((n, Nothing) : _) = Left ("line " ++ show n ++ ": cut short")
    header h ls@((n, Just l) : rest) = case utf8 (BL.toStrict l) of
        ('#' : note) -> header h{hNotes = hNotes h ++ [dropWhile (== ' ') note]} rest
        str
            | all isSpace str -> header h rest
            | startsTimed str -> finish h ls
            | otherwise -> case break (== ' ') str of
                ("camera", spec)
                    | Just [f, x, y] <- traverse parseExact (words spec) -> header h{hIntrinsics = Just (Intrinsics f x y)} rest
                ("setting", spec)
                    | Just named <- parseSetting spec -> header h{hSettings = hSettings h ++ [named]} rest
                ("calibration", ' ' : line) -> header h{hCalibration = hCalibration h ++ [line]} rest
                ("clock", spec)
                    | Just c <- parseNatural (trim spec), c <= toInteger (maxBound :: Word64) -> header h{hClock = Just (fromInteger c)} rest
                _ -> Left ("line " ++ show n ++ ": not part of a recording's start: " ++ str)
    finish h ls = do
        k <- maybe (Left "no camera line") Right (hIntrinsics h)
        clock <- maybe (Left "no clock line") Right (hClock h)
        let start =
                Start
                    { startNotes = hNotes h
                    , startIntrinsics = k
                    , startSettings = hSettings h
                    , startCalibration = if null (hCalibration h) then FreshCalibration else SavedCalibration (unlines (hCalibration h))
                    , startClock = clock
                    }
        _ <- startTracker start
        pure (Recording start (entries clock (startSettings start) ls))
    startsTimed (c : _) = isDigit c || c == '-'
    startsTimed [] = False
    trim = dropWhileEnd isSpace . dropWhile isSpace
    -- The entries, given the settings as the lines before them leave them.
    entries _ _ [] = []
    entries _ _ ((n, Nothing) : _) = [Unreadable n "cut short"]
    entries clock settings ((n, Just l) : rest)
        | BL.all isSpace l || BL.isPrefixOf "#" l = entries clock settings rest
        | otherwise = case parseEntry clock (BL.toStrict l) of
            Right e@(InputEntry _ (Setting name value)) -> case parseSettings (set name value settings) of
                Right _ -> e : entries clock (set name value settings) rest
                Left why -> [Unreadable n why]
            Right e -> e : entries clock settings rest
            Left why -> [Unreadable n why]

-- | A text's lines, each but one cut short at the end of the text.
wholeLines :: BL.ByteString -> [Maybe BL.ByteString]
wholeLines text
    | BL.null text = []
    | otherwise = case BL.break (== '\n') text of
        (l, rest)
            | BL.null rest -> [Nothing]
            | otherwise -> Just (dropCR l) : wholeLines (BL.drop 1 rest)
  where
    -- Lines ended Windows' way, as by an editor.
    dropCR l = if not (BL.null l) && BL.last l == '\r' then BL.init l else l

data Header = Header
    { hNotes :: [String]
    , hIntrinsics :: Maybe Intrinsics
    , hSettings :: [(String, String)]
    , hCalibration :: [String]
    , hClock :: Maybe Word64
    }

emptyHeader :: Header
emptyHeader = Header [] Nothing [] [] Nothing

-- | One timed line, given the start's clock.
parseEntry :: Word64 -> B.ByteString -> Either String Entry
parseEntry clock line = do
    let (timeText, rest) = B8.break (== ' ') line
    t <- maybe (Left ("bad time " ++ show (B8.unpack timeText))) Right (parseTime timeText)
    let at = fromInteger (toInteger clock + t)
    case B8.words rest of
        "frame" : counter : stripes -> do
            n <- case B8.readInt counter of
                Just (c, "") | c >= 0, c <= 255 -> Right (fromIntegral c)
                _ -> Left "bad frame counter"
            FrameEntry at n <$> traverse parseStripe stripes
        ["recenter"] -> Right (InputEntry at (Press Recenter))
        ["calibrate"] -> Right (InputEntry at (Press Calibrate))
        ["mark", m] | Just (k, "") <- B8.readInt m -> Right (InputEntry at (Mark k))
        ["clear", "calibration"] -> Right (InputEntry at ClearCalibration)
        "setting" : _ | Just (name, value) <- parseSetting (utf8 (afterWord rest)) -> Right (InputEntry at (Setting name value))
        "corrupt" : _ -> Right (CorruptEntry at (utf8 (afterWord rest)))
        "note" : _ -> Right (NoteEntry at (utf8 (afterWord rest)))
        _ -> Left ("unknown line: " ++ B8.unpack (B8.take 40 line))
  where
    -- What follows the first word, and a space, of a line after its time.
    afterWord = B8.drop 1 . B8.dropWhile (/= ' ') . B8.dropWhile (== ' ')
    parseStripe s = case traverse B8.readInt (B8.split ',' s) of
        Just [(a, ""), (b, ""), (c, ""), (d, ""), (e, "")] -> Right (Stripe a b c d e)
        _ -> Left ("bad stripe " ++ show (B8.unpack s))

-- | A setting's name and value from what follows @setting@ on its line.
parseSetting :: String -> Maybe (String, String)
parseSetting spec = case break (== '=') spec of
    (name, '=' : value) -> Just (trim name, trim value)
    _ -> Nothing
  where
    trim = dropWhileEnd isSpace . dropWhile isSpace

-- | Nanoseconds from seconds to the nanosecond, like @12.345678901@.
parseTime :: B.ByteString -> Maybe Integer
parseTime str = case B8.uncons str of
    Just ('-', rest) -> negate <$> parseTime rest
    _ -> case B8.split '.' str of
        [whole, frac]
            | B.length frac == 9 ->
                (\seconds nanoseconds -> seconds * 1000000000 + nanoseconds) <$> parseNatural (B8.unpack whole) <*> parseNatural (B8.unpack frac)
        _ -> Nothing

-- | Read a recording from a file, unless it can't be opened.
readRecording :: FilePath -> IO (Either String Recording)
readRecording path = either (Left . displayException @IOException) parseRecording <$> try (BL.readFile path)

-- * Tracking

-- | A tracker of the clip settings give, masking still lights or not as
-- they say, starting from a calibration.
trackerFor :: Intrinsics -> Config -> Calibration -> Tracker
trackerFor k config = newTracker k (clipModel (clip config)) (lightMasking config)

-- | A tracker taken from tracking by some settings to tracking by others.
resettled :: Config -> Config -> Tracker -> Tracker
resettled old new = reshaped . masked
  where
    masked
        | lightMasking new /= lightMasking old = setMasking (lightMasking new)
        | otherwise = id
    reshaped
        | clip new /= clip old = setModel (clipModel (clip new))
        | otherwise = id

-- | A tracker with the camera and the settings it tracks by: what a
-- recording's entries are carried out on.
data Tracking = Tracking
    { trackingCamera :: Intrinsics
    , trackingSettings :: [(String, String)]
    -- ^ trackkat.conf's settings, by name, as last given
    , trackingConfig :: Config
    -- ^ what those settings say
    , trackingTracker :: !Tracker
    }

-- | The settings that names and values give, as trackkat.conf writes them.
parseSettings :: [(String, String)] -> Either String Config
parseSettings settings = either (Left . ("settings: " ++)) Right (parseConfig (unlines [n ++ " = " ++ v | (n, v) <- settings]))

-- | Settings with one given a value, at the end if it wasn't among them.
set :: String -> String -> [(String, String)] -> [(String, String)]
set name value settings
    | any ((== name) . fst) settings = [(n, if n == name then value else v) | (n, v) <- settings]
    | otherwise = settings ++ [(name, value)]

-- | Carry out something the user did. A setting given a value it can't
-- have changes nothing.
applyInput :: Input -> Tracking -> Tracking
applyInput input t = case input of
    Press r -> t{trackingTracker = request r (trackingTracker t)}
    Mark _ -> t
    ClearCalibration -> t{trackingTracker = trackerFor (trackingCamera t) (trackingConfig t) defaultCalibration}
    Setting name value ->
        let settings = set name value (trackingSettings t)
         in case parseSettings settings of
                Right config -> t{trackingSettings = settings, trackingConfig = config, trackingTracker = resettled (trackingConfig t) config (trackingTracker t)}
                Left _ -> t

-- | Tracking as a recording's start describes it, unless its settings or
-- calibration can't be read.
startTracking :: Start -> Either String Tracking
startTracking s = do
    config <- parseSettings (startSettings s)
    calibration <- case startCalibration s of
        FreshCalibration -> Right defaultCalibration
        SavedCalibration text -> either (Left . ("calibration: " ++)) Right (parseCalibration text)
    pure (Tracking (startIntrinsics s) (startSettings s) config (trackerFor (startIntrinsics s) config calibration))

-- | A tracker as a recording's start describes it, unless its settings or
-- calibration can't be read.
startTracker :: Start -> Either String Tracker
startTracker = fmap trackingTracker . startTracking

-- | Carry out what the user did since the last frame, then track a frame
-- seen at the given time on the monotonic clock, with its counter: the
-- entries that say so, which 'replay' repeats, and what tracking comes to.
liveFrame :: [(Word64, Input)] -> Word64 -> Word8 -> [Stripe] -> Tracking -> ([Entry], (Tracking, Outcome, [Event]))
liveFrame inputs at n stripes t =
    ( [InputEntry pressed i | (pressed, i) <- inputs] ++ [FrameEntry at n stripes]
    , (given{trackingTracker = t'}, outcome, events)
    )
  where
    given = foldl (flip applyInput) t (map snd inputs)
    (t', outcome, events) = step (trackerTime at) stripes (trackingTracker given)

-- * What a session does

-- | How the recording of a session of trackkat starts, the clock telling
-- the time given: with some notes, the camera trackkat takes it to be,
-- the settings as the settings file's text gives them, and, if the
-- session starts from what a calibration file says, that file's text.
startOfSession :: [String] -> String -> Maybe String -> Word64 -> Start
startOfSession notes settings calibration clock =
    Start
        { startNotes = notes
        , startIntrinsics = defaultIntrinsics
        , startSettings = settingValues settings
        , startCalibration = maybe FreshCalibration SavedCalibration calibration
        , startClock = clock
        }

-- | Something a session of trackkat did with its tracker, or that
-- happened to it, for a recording to say.
data Did
    = -- | gave the tracker a frame
      GaveFrame [Stripe]
    | -- | asked the tracker for something
      Requested Request
    | -- | cleared the calibration, tracking afresh from none
      ClearedCalibration
    | -- | took up the settings file as saved: its text
      TookSettings String
    | -- | was asked to mark the moment, the marks numbered from 1
      Marked Int
    | -- | dropped a packet from the camera as corrupt, and why
      DroppedCorrupt String
    | -- | something worth a note that changes no tracking
      Noted String
    deriving (Eq, Show)

-- | The settings as a recording of a session last said them.
newtype Tape = Tape [(String, String)]

-- | A recording that has said only its start.
startTape :: Start -> Tape
startTape = Tape . startSettings

-- | The entries that say what a session did at a time on the monotonic
-- clock, a frame having the given counter: settings are said just where
-- the file gives them another value than the recording last said.
taped :: Word64 -> Word8 -> Did -> Tape -> (Tape, [Entry])
taped at n did tape@(Tape said) = case did of
    GaveFrame stripes -> (tape, [FrameEntry at n stripes])
    Requested r -> (tape, [InputEntry at (Press r)])
    ClearedCalibration -> (tape, [InputEntry at ClearCalibration])
    TookSettings text ->
        let now = settingValues text
         in (Tape now, [InputEntry at (Setting name value) | (name, value) <- now, lookup name said /= Just value])
    Marked m -> (tape, [InputEntry at (Mark m)])
    DroppedCorrupt why -> (tape, [CorruptEntry at why])
    Noted text -> (tape, [NoteEntry at text])

-- * Replaying

-- | An entry replayed, and for a frame, the tracker after it, what the
-- frame showed, and what happened.
data Replayed = Replayed Entry (Maybe (Tracker, Outcome, [Event]))

-- | Replay a recording through the tracker, as it was tracked live,
-- unless its start can't be read.
replay :: Recording -> Either String [Replayed]
replay r = (\t0 -> from t0 (recEntries r)) <$> startTracking (recStart r)
  where
    from _ [] = []
    from t (e : es) = case e of
        FrameEntry at _ stripes ->
            let (t', o, evs) = step (trackerTime at) stripes (trackingTracker t)
             in t' `seq` Replayed e (Just (t', o, evs)) : from t{trackingTracker = t'} es
        InputEntry _ i -> Replayed e Nothing : from (applyInput i t) es
        _ -> Replayed e Nothing : from t es

-- | Text written as UTF-8, as notes, settings and reasons are.
utf8 :: B.ByteString -> String
utf8 = T.unpack . decodeUtf8Lenient
