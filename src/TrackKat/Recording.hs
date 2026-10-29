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
    formatVersion,
    renderStart,
    renderEntry,
    parseRecording,
    readRecording,

    -- * Time
    trackerTime,
    secondsSince,

    -- * Tracking live
    applyInput,
    startTracker,
    Recorder,
    notRecording,
    record,
    withRecorder,
    trackLive,

    -- * Replaying
    Replayed (..),
    replay,
) where

import Control.Concurrent.Async (link, wait, withAsync)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Exception (IOException, displayException, finally, try)
import Control.Monad (when)
import qualified Data.ByteString as B
import Data.ByteString.Builder (Builder, char7, hPutBuilder, intDec, integerDec, string7, stringUtf8, word8Dec)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char (isDigit, isSpace)
import Data.List (dropWhileEnd)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Word (Word64, Word8)
import System.IO (BufferMode (..), IOMode (..), hFlush, hSetBinaryMode, hSetBuffering, withFile)
import Text.Read (readMaybe)

import TrackKat.Calibration (defaultCalibration, parseCalibration)
import TrackKat.Config (Config (..), clipModel, parseConfig)
import TrackKat.Frame (Stripe (..))
import TrackKat.Pose (Intrinsics (..))
import TrackKat.Tracker

-- Note [Recording format]
-- ~~~~~~~~~~~~~~~~~~~~~~~
-- A recording is text, one line per frame, so it can be read, diffed and
-- cut down by hand. It starts with what the tracker was made from:
--
--   trackkat recording 1
--   # tirprobe 0.1.142+3f9c2e1 --record problem.txt     (comments: notes)
--   camera 660.2235… 320.0 240.0     (focal length and centre, in pixels)
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
-- A new kind of line or header, or a change to one, needs a new version.

-- | The version of the format written, and the only one read.
formatVersion :: Int
formatVersion = 1

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
    deriving (Show)

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
    | -- | a line that couldn't be read, by number from 1, and why; as when
      -- the program writing it was killed. Nothing follows.
      Unreadable Int String
    deriving (Eq, Show)

data Recording = Recording
    { recStart :: Start
    , recEntries :: [Entry]
    }
    deriving (Show)

-- | The tracker's time, in seconds, for nanoseconds on the monotonic clock,
-- as 'GHC.Clock.getMonotonicTime' gives it.
trackerTime :: Word64 -> Double
trackerTime ns = fromIntegral ns / 1e9

-- | Seconds since a recording's start.
secondsSince :: Start -> Word64 -> Double
secondsSince s ns = fromIntegral (toInteger ns - toInteger (startClock s)) / 1e9

-- * Writing

renderStart :: Start -> Builder
renderStart s =
    mconcat $
        [string7 "trackkat recording " <> intDec formatVersion <> nl]
            ++ [stringUtf8 ("# " ++ n) <> nl | n <- startNotes s]
            ++ [string7 "camera " <> string7 (unwords (map show [focal k, cx k, cy k])) <> nl]
            ++ [stringUtf8 ("setting " ++ n ++ " =" ++ (if null v then "" else " " ++ v)) <> nl | (n, v) <- startSettings s]
            ++ [stringUtf8 ("calibration " ++ l) <> nl | l <- calibrationLines (startCalibration s)]
            ++ [string7 "clock " <> integerDec (toInteger (startClock s)) <> nl]
  where
    k = startIntrinsics s
    calibrationLines FreshCalibration = []
    calibrationLines (SavedCalibration text) = filter (not . blank) (map (dropWhileEnd isSpace . takeWhile (/= '#')) (lines text))
    blank = all isSpace

renderEntry :: Start -> Entry -> Builder
renderEntry s = \case
    FrameEntry t n stripes -> time t <> string7 " frame " <> word8Dec n <> foldMap stripe stripes <> nl
    InputEntry t (Press Recenter) -> time t <> string7 " recenter" <> nl
    InputEntry t (Press Calibrate) -> time t <> string7 " calibrate" <> nl
    InputEntry t (Mark m) -> time t <> string7 " mark " <> intDec m <> nl
    CorruptEntry t why -> time t <> string7 " corrupt " <> stringUtf8 (oneLine why) <> nl
    Unreadable n why -> stringUtf8 ("# line " ++ show n ++ ": " ++ oneLine why) <> nl
  where
    time t =
        let d = toInteger t - toInteger (startClock s)
            (whole, frac) = abs d `quotRem` 1000000000
            digits = show frac
         in (if d < 0 then char7 '-' else mempty) <> integerDec whole <> char7 '.' <> string7 (replicate (9 - length digits) '0' ++ digits)
    stripe (Stripe line start points sumX total) =
        char7 ' ' <> intDec line <> char7 ',' <> intDec start <> char7 ',' <> intDec points <> char7 ',' <> intDec sumX <> char7 ',' <> intDec total
    oneLine = map (\c -> if c == '\n' || c == '\r' then ' ' else c)

nl :: Builder
nl = char7 '\n'

-- | Something to give each entry to, to record it, or not.
newtype Recorder = Recorder
    { record :: Entry -> IO ()
    -- ^ Hand an entry over to be written; this doesn't wait for the file.
    }

-- | Records nothing.
notRecording :: Recorder
notRecording = Recorder (const (pure ()))

-- | Record to a file, replacing it, starting with the given start: the
-- action is given a recorder. Entries are written, buffered, by a thread
-- of their own, so that recording doesn't hold up the caller; the file is
-- flushed at each mark, and complete however the action exits.
withRecorder :: FilePath -> Start -> (Recorder -> IO a) -> IO a
withRecorder path start act = withFile path WriteMode $ \h -> do
    hSetBinaryMode h True
    hSetBuffering h (BlockBuffering (Just 65536))
    hPutBuilder h (renderStart start)
    hFlush h
    entries <- newChan
    let writer =
            readChan entries >>= \case
                Nothing -> hFlush h
                Just e -> do
                    hPutBuilder h (renderEntry start e)
                    when (isMark e) (hFlush h)
                    writer
    withAsync writer $ \w -> do
        link w
        act (Recorder (writeChan entries . Just)) `finally` (writeChan entries Nothing >> wait w)
  where
    isMark (InputEntry _ (Mark _)) = True
    isMark _ = False

-- * Reading

-- | Read a recording. The start must be complete; a line after it that
-- can't be read, or is cut short, without the newline that ends every
-- line, ends the entries with 'Unreadable'. Entries are read as
-- they are used, so a long recording can be replayed without holding it
-- all.
parseRecording :: BL.ByteString -> Either String Recording
parseRecording text = case zip [1 :: Int ..] (wholeLines text) of
    [] -> Left "empty: not a trackkat recording"
    (_, Nothing) : _ -> Left "cut short: not a trackkat recording"
    (_, Just first) : rest
        | BL.words first == ["trackkat", "recording", BL.pack (show formatVersion)] -> header emptyHeader rest
        | take 2 (BL.words first) == ["trackkat", "recording"] ->
            Left ("a version " ++ BL.unpack (BL.unwords (drop 2 (BL.words first))) ++ " recording; this reads version " ++ show formatVersion)
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
                    | Just [f, x, y] <- traverse readMaybe (words spec) -> header h{hIntrinsics = Just (Intrinsics f x y)} rest
                ("setting", spec)
                    | (name, '=' : value) <- break (== '=') spec -> header h{hSettings = hSettings h ++ [(trim name, trim value)]} rest
                ("calibration", ' ' : line) -> header h{hCalibration = hCalibration h ++ [line]} rest
                ("clock", spec)
                    | Just c <- readMaybe spec -> header h{hClock = Just c} rest
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
        pure (Recording start (entries clock ls))
    startsTimed (c : _) = isDigit c || c == '-'
    startsTimed [] = False
    trim = dropWhileEnd isSpace . dropWhile isSpace
    entries _ [] = []
    entries _ ((n, Nothing) : _) = [Unreadable n "cut short"]
    entries clock ((n, Just l) : rest)
        | BL.all isSpace l || BL.isPrefixOf "#" l = entries clock rest
        | otherwise = case parseEntry clock (BL.toStrict l) of
            Right e -> e : entries clock rest
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
        "corrupt" : _ -> Right (CorruptEntry at (utf8 (B8.drop 1 (B8.dropWhile (/= ' ') (B8.dropWhile (== ' ') rest)))))
        _ -> Left ("unknown line: " ++ B8.unpack (B8.take 40 line))
  where
    parseStripe s = case traverse B8.readInt (B8.split ',' s) of
        Just [(a, ""), (b, ""), (c, ""), (d, ""), (e, "")] -> Right (Stripe a b c d e)
        _ -> Left ("bad stripe " ++ show (B8.unpack s))

-- | Nanoseconds from seconds to the nanosecond, like @12.345678901@.
parseTime :: B.ByteString -> Maybe Integer
parseTime str = case B8.uncons str of
    Just ('-', rest) -> negate <$> parseTime rest
    _ -> case B8.split '.' str of
        [whole, frac]
            | not (B.null whole)
            , B8.all isDigit whole
            , B.length frac == 9
            , B8.all isDigit frac ->
                Just (read (B8.unpack whole) * 1000000000 + read (B8.unpack frac))
        _ -> Nothing

-- | Read a recording from a file, unless it can't be opened.
readRecording :: FilePath -> IO (Either String Recording)
readRecording path = either (Left . displayException @IOException) parseRecording <$> try (BL.readFile path)

-- * Tracking

-- | Carry out something the user did.
applyInput :: Input -> Tracker -> Tracker
applyInput (Press r) = request r
applyInput (Mark _) = id

-- | A tracker as a recording's start describes it, unless its settings or
-- calibration can't be read.
startTracker :: Start -> Either String Tracker
startTracker s = do
    config <- either (Left . ("settings: " ++)) Right (parseConfig (unlines [n ++ " = " ++ v | (n, v) <- startSettings s]))
    calibration <- case startCalibration s of
        FreshCalibration -> Right defaultCalibration
        SavedCalibration text -> either (Left . ("calibration: " ++)) Right (parseCalibration text)
    pure (newTracker (startIntrinsics s) (clipModel (clip config)) (lightMasking config) calibration)

-- | Carry out what the user did since the last frame, then track a frame
-- seen at the given time on the monotonic clock, recording both: what
-- tracking live does, and what 'replay' repeats.
trackLive :: Recorder -> [(Word64, Input)] -> Word64 -> Word8 -> [Stripe] -> Tracker -> IO (Tracker, Outcome, [Event])
trackLive rec inputs at n stripes t = do
    mapM_ (\(pressed, i) -> record rec (InputEntry pressed i)) inputs
    record rec (FrameEntry at n stripes)
    pure (step (trackerTime at) stripes (foldl (flip applyInput) t (map snd inputs)))

-- * Replaying

-- | An entry replayed, and for a frame, the tracker after it, what the
-- frame showed, and what happened.
data Replayed = Replayed Entry (Maybe (Tracker, Outcome, [Event]))

-- | Replay a recording through the tracker, as 'trackLive' tracked it,
-- unless its start can't be read.
replay :: Recording -> Either String [Replayed]
replay r = (\t0 -> from t0 (recEntries r)) <$> startTracker (recStart r)
  where
    from _ [] = []
    from t (e : es) = case e of
        FrameEntry at _ stripes ->
            let (t', o, evs) = step (trackerTime at) stripes t
             in t' `seq` Replayed e (Just (t', o, evs)) : from t' es
        InputEntry _ i -> Replayed e Nothing : from (applyInput i t) es
        _ -> Replayed e Nothing : from t es

-- | Text written as UTF-8, as notes, settings and reasons are.
utf8 :: B.ByteString -> String
utf8 = T.unpack . decodeUtf8Lenient
