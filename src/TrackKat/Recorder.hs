{-# LANGUAGE LambdaCase #-}

-- | Writing a recording (see "TrackKat.Recording") while tracking, without
-- holding tracking up.
--
-- See Note [Recording never holds up tracking].
module TrackKat.Recorder (
    -- * Recorders
    Recorder,
    record,
    finish,
    soFar,
    Ending (..),
    notRecording,
    withRecorder,
    describeRecording,
    describeUnrecorded,
    describeStopped,
    describeRecorded,

    -- * Where entries go
    Sink (..),
    withRecorderTo,
    backlog,

    -- * Recording tirprobe
    trackLive,

    -- * Recording a session
    SessionRecorder,
    sessionRecorder,
    newSessionRecorder,
    recordStep,
    describeMark,

    -- * Files
    recordingName,
) where

import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Exception (IOException, catch, displayException, finally, mask, try)
import Data.ByteString.Builder (Builder, hPutBuilder)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.List (mapAccumL)
import Data.Word (Word64, Word8)
import System.IO (BufferMode (..), IOMode (..), hClose, hFlush, hSetBuffering, openBinaryFile)

import TrackKat.Frame (Stripe)
import TrackKat.Number (renderNatural)
import TrackKat.Recording
import TrackKat.Trackers (Event, Outcome)

-- Note [Recording never holds up tracking]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Whoever tracks hands each entry to a queue and goes on; a thread of the
-- recorder's own turns entries into text and writes them. So tracking
-- never waits for the disk, and a recording costs it a little memory: the
-- entries not yet written.
--
-- That is kept to 'backlog' entries. A disk that falls further behind
-- than that, or that fails, as when it is full, ends the recording there:
-- whoever started it is told why, once, and entries handed over from then
-- on are dropped. Tracking goes on as if nothing were being recorded.
--
-- What is written goes to the file at each mark, and each second, so
-- that a recording whose program is killed lacks only its last moments;
-- 'parseRecording' reads one cut short up to its last whole line.

-- | Something to give each entry to, to record it, or not.
data Recorder = Recorder
    { record :: Entry -> IO ()
    -- ^ Hand an entry over to be written; this doesn't wait for the file.
    , finish :: IO Ending
    -- ^ Write what was handed over and close the recording, waiting for
    -- the file; once finished, it stays as it ended.
    , soFar :: IO Ending
    -- ^ How the recording stands.
    }

-- | How a recording stands, or ended: with all it was given, or short of
-- that.
data Ending = Whole | Partial
    deriving (Eq, Show)

-- | Records nothing.
notRecording :: Recorder
notRecording = Recorder (const (pure ())) (pure Whole) (pure Whole)

-- | Where a recording's text goes.
data Sink = Sink
    { sinkWrite :: Builder -> IO ()
    , sinkFlush :: IO ()
    -- ^ see that what was written is where it is going
    , sinkClose :: IO ()
    -- ^ that, and let go of it
    }

-- | How many entries may wait to be written: about twenty seconds of
-- frames, some megabytes, which is longer than a disk takes to wake up.
backlog :: Int
backlog = 2400

-- | Whether a recorder is taking entries, this many of them waiting to be
-- written, or is over.
data Taking = Taking !Int | Over Ending

-- | What to do with an entry handed over.
data Verdict = Take | Overrun | Drop

-- | Record to a file, replacing it, starting with the given start: the
-- action is given a recorder, or why the file can't be written to. If the
-- recording stops before it is finished, the first function is told why,
-- once. See Note [Recording never holds up tracking]. The recording is
-- finished however the action exits.
withRecorder :: FilePath -> Start -> (String -> IO ()) -> (Either String Recorder -> IO a) -> IO a
withRecorder path start stopped act = mask $ \restore ->
    try (openBinaryFile path WriteMode) >>= \case
        Left e -> restore (act (Left (displayException (e :: IOException))))
        Right h ->
            restore (hSetBuffering h (BlockBuffering (Just 65536)) >> withRecorderTo (Sink (hPutBuilder h) (hFlush h) (hClose h)) start stopped (act . Right))
                `finally` (hClose h `catch` \e -> const (pure ()) (e :: IOException))

-- | Record to a sink, as 'withRecorder' does to a file.
withRecorderTo :: Sink -> Start -> (String -> IO ()) -> (Recorder -> IO a) -> IO a
withRecorderTo sink start stopped act = do
    entries <- newChan
    taking <- newIORef (Taking 0)
    ended <- newMVar Nothing
    let
        -- Take no more entries, the recording ending as given unless it
        -- is over: how it ended.
        end ending = atomicModifyIORef' taking $ \case
            Taking _ -> (Over ending, ending)
            Over already -> (Over already, already)
        failed e = do
            was <- atomicModifyIORef' taking (\t -> (Over Partial, t))
            case was of
                Taking _ -> stopped (displayException (e :: IOException))
                Over _ -> pure ()
        written = atomicModifyIORef' taking $ \case
            Taking n -> (Taking (n - 1), ())
            over -> (over, ())
        writer = (sinkWrite sink (renderStart start) >> sinkFlush sink >> write (startClock start)) `catch` failed
        -- Write entries until there are no more, given when what was
        -- written last went to the sink.
        write flushed =
            readChan entries >>= \case
                Nothing -> sinkFlush sink
                Just e -> do
                    sinkWrite sink (renderEntry start e)
                    written
                    case entryTime e of
                        Just at | isMark e || at >= flushed + 1000000000 -> sinkFlush sink >> write at
                        _ -> write flushed
        hand e = do
            verdict <- atomicModifyIORef' taking $ \case
                Taking n
                    | n < backlog -> (Taking (n + 1), Take)
                    | otherwise -> (Over Partial, Overrun)
                over -> (over, Drop)
            case verdict of
                Take -> writeChan entries (Just e)
                Overrun -> do
                    mapM_ (\at -> writeChan entries (Just (NoteEntry at "the recording stops here: the disk couldn't keep up"))) (entryTime e)
                    stopped "the disk can't keep up"
                Drop -> pure ()
    withAsync writer $ \w -> do
        let finishing = modifyMVar ended $ \case
                Just ending -> pure (Just ending, ending)
                Nothing -> do
                    writeChan entries Nothing
                    wait w
                    sinkClose sink `catch` failed
                    ending <- end Whole
                    pure (Just ending, ending)
            standing =
                readIORef taking >>= \case
                    Taking _ -> pure Whole
                    Over ending -> pure ending
        act (Recorder hand finishing standing) `finally` finishing
  where
    isMark (InputEntry _ (Mark _)) = True
    isMark _ = False

-- | When an entry happened, if it says.
entryTime :: Entry -> Maybe Word64
entryTime = \case
    FrameEntry at _ _ -> Just at
    InputEntry at _ -> Just at
    CorruptEntry at _ -> Just at
    NoteEntry at _ -> Just at
    Unreadable _ _ -> Nothing

-- | The log's line for a recording begun, to the file given.
describeRecording :: FilePath -> String
describeRecording path = "Recording to " ++ path ++ ": about 6 MB a minute, until trackkat quits."

-- | The log's line for a recording that couldn't be begun, and why.
describeUnrecorded :: String -> String
describeUnrecorded why = "Can't record: " ++ why ++ ". Tracking goes on."

-- | The log's line for a recording finished whole, in the file given.
describeRecorded :: FilePath -> String
describeRecorded path = "Recording saved to " ++ path ++ "."

-- | The log's line for a recording that stopped before it was finished,
-- and why.
describeStopped :: String -> String
describeStopped why = "Recording stopped: " ++ why ++ ". Tracking goes on."

-- | Track a frame live (see 'liveFrame'), recording it.
trackLive :: Recorder -> [(Word64, Input)] -> Word64 -> Word8 -> [Stripe] -> Tracking -> IO (Tracking, Outcome, [Event])
trackLive rec inputs at n stripes t = do
    let (entries, tracked) = liveFrame inputs at n stripes t
    mapM_ (record rec) entries
    pure tracked

-- | A recorder of a session of trackkat, whose threads each take the
-- session through what happens to it.
data SessionRecorder = SessionRecorder
    { sessionTape :: MVar Tape
    , sessionRecorder :: Recorder
    }

-- | Record a session whose recording starts as given.
newSessionRecorder :: Start -> Recorder -> IO SessionRecorder
newSessionRecorder start rec = (`SessionRecorder` rec) <$> newMVar (startTape start)

-- | Take a step of a session, which says what it did, and record that as
-- done at the given time on the monotonic clock, a frame having the given
-- counter. Steps are recorded in the order they are taken, whichever
-- threads take them: one waits for another's to be taken and handed over,
-- never for the file.
recordStep :: SessionRecorder -> Word64 -> Word8 -> IO (a, [Did]) -> IO a
recordStep r at n step = modifyMVar (sessionTape r) $ \said -> do
    (result, did) <- step
    let (said', entries) = mapAccumL (flip (taped at n)) said did
    mapM_ (record (sessionRecorder r)) (concat entries)
    pure (said', result)

-- | The log's line for a mark asked for: its number if it is recorded, as
-- when a recording was begun and has not stopped.
describeMark :: Maybe Ending -> Int -> String
describeMark recording n = case recording of
    Just Whole -> "Mark " ++ renderNatural (toInteger n) ++ " recorded."
    Just Partial -> "No mark: the recording has stopped."
    Nothing -> "No mark: start trackkat with --record to record."

-- | The name of a recording begun at a local date, as year, month and day,
-- and time, as hours, minutes and seconds: one that sorts by when, and
-- that any file system takes.
recordingName :: (Integer, Int, Int) -> (Int, Int, Int) -> FilePath
recordingName (year, month, day) (hours, minutes, secs) =
    "recording-" ++ renderNatural year ++ "-" ++ two month ++ "-" ++ two day ++ "-" ++ two hours ++ two minutes ++ two secs ++ ".txt"
  where
    two n = let digits = renderNatural (toInteger (max 0 n)) in replicate (2 - length digits) '0' ++ digits
