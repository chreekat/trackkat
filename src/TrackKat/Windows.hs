{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- | trackkat.exe's Windows plumbing: see cbits/windows.c, cbits/trackir.c
-- and cbits/joystick.c.
module TrackKat.Windows (
    ConsoleOutput (..),
    consoleOutput,
    openShared,
    publish,
    recenterCount,
    gamesTracking,
    trackingGameName,
    singleInstance,
    setNPClientPath,
    findTrackIRFolder,
    StandInProblem (..),
    startTrackIRStandIn,
    takeTrackIRGame,
    scrambledGameName,
    trackIRServed,
    gameProcess,
    windowProcess,
    Watched (..),
    startHotkeys,
    rebindHotkeys,
    takeKeyEdges,
    JoystickEvent (..),
    takeJoystickEvents,
) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.List (isSuffixOf)
import Data.Word (Word64)
import Foreign.C.String (CString, CWString, peekCString, peekCWString, withCWString)
import Foreign.C.Types (CDouble (..), CFloat (..), CInt (..), CLong (..), CULLong (..), CULong (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray, withArray, withArrayLen)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)

import TrackKat.Config (Joystick (..), KeyOrButton (..), keyFromCodes, modifierMask, virtualKey)
import TrackKat.Controls (JoystickEvent (..), KeyEdge (..))
import TrackKat.GamePose
import TrackKat.Gesture (Edge (..))
import TrackKat.ServedGame
import TrackKat.World (Instant, seconds, (.+^))

foreign import ccall unsafe "hts_console_output" c_console_output :: IO CInt
foreign import ccall unsafe "hts_shared_open" c_shared_open :: IO CInt
foreign import ccall unsafe "hts_publish" c_publish :: CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> IO ()
foreign import ccall unsafe "hts_recenter_count" c_recenter_count :: IO CLong
foreign import ccall unsafe "hts_games_tracking" c_games_tracking :: IO CInt
foreign import ccall safe "hts_tracking_game_name" c_tracking_game_name :: CWString -> CInt -> IO CInt
foreign import ccall unsafe "hts_single_instance" c_single_instance :: IO CInt
foreign import ccall safe "hts_set_npclient_path" c_set_npclient_path :: CWString -> IO CInt
foreign import ccall safe "hts_find_trackir_dir" c_find_trackir_dir :: CWString -> CInt -> IO CInt
foreign import ccall safe "hts_trackir_start" c_trackir_start :: CULong -> CULLong -> CULLong -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "hts_trackir_served"
    c_trackir_served :: Ptr CULong -> Ptr CULLong -> Ptr CULLong -> Ptr CInt -> Ptr CInt -> IO CLong
foreign import ccall unsafe "hts_process_started" c_process_started :: CULong -> Ptr CULLong -> IO CInt
foreign import ccall unsafe "hts_window_process" c_window_process :: CULLong -> IO CULong
foreign import ccall unsafe "hts_trackir_take_game" c_trackir_take_game :: IO CInt
foreign import ccall unsafe "hts_scrambled_game" c_scrambled_game :: CInt -> IO CString
foreign import ccall unsafe "hts_watch" c_watch :: CInt -> Ptr CInt -> Ptr CWString -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall safe "hts_hotkeys_start" c_hotkeys_start :: Ptr CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "hts_key_edge"
    c_key_edge :: Ptr CInt -> CWString -> CInt -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO CInt
foreign import ccall unsafe "hts_joystick_event" c_joystick_event :: CWString -> CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- | Where what trackkat.exe prints goes. A GUI program, it has no console
-- of its own.
data ConsoleOutput
    = -- | the file or pipe it was started with, as when a terminal captures
      -- or pipes its output
      OutputRedirected
    | -- | the console of the terminal it was started from
      ParentConsole
    | -- | nowhere, as when started from the Start menu, at login or by a
      -- game; or, in a console program, its own console
      NoConsole
    deriving (Eq, Show)

-- | Where standard output goes. Before main, cbits/windows.c points
-- standard output and error at the terminal trackkat.exe was started from,
-- if they aren't a file or pipe, or else at nothing, so they are always
-- safe to write to.
consoleOutput :: IO ConsoleOutput
consoleOutput =
    c_console_output >>= \case
        0 -> pure OutputRedirected
        1 -> pure ParentConsole
        _ -> pure NoConsole

-- | Create the shared memory games read from. Left is a Windows error code.
openShared :: IO (Either Int ())
openShared = (\rc -> if rc == 0 then Right () else Left (fromIntegral rc)) <$> c_shared_open

-- | Give games this pose, through the memory NPClient reads.
publish :: GamePose -> IO ()
publish (GamePose y p r x yy z) =
    c_publish (realToFrac y) (realToFrac p) (realToFrac r) (realToFrac x) (realToFrac yy) (realToFrac z)

-- | Recenter requests games have made so far.
recenterCount :: IO Int
recenterCount = fromIntegral <$> c_recenter_count

-- | Games that have started head tracking and are still running.
gamesTracking :: IO Int
gamesTracking = fromIntegral <$> c_games_tracking

-- | The name of one of the games tracking, as its program is named without
-- @.exe@, such as DCS; Nothing if there is none, or none can be read.
trackingGameName :: IO (Maybe String)
trackingGameName = allocaArray size $ \buf -> do
    found <- c_tracking_game_name buf (fromIntegral size)
    if found /= 0 then Just <$> peekCWString buf else pure Nothing
  where
    size = 64

-- | Claim the right to be the one running trackkat; False if another is.
singleInstance :: IO Bool
singleInstance = (/= 0) <$> c_single_instance

-- | Tell games to load NPClient from this directory. Left is a Windows error
-- code.
setNPClientPath :: FilePath -> IO (Either Int ())
setNPClientPath dir =
    withCWString (if "\\" `isSuffixOf` dir then dir else dir ++ "\\") $ \p ->
        (\rc -> if rc == 0 then Right () else Left (fromIntegral rc)) <$> c_set_npclient_path p

-- | The folder of NaturalPoint's TrackIR software, ending in a backslash,
-- if it is installed with its NPClient.
findTrackIRFolder :: IO (Maybe FilePath)
findTrackIRFolder = allocaArray size $ \buf -> do
    found <- c_find_trackir_dir buf (fromIntegral size)
    if found /= 0 then Just <$> peekCWString buf else pure Nothing
  where
    size = 260

-- | Why trackkat can't stand in for NaturalPoint's software.
data StandInProblem
    = -- | it is running itself, and so serves its NPClient
      TrackIRSoftwareRunning
    | -- | a Windows error code
      StandInFailed Int

-- | Stand in for NaturalPoint's TrackIR software, so that games can load
-- NaturalPoint's NPClient and still get the pose from trackkat, serving
-- again the game given, which a trackkat that ran before was serving.
startTrackIRStandIn :: Maybe ServedGame -> IO (Either StandInProblem ())
startTrackIRStandIn served =
    start >>= \case
        0 -> pure (Right ())
        -1 -> pure (Left TrackIRSoftwareRunning)
        e -> pure (Left (StandInFailed (fromIntegral e)))
  where
    start = case served of
        Nothing -> c_trackir_start 0 0 0 (-1) 0
        Just g ->
            c_trackir_start
                (fromIntegral (servedProcess g))
                (maybe 0 fromIntegral (servedStarted g))
                (fromIntegral (servedWindow g))
                (maybe (-1) fromIntegral (servedProfile g))
                (case servedTracking g of TrackingStarted -> 1; TrackingStopped -> 0)

-- | The game the stand-in for NaturalPoint's software serves, if any, with
-- a count that changes whenever it does.
trackIRServed :: IO (Int, Maybe ServedGame)
trackIRServed =
    alloca $ \pid -> alloca $ \started -> alloca $ \window -> alloca $ \profile -> alloca $ \tracking -> do
        changes <- c_trackir_served pid started window profile tracking
        p <- peek pid
        game <-
            if p == 0
                then pure Nothing
                else do
                    s <- peek started
                    w <- peek window
                    i <- peek profile
                    t <- peek tracking
                    pure . Just $
                        ServedGame
                            { servedProcess = fromIntegral p
                            , servedStarted = if s == 0 then Nothing else Just (fromIntegral s)
                            , servedWindow = fromIntegral w
                            , servedProfile = if i < 0 then Nothing else Just (fromIntegral i)
                            , servedTracking = if t /= 0 then TrackingStarted else TrackingStopped
                            }
        pure (fromIntegral changes, game)

-- | Whether the process with this ID runs, and since when.
gameProcess :: Int -> IO GameProcess
gameProcess pid = alloca $ \started ->
    c_process_started (fromIntegral pid) started >>= \case
        1 -> ProcessRunning . Just . fromIntegral <$> peek started
        2 -> pure (ProcessRunning Nothing)
        _ -> pure ProcessGone

-- | The process a window belongs to, if the window is open.
windowProcess :: Word64 -> IO (Maybe Int)
windowProcess window = (\p -> if p == 0 then Nothing else Just (fromIntegral p)) <$> c_window_process (fromIntegral window)

-- | The profile ID a game has given NaturalPoint's NPClient since the last
-- call, with the game's name if its data is scrambled.
takeTrackIRGame :: IO (Maybe (Int, Maybe String))
takeTrackIRGame = do
    game <- c_trackir_take_game
    if game < 0
        then pure Nothing
        else Just . (fromIntegral game,) <$> scrambledGameName (fromIntegral game)

-- | The name of the game with this profile ID, if its data is scrambled.
scrambledGameName :: Int -> IO (Maybe String)
scrambledGameName game = do
    name <- c_scrambled_game (fromIntegral game)
    if name == nullPtr then pure Nothing else Just <$> peekCString name

-- | What is watched for hotkeys.
data Watched = Keyboard | Mouse | Joysticks
    deriving (Eq, Show)

-- | Watch the given keys and buttons going down and coming up, anywhere in
-- Windows, without hiding them from other programs. Returns what can't be
-- watched, with Windows error codes.
startHotkeys :: [KeyOrButton] -> IO [(Watched, Int)]
startHotkeys keys = do
    rebindHotkeys keys
    alloca $ \mouseError -> alloca $ \joystickError -> do
        keyboardError <- c_hotkeys_start mouseError joystickError
        merr <- peek mouseError
        jerr <- peek joystickError
        pure [(w, fromIntegral e) | (w, e) <- [(Keyboard, keyboardError), (Mouse, merr), (Joysticks, jerr)], e /= 0]

-- | Change the keys and buttons watched, all at once.
rebindHotkeys :: [KeyOrButton] -> IO ()
rebindHotkeys keys =
    withNames (map name keys) $ \names ->
        withArrayLen (map code keys) $ \n codes ->
            withArray names $ \namesP ->
                withArray (map index keys) $ \indexes ->
                    withArray (map button keys) $ \buttons ->
                        c_watch (fromIntegral n) codes namesP indexes buttons
  where
    -- As hts_watch in cbits/windows.c takes them.
    code (Key mods key) = fromIntegral (modifierMask mods `shiftL` 16 .|. virtualKey key)
    code JoystickButton{} = 0
    name (Key _ _) = Nothing
    name (JoystickButton (Joystick j _) _) = Just j
    index (Key _ _) = 0
    index (JoystickButton (Joystick _ i) _) = fromIntegral i
    button (Key _ _) = 0
    button (JoystickButton _ b) = fromIntegral b
    -- Each name as a C string, or null for none, while the action runs.
    withNames :: [Maybe String] -> ([CWString] -> IO a) -> IO a
    withNames [] act = act []
    withNames (m : ms) act = maybe ($ nullPtr) withCWString m $ \p -> withNames ms (act . (p :))

-- | The watched keys' and buttons' presses and releases since the last
-- call, oldest first, given the moment now, which each is timed back from.
-- A joystick's button is told by its joystick's name as it was given to
-- watch.
takeKeyEdges :: Instant -> IO [KeyEdge]
takeKeyEdges now = alloca $ \code -> allocaArray nameLength $ \name -> alloca $ \index -> alloca $ \button -> alloca $ \age -> do
    let next = do
            kind <- c_key_edge code name (fromIntegral nameLength) index button age
            case kind of
                -- As EDGE_DOWN and EDGE_UP in cbits/windows.c.
                1 -> told WentDown
                2 -> told CameUp
                _ -> pure []
        told way = do
            ago <- realToFrac <$> peek age
            c <- fromIntegral <$> peek code
            key <-
                if c /= (0 :: Int)
                    then pure (keyFromCodes (c `shiftR` 16) (c .&. 0xFFFF))
                    else fmap Just (JoystickButton <$> (Joystick <$> peekCWString name <*> (fromIntegral <$> peek index)) <*> (fromIntegral <$> peek button))
            maybe id ((:) . KeyEdge (now .+^ seconds (negate ago)) way) key <$> next
    next
  where
    -- As JOYSTICK_NAME_LEN in cbits/joystick.h.
    nameLength = 128

-- | Joystick events since the last call, oldest first.
takeJoystickEvents :: IO [JoystickEvent]
takeJoystickEvents = allocaArray nameLength $ \name -> alloca $ \index -> alloca $ \button -> do
    let next = do
            kind <- c_joystick_event name (fromIntegral nameLength) index button
            let joystick = Joystick <$> peekCWString name <*> (fromIntegral <$> peek index)
            case kind of
                1 -> (:) <$> (Connected <$> joystick) <*> next
                2 -> (:) <$> (Disconnected <$> joystick) <*> next
                3 -> (:) <$> (Pressed <$> joystick <*> (fromIntegral <$> peek button)) <*> next
                _ -> pure []
    next
  where
    -- As JOYSTICK_NAME_LEN in cbits/joystick.h.
    nameLength = 128
