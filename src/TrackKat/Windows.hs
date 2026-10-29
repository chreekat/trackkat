{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- | trackkat.exe's Windows plumbing: see cbits/windows.c, cbits/trackir.c
-- and cbits/joystick.c.
module TrackKat.Windows (
    openShared,
    publish,
    recenterCount,
    gameReads,
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
    Hotkey (..),
    Watched (..),
    startHotkeys,
    rebindHotkeys,
    takeHotkeys,
    JoystickEvent (..),
    takeJoystickEvents,
) where

import Data.Bits ((.&.))
import Data.Word (Word64)
import Foreign.C.String (CString, CWString, peekCString, peekCWString, withCWString)
import Foreign.C.Types (CFloat (..), CInt (..), CLong (..), CUInt (..), CULLong (..), CULong (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)

import TrackKat.Config (Binding (..), Joystick (..), modifierMask, virtualKey)
import TrackKat.GamePose
import TrackKat.ServedGame

foreign import ccall unsafe "hts_shared_open" c_shared_open :: IO CInt
foreign import ccall unsafe "hts_publish" c_publish :: CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> IO ()
foreign import ccall unsafe "hts_recenter_count" c_recenter_count :: IO CLong
foreign import ccall unsafe "hts_game_reads" c_game_reads :: IO CLong
foreign import ccall unsafe "hts_games_tracking" c_games_tracking :: IO CInt
foreign import ccall unsafe "hts_tracking_game_name" c_tracking_game_name :: CWString -> CInt -> IO CInt
foreign import ccall unsafe "hts_single_instance" c_single_instance :: IO CInt
foreign import ccall unsafe "hts_set_npclient_path" c_set_npclient_path :: CWString -> IO CInt
foreign import ccall unsafe "hts_find_trackir_dir" c_find_trackir_dir :: CWString -> CInt -> IO CInt
foreign import ccall safe "hts_trackir_start" c_trackir_start :: CULong -> CULLong -> CULLong -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "hts_trackir_served"
    c_trackir_served :: Ptr CULong -> Ptr CULLong -> Ptr CULLong -> Ptr CInt -> Ptr CInt -> IO CLong
foreign import ccall unsafe "hts_process_started" c_process_started :: CULong -> Ptr CULLong -> IO CInt
foreign import ccall unsafe "hts_window_process" c_window_process :: CULLong -> IO CULong
foreign import ccall unsafe "hts_trackir_take_game" c_trackir_take_game :: IO CInt
foreign import ccall unsafe "hts_scrambled_game" c_scrambled_game :: CInt -> IO CString
foreign import ccall unsafe "hts_bindings_clear" c_bindings_clear :: IO ()
foreign import ccall unsafe "hts_bind_key" c_bind_key :: CInt -> CUInt -> CUInt -> IO ()
foreign import ccall unsafe "hts_bind_button" c_bind_button :: CInt -> CWString -> CInt -> CInt -> IO ()
foreign import ccall unsafe "hts_bindings_apply" c_bindings_apply :: IO ()
foreign import ccall unsafe "hts_hotkeys_start" c_hotkeys_start :: Ptr CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "hts_hotkeys_take" c_hotkeys_take :: IO CInt
foreign import ccall unsafe "hts_joystick_event" c_joystick_event :: CWString -> CInt -> Ptr CInt -> Ptr CInt -> IO CInt

-- | Create the shared memory games read from. Left is a Windows error code.
openShared :: IO (Either Int ())
openShared = (\rc -> if rc == 0 then Right () else Left (fromIntegral rc)) <$> c_shared_open

publish :: GamePose -> IO ()
publish (GamePose y p r x yy z) =
    c_publish (realToFrac y) (realToFrac p) (realToFrac r) (realToFrac x) (realToFrac yy) (realToFrac z)

-- | Recenter requests games have made so far.
recenterCount :: IO Int
recenterCount = fromIntegral <$> c_recenter_count

-- | Poses games have read so far.
gameReads :: IO Int
gameReads = fromIntegral <$> c_game_reads

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
    withCWString (if last dir == '\\' then dir else dir ++ "\\") $ \p ->
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

-- | An action bound to keys and joystick buttons.
data Hotkey = RecenterKey | PauseKey | TrimKey
    deriving (Eq, Show, Enum, Bounded)

-- | What is watched for hotkeys.
data Watched = Keyboard | Mouse | Joysticks
    deriving (Eq, Show)

-- | Bind the hotkeys, each to its keys and buttons, and watch for them
-- anywhere in Windows without hiding them from other programs. Returns what
-- can't be watched, with Windows error codes.
startHotkeys :: (Hotkey -> [Binding]) -> IO [(Watched, Int)]
startHotkeys bindings = do
    rebindHotkeys bindings
    alloca $ \mouseError -> alloca $ \joystickError -> do
        keyboardError <- c_hotkeys_start mouseError joystickError
        merr <- peek mouseError
        jerr <- peek joystickError
        pure [(w, fromIntegral e) | (w, e) <- [(Keyboard, keyboardError), (Mouse, merr), (Joysticks, jerr)], e /= 0]

-- | Change the hotkeys' keys and buttons.
rebindHotkeys :: (Hotkey -> [Binding]) -> IO ()
rebindHotkeys bindings = do
    c_bindings_clear
    mapM_ (\k -> mapM_ (bind k) (bindings k)) [minBound .. maxBound]
    c_bindings_apply
  where
    bind k (Key mods key) = c_bind_key (hotkeyBit k) (fromIntegral (modifierMask mods)) (fromIntegral (virtualKey key))
    bind k (JoystickButton (Joystick name index) n) =
        withCWString name $ \p -> c_bind_button (hotkeyBit k) p (fromIntegral index) (fromIntegral n)

-- | Hotkeys pressed since the last call.
takeHotkeys :: IO [Hotkey]
takeHotkeys = do
    pressed <- c_hotkeys_take
    pure [k | k <- [minBound .. maxBound], pressed .&. hotkeyBit k /= 0]

-- | How cbits/joystick.h numbers a hotkey: HOTKEY_RECENTER, HOTKEY_PAUSE,
-- HOTKEY_TRIM.
hotkeyBit :: Hotkey -> CInt
hotkeyBit RecenterKey = 1
hotkeyBit PauseKey = 2
hotkeyBit TrimKey = 4

-- | Something that happened to a joystick: it was attached (or found at
-- start), detached, or one of its buttons was pressed.
data JoystickEvent = Connected Joystick | Disconnected Joystick | Pressed Joystick Int
    deriving (Eq, Show)

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
