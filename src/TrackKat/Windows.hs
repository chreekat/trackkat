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
    singleInstance,
    setNPClientPath,
    findTrackIRFolder,
    StandInProblem (..),
    startTrackIRStandIn,
    takeTrackIRGame,
    Hotkey (..),
    Watched (..),
    startHotkeys,
    rebindHotkeys,
    takeHotkeys,
    JoystickEvent (..),
    takeJoystickEvents,
) where

import Data.Bits ((.&.))
import Foreign.C.String (CString, CWString, peekCString, peekCWString, withCWString)
import Foreign.C.Types (CFloat (..), CInt (..), CLong (..), CUInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)

import TrackKat.Config (Binding (..), Hotkey (..), Joystick (..), modifierMask, virtualKey)
import TrackKat.GamePose

foreign import ccall unsafe "hts_shared_open" c_shared_open :: IO CInt
foreign import ccall unsafe "hts_publish" c_publish :: CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> IO ()
foreign import ccall unsafe "hts_recenter_count" c_recenter_count :: IO CLong
foreign import ccall unsafe "hts_game_reads" c_game_reads :: IO CLong
foreign import ccall unsafe "hts_games_tracking" c_games_tracking :: IO CInt
foreign import ccall unsafe "hts_single_instance" c_single_instance :: IO CInt
foreign import ccall unsafe "hts_set_npclient_path" c_set_npclient_path :: CWString -> IO CInt
foreign import ccall unsafe "hts_find_trackir_dir" c_find_trackir_dir :: CWString -> CInt -> IO CInt
foreign import ccall safe "hts_trackir_start" c_trackir_start :: IO CInt
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
-- NaturalPoint's NPClient and still get the pose from trackkat.
startTrackIRStandIn :: IO (Either StandInProblem ())
startTrackIRStandIn =
    c_trackir_start >>= \case
        0 -> pure (Right ())
        -1 -> pure (Left TrackIRSoftwareRunning)
        e -> pure (Left (StandInFailed (fromIntegral e)))

-- | The profile ID a game has given NaturalPoint's NPClient since the last
-- call, with the game's name if its data is scrambled.
takeTrackIRGame :: IO (Maybe (Int, Maybe String))
takeTrackIRGame = do
    game <- c_trackir_take_game
    if game < 0
        then pure Nothing
        else do
            name <- c_scrambled_game game
            Just . (fromIntegral game,) <$> (if name == nullPtr then pure Nothing else Just <$> peekCString name)

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
