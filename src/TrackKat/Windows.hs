-- | trackkat.exe's Windows plumbing: see cbits/windows.c.
module TrackKat.Windows (
    openShared,
    publish,
    recenterCount,
    gameReads,
    gamesTracking,
    singleInstance,
    setNPClientPath,
    Hotkey (..),
    startHotkeys,
    rebindHotkeys,
    takeHotkeys,
) where

import Data.Bits (testBit)
import Foreign.C.String (CWString, withCWString)
import Foreign.C.Types (CFloat (..), CInt (..), CLong (..), CUInt (..))

import TrackKat.Config (Binding, modifierMask, virtualKey)
import TrackKat.GamePose

foreign import ccall unsafe "hts_shared_open" c_shared_open :: IO CInt
foreign import ccall unsafe "hts_publish" c_publish :: CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> IO ()
foreign import ccall unsafe "hts_recenter_count" c_recenter_count :: IO CLong
foreign import ccall unsafe "hts_game_reads" c_game_reads :: IO CLong
foreign import ccall unsafe "hts_games_tracking" c_games_tracking :: IO CInt
foreign import ccall unsafe "hts_single_instance" c_single_instance :: IO CInt
foreign import ccall unsafe "hts_set_npclient_path" c_set_npclient_path :: CWString -> IO CInt
foreign import ccall unsafe "hts_hotkeys_start" c_hotkeys_start :: CUInt -> CUInt -> CUInt -> CUInt -> IO CInt
foreign import ccall unsafe "hts_hotkeys_rebind" c_hotkeys_rebind :: CUInt -> CUInt -> CUInt -> CUInt -> IO ()
foreign import ccall unsafe "hts_hotkeys_take" c_hotkeys_take :: IO CInt

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

data Hotkey = RecenterKey | PauseKey
    deriving (Eq, Show)

-- | Watch for the recenter and pause hotkeys anywhere in Windows, without
-- hiding them from other programs. Left is a Windows error code.
startHotkeys :: Binding -> Binding -> IO (Either Int ())
startHotkeys recenter pause = do
    rc <- withCodes c_hotkeys_start recenter pause
    pure (if rc == 0 then Right () else Left (fromIntegral rc))

-- | Change the recenter and pause hotkeys.
rebindHotkeys :: Binding -> Binding -> IO ()
rebindHotkeys = withCodes c_hotkeys_rebind

withCodes :: (CUInt -> CUInt -> CUInt -> CUInt -> a) -> Binding -> Binding -> a
withCodes f recenter pause =
    f
        (fromIntegral (modifierMask recenter))
        (fromIntegral (virtualKey recenter))
        (fromIntegral (modifierMask pause))
        (fromIntegral (virtualKey pause))

-- | Hotkeys pressed since the last call.
takeHotkeys :: IO [Hotkey]
takeHotkeys = do
    pressed <- c_hotkeys_take
    pure [k | (k, b) <- [(RecenterKey, 0), (PauseKey, 1)], testBit pressed b]
