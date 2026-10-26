-- | trackkat.exe's Windows plumbing: see cbits/windows.c.
module TrackKat.Windows (
    openShared,
    publish,
    recenterCount,
    gameReads,
    singleInstance,
    setNPClientPath,
    Hotkey (..),
    startHotkeys,
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
foreign import ccall unsafe "hts_single_instance" c_single_instance :: IO CInt
foreign import ccall unsafe "hts_set_npclient_path" c_set_npclient_path :: CWString -> IO CInt
foreign import ccall unsafe "hts_hotkeys_start" c_hotkeys_start :: CUInt -> CUInt -> CUInt -> CUInt -> IO CInt
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

-- | Listen for the recenter and pause hotkeys anywhere in Windows. Returns
-- those that couldn't be registered, usually because another program holds
-- them.
startHotkeys :: Binding -> Binding -> IO [Hotkey]
startHotkeys recenter pause = do
    failed <-
        c_hotkeys_start
            (fromIntegral (modifierMask recenter))
            (fromIntegral (virtualKey recenter))
            (fromIntegral (modifierMask pause))
            (fromIntegral (virtualKey pause))
    pure [k | (k, b) <- [(RecenterKey, 0), (PauseKey, 1)], testBit failed b]

-- | Hotkeys pressed since the last call.
takeHotkeys :: IO [Hotkey]
takeHotkeys = do
    pressed <- c_hotkeys_take
    pure [k | (k, b) <- [(RecenterKey, 0), (PauseKey, 1)], testBit pressed b]
