-- | A deliberately tiny binding to the parts of libusb we need: open a device,
-- claim interface 0, and do bulk transfers.
module TrackKat.USB (
    Context,
    Handle,
    Reads (..),
    USBError (..),
    withContext,
    openFirst,
    DeviceInfo (..),
    listDevices,
    errorName,
    claim,
    release,
    close,
    bulkWrite,
    bulkRead,
    Stream,
    withStream,
    pump,
    errNotFound,
    errNotSupported,
    errAccess,
    errBusy,
) where

import Control.Exception (Exception, bracket, throwIO)
import Control.Monad (unless, when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word16, Word8)
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CInt (..), CUChar (..), CUInt (..))
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (allocaArray, peekArray, withArrayLen)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek)

data CContext
data CHandle
data CStream

-- | Whether the reads a stream queued with a device all finished once
-- cancelled, or some are stranded there, still libusb's (see 'withStream').
data Reads = Finished | Stranded
    deriving (Eq, Show)

-- | A libusb session, and whether any of its devices has stranded reads.
data Context = Context (Ptr CContext) (IORef Reads)

-- | An open device, and whether it has stranded reads.
data Handle = Handle (Ptr CHandle) (IORef Reads)

-- | A libusb call that failed: which, its error code, and libusb's name
-- for the code.
data USBError = USBError
    { usbCall :: String
    , usbCode :: Int
    , usbName :: String
    }
    deriving (Show)

instance Exception USBError

-- | libusb's error codes that callers tell apart.
errNotFound, errNotSupported, errAccess, errBusy, errTimeout :: Int
errAccess = -3
errNotFound = -5
errBusy = -6
errTimeout = -7
errNotSupported = -12

-- Calls that can block (enumerating or opening devices, USB requests,
-- handling events) are safe, so they don't hold up every other Haskell
-- thread; only those that just look at or set memory are unsafe.
foreign import ccall safe "libusb_init"
    c_init :: Ptr (Ptr CContext) -> IO CInt
foreign import ccall safe "libusb_exit"
    c_exit :: Ptr CContext -> IO ()
foreign import ccall unsafe "libusb_error_name"
    c_error_name :: CInt -> IO CString
foreign import ccall safe "hts_open_first"
    c_open_first :: Ptr CContext -> Word16 -> Ptr Word16 -> CInt -> Ptr Word16 -> Ptr (Ptr CHandle) -> IO CInt
foreign import ccall safe "hts_list"
    c_list :: Ptr CContext -> Ptr Word16 -> Ptr Word16 -> Ptr CInt -> CInt -> IO CInt
foreign import ccall safe "libusb_close"
    c_close :: Ptr CHandle -> IO ()
foreign import ccall safe "libusb_get_configuration"
    c_get_configuration :: Ptr CHandle -> Ptr CInt -> IO CInt
foreign import ccall safe "libusb_set_configuration"
    c_set_configuration :: Ptr CHandle -> CInt -> IO CInt
foreign import ccall unsafe "libusb_set_auto_detach_kernel_driver"
    c_set_auto_detach :: Ptr CHandle -> CInt -> IO CInt
foreign import ccall safe "libusb_claim_interface"
    c_claim_interface :: Ptr CHandle -> CInt -> IO CInt
foreign import ccall safe "libusb_release_interface"
    c_release_interface :: Ptr CHandle -> CInt -> IO CInt

foreign import ccall safe "hts_stream_open"
    c_stream_open :: Ptr CContext -> Ptr CHandle -> CUChar -> CInt -> CInt -> Ptr CInt -> IO (Ptr CStream)
foreign import ccall safe "hts_stream_close"
    c_stream_close :: Ptr CStream -> IO CInt
foreign import ccall safe "hts_stream_pump"
    c_stream_pump :: Ptr CStream -> CInt -> Ptr Word8 -> CInt -> IO CInt
foreign import ccall safe "libusb_bulk_transfer"
    c_bulk_transfer :: Ptr CHandle -> CUChar -> Ptr Word8 -> CInt -> Ptr CInt -> CUInt -> IO CInt

check :: String -> CInt -> IO ()
check call rc = when (rc < 0) $ throwUSB call rc

throwUSB :: String -> CInt -> IO a
throwUSB call rc = do
    name <- errorName (fromIntegral rc)
    throwIO (USBError call (fromIntegral rc) name)

-- | libusb's name for an error code.
errorName :: Int -> IO String
errorName rc = c_error_name (fromIntegral rc) >>= peekCString

-- | Run the action with a libusb session, ended however the action exits,
-- unless one of its devices was left open under stranded reads (see
-- 'close'): libusb must not end a session under them.
withContext :: (Context -> IO a) -> IO a
withContext = bracket acquire end
  where
    acquire = alloca $ \pp -> do
        c_init pp >>= check "libusb_init"
        Context <$> peek pp <*> newIORef Finished
    end (Context p ctxReads) = do
        r <- readIORef ctxReads
        case r of
            Finished -> c_exit p
            Stranded -> pure ()

-- | Open the first device with this vendor id and one of these product ids
-- (earlier ids preferred). Returns the handle and the product id it matched.
openFirst :: Context -> Word16 -> [Word16] -> IO (Handle, Word16)
openFirst (Context ctx _) vid pids =
    withArrayLen pids $ \n pidsPtr ->
        alloca $ \foundPtr ->
            alloca $ \hPtr -> do
                c_open_first ctx vid pidsPtr (fromIntegral n) foundPtr hPtr
                    >>= check "open"
                (,) <$> (Handle <$> peek hPtr <*> newIORef Finished) <*> peek foundPtr

-- | An attached device.
data DeviceInfo = DeviceInfo
    { devVendor :: Word16
    , devProduct :: Word16
    , devOpenResult :: Int
    -- ^ 0 if we could open it, else a libusb error
    }
    deriving (Show)

-- | Every attached device, and whether we can open it. For diagnostics.
listDevices :: Context -> IO [DeviceInfo]
listDevices (Context ctx _) =
    allocaArray maxDevs $ \vids ->
        allocaArray maxDevs $ \pids ->
            allocaArray maxDevs $ \rcs -> do
                n <- c_list ctx vids pids rcs (fromIntegral maxDevs)
                check "list devices" n
                let k = fromIntegral n
                zipWith3 DeviceInfo
                    <$> peekArray k vids
                    <*> peekArray k pids
                    <*> (map fromIntegral <$> peekArray k rcs)
  where
    maxDevs = 256

-- | Select configuration 1 if needed and claim interface 0.
claim :: Handle -> IO ()
claim (Handle h _) = do
    -- Unsupported on Windows; only matters on Linux.
    _ <- c_set_auto_detach h 1
    cfg <- alloca $ \p -> c_get_configuration h p >>= check "get_configuration" >> peek p
    unless (cfg == 1) $ c_set_configuration h 1 >>= check "set_configuration"
    c_claim_interface h 0 >>= check "claim_interface"

-- | Give up interface 0, unless reads are stranded on it; failure is
-- ignored.
release :: Handle -> IO ()
release (Handle h hReads) = do
    r <- readIORef hReads
    case r of
        Finished -> () <$ c_release_interface h 0
        Stranded -> pure ()

-- | Close a device opened by 'openFirst', or leave it open if reads are
-- stranded with it, as libusb must not close a device under them. Returns
-- which.
close :: Handle -> IO Reads
close (Handle h hReads) = do
    r <- readIORef hReads
    case r of
        Finished -> c_close h
        Stranded -> pure ()
    pure r

-- | Write the bytes to an endpoint, waiting up to a second.
bulkWrite :: Handle -> Word8 -> B.ByteString -> IO ()
bulkWrite (Handle h _) ep bytes =
    BU.unsafeUseAsCStringLen bytes $ \(p, len) ->
        alloca $ \transferred ->
            c_bulk_transfer h (fromIntegral ep) (castPtr p) (fromIntegral len) transferred 1000
                >>= check "bulk write"

-- | Read up to @size@ bytes. A timeout is not an error: it yields whatever
-- arrived, usually nothing.
bulkRead :: Handle -> Word8 -> Int -> Int -> IO B.ByteString
bulkRead (Handle h _) ep size timeoutMs =
    allocaBytes size $ \buf ->
        alloca $ \transferred -> do
            rc <- c_bulk_transfer h (fromIntegral ep) buf (fromIntegral size) transferred (fromIntegral timeoutMs)
            unless (rc == 0 || fromIntegral rc == errTimeout) $ throwUSB "bulk read" rc
            n <- peek transferred
            B.packCStringLen (castPtr buf, fromIntegral n)

-- | Reads from an endpoint that stay queued with the device.
newtype Stream = Stream (Ptr CStream)

-- | Keep @n@ reads of @size@ bytes queued on an endpoint for the duration of
-- the action. Use the stream from one thread only. Reads still unfinished
-- some seconds after they are cancelled are stranded: the device and the
-- session are marked so, to be left open under them.
withStream :: Context -> Handle -> Word8 -> Int -> Int -> (Stream -> IO a) -> IO a
withStream (Context ctx ctxReads) (Handle h hReads) ep n size = bracket open end
  where
    open = alloca $ \rc -> do
        s <- c_stream_open ctx h (fromIntegral ep) (fromIntegral n) (fromIntegral size) rc
        failed <- peek rc
        unless (failed == 0) $ do
            unless (s == nullPtr) $ end (Stream s)
            throwUSB "stream open" failed
        pure (Stream s)
    end (Stream s) = do
        unfinished <- c_stream_close s
        unless (unfinished == 0) $ do
            writeIORef hReads Stranded
            writeIORef ctxReads Stranded

-- | Wait up to the timeout in milliseconds for data, and return what has
-- arrived, possibly nothing.
pump :: Stream -> Int -> IO B.ByteString
pump (Stream s) timeoutMs =
    allocaBytes cap $ \buf -> do
        n <- c_stream_pump s (fromIntegral timeoutMs) buf (fromIntegral cap)
        when (n < 0) $ throwUSB "stream read" n
        B.packCStringLen (castPtr buf, fromIntegral n)
  where
    cap = 65536
