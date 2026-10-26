-- | A deliberately tiny binding to the parts of libusb we need: open a device,
-- claim interface 0, and do bulk transfers.
module TrackKat.USB
  ( Context
  , Handle
  , USBError (..)
  , withContext
  , openFirst
  , DeviceInfo (..)
  , listDevices
  , errorName
  , claim
  , release
  , close
  , bulkWrite
  , bulkRead
  , errNotFound
  , errNotSupported
  , errAccess
  , errBusy
  , errNoDevice
  ) where

import Control.Exception (Exception, bracket, throwIO)
import Control.Monad (unless, when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Data.Word (Word16, Word8)
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CInt (..), CUChar (..), CUInt (..))
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (allocaArray, peekArray, withArrayLen)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peek)

data CContext
data CHandle

newtype Context = Context (Ptr CContext)
newtype Handle = Handle (Ptr CHandle)

data USBError = USBError
  { usbCall :: String
  , usbCode :: Int
  , usbName :: String
  }
  deriving (Show)

instance Exception USBError

errNotFound, errNotSupported, errAccess, errBusy, errNoDevice, errTimeout :: Int
errAccess = -3
errNoDevice = -4
errNotFound = -5
errBusy = -6
errTimeout = -7
errNotSupported = -12

foreign import ccall unsafe "libusb_init"
  c_init :: Ptr (Ptr CContext) -> IO CInt
foreign import ccall unsafe "libusb_exit"
  c_exit :: Ptr CContext -> IO ()
foreign import ccall unsafe "libusb_error_name"
  c_error_name :: CInt -> IO CString
foreign import ccall unsafe "hts_open_first"
  c_open_first :: Ptr CContext -> Word16 -> Ptr Word16 -> CInt -> Ptr Word16 -> Ptr (Ptr CHandle) -> IO CInt
foreign import ccall unsafe "hts_list"
  c_list :: Ptr CContext -> Ptr Word16 -> Ptr Word16 -> Ptr CInt -> CInt -> IO CInt
foreign import ccall unsafe "libusb_close"
  c_close :: Ptr CHandle -> IO ()
foreign import ccall unsafe "libusb_get_configuration"
  c_get_configuration :: Ptr CHandle -> Ptr CInt -> IO CInt
foreign import ccall unsafe "libusb_set_configuration"
  c_set_configuration :: Ptr CHandle -> CInt -> IO CInt
foreign import ccall unsafe "libusb_set_auto_detach_kernel_driver"
  c_set_auto_detach :: Ptr CHandle -> CInt -> IO CInt
foreign import ccall unsafe "libusb_claim_interface"
  c_claim_interface :: Ptr CHandle -> CInt -> IO CInt
foreign import ccall unsafe "libusb_release_interface"
  c_release_interface :: Ptr CHandle -> CInt -> IO CInt
-- Blocks for up to the timeout, so it must be a safe call.
foreign import ccall safe "libusb_bulk_transfer"
  c_bulk_transfer :: Ptr CHandle -> CUChar -> Ptr Word8 -> CInt -> Ptr CInt -> CUInt -> IO CInt

check :: String -> CInt -> IO ()
check call rc = when (rc < 0) $ throwUSB call rc

throwUSB :: String -> CInt -> IO a
throwUSB call rc = do
  name <- errorName (fromIntegral rc)
  throwIO (USBError call (fromIntegral rc) name)

errorName :: Int -> IO String
errorName rc = c_error_name (fromIntegral rc) >>= peekCString

withContext :: (Context -> IO a) -> IO a
withContext = bracket acquire (\(Context p) -> c_exit p)
  where
    acquire = alloca $ \pp -> do
      c_init pp >>= check "libusb_init"
      Context <$> peek pp

-- | Open the first device with this vendor id and one of these product ids
-- (earlier ids preferred). Returns the handle and the product id it matched.
openFirst :: Context -> Word16 -> [Word16] -> IO (Handle, Word16)
openFirst (Context ctx) vid pids =
  withArrayLen pids $ \n pidsPtr ->
    alloca $ \foundPtr ->
      alloca $ \hPtr -> do
        c_open_first ctx vid pidsPtr (fromIntegral n) foundPtr hPtr
          >>= check "open"
        (,) <$> (Handle <$> peek hPtr) <*> peek foundPtr

data DeviceInfo = DeviceInfo
  { devVendor :: Word16
  , devProduct :: Word16
  , devOpenResult :: Int  -- ^ 0 if we could open it, else a libusb error
  }
  deriving (Show)

-- | Every attached device, and whether we can open it. For diagnostics.
listDevices :: Context -> IO [DeviceInfo]
listDevices (Context ctx) =
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
claim (Handle h) = do
  -- Unsupported on Windows; only matters on Linux.
  _ <- c_set_auto_detach h 1
  cfg <- alloca $ \p -> c_get_configuration h p >>= check "get_configuration" >> peek p
  unless (cfg == 1) $ c_set_configuration h 1 >>= check "set_configuration"
  c_claim_interface h 0 >>= check "claim_interface"

release :: Handle -> IO ()
release (Handle h) = () <$ c_release_interface h 0

close :: Handle -> IO ()
close (Handle h) = c_close h

bulkWrite :: Handle -> Word8 -> B.ByteString -> IO ()
bulkWrite (Handle h) ep bytes =
  BU.unsafeUseAsCStringLen bytes $ \(p, len) ->
    alloca $ \transferred ->
      c_bulk_transfer h (fromIntegral ep) (castPtr p) (fromIntegral len) transferred 1000
        >>= check "bulk write"

-- | Read up to @size@ bytes. A timeout is not an error: it yields whatever
-- arrived, usually nothing.
bulkRead :: Handle -> Word8 -> Int -> Int -> IO B.ByteString
bulkRead (Handle h) ep size timeoutMs =
  allocaBytes size $ \buf ->
    alloca $ \transferred -> do
      rc <- c_bulk_transfer h (fromIntegral ep) buf (fromIntegral size) transferred (fromIntegral timeoutMs)
      unless (rc == 0 || fromIntegral rc == errTimeout) $ throwUSB "bulk read" rc
      n <- peek transferred
      B.packCStringLen (castPtr buf, fromIntegral n)
