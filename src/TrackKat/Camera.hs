{-# LANGUAGE TypeApplications #-}

-- | Driving a TrackIR 5 revision 3 camera.
--
-- Command sequences ported from linuxtrack's @tir_hw.c@ (MIT licence,
-- Copyright (c) 2009 Tulthix, uglyDwarf). Most opcodes are undocumented; the
-- names below are linuxtrack's best guesses.
module TrackKat.Camera
  ( Camera
  , Settings (..)
  , defaultSettings
  , OpenError (..)
  , vendorId
  , productIdV3
  , withCamera
  , readPackets
  , Log
  , hex
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception, bracket_, finally, throwIO, try)
import Control.Monad (replicateM_, void)
import Data.Bits (shiftL, shiftR, (.&.))
import qualified Data.ByteString as B
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Word (Word16, Word8)
import GHC.Clock (getMonotonicTime, getMonotonicTimeNSec)

import TrackKat.Frame (Packet, parseTransfer)
import TrackKat.Protocol
import qualified TrackKat.USB as USB

type Log = String -> IO ()

vendorId, productIdV3 :: Word16
vendorId = 0x131D
productIdV3 = 0x0159

outEp, inEp :: Word8
outEp = 0x01
inEp = 0x82

data Settings = Settings
  { threshold :: Int     -- ^ pixel brightness cut-off, 30..253
  , irBrightness :: Int  -- ^ IR illuminator level, 5..7
  , statusLed :: Bool    -- ^ show the green "tracking" light
  }
  deriving (Show)

defaultSettings :: Settings
defaultSettings = Settings {threshold = 140, irBrightness = 7, statusLed = True}

data Camera = Camera
  { camHandle :: USB.Handle
  , camSeed :: IORef Seed
  , camLog :: Log
  }

data OpenError
  = NoCamera
  | WrongDriver String  -- ^ present, but not bound to WinUSB (libusb's error)
  | InUse         -- ^ another program has it open
  | OpenFailed USB.USBError
  deriving (Show)

instance Exception OpenError

-- | Find, initialise and start the camera, run the action, and shut the
-- camera down again however the action exits.
withCamera :: USB.Context -> Log -> Settings -> (Camera -> IO a) -> IO a
withCamera ctx logger settings act = do
  (h, _) <- openOrExplain
  seed <- newIORef . newSeed =<< getMonotonicTimeNSec
  let cam = Camera h seed logger
  (`finally` USB.close h) $ do
    explainClaim (USB.claim h)
    bracket_ (initialise cam settings >> start cam settings) (shutdown cam) (act cam)
      `finally` USB.release h
  where
    openOrExplain = do
      r <- try (USB.openFirst ctx vendorId [productIdV3])
      case r of
        Right x -> pure x
        Left e -> do
          -- libusb's open can fail with NOT_FOUND even when the device is
          -- attached (on Windows, depending on the state of the vendor
          -- driver), so check whether it is really there.
          attached <- any isCamera <$> USB.listDevices ctx
          throwIO $ case () of
            _
              | not attached -> NoCamera
              | USB.usbCode e `elem` [USB.errNotFound, USB.errNotSupported] -> WrongDriver (USB.usbName e)
              | USB.usbCode e == USB.errAccess -> InUse
              | otherwise -> OpenFailed e
    isCamera d = USB.devVendor d == vendorId && USB.devProduct d == productIdV3
    explainClaim io = do
      r <- try io
      case r of
        Right () -> pure ()
        Left e
          | USB.usbCode e `elem` [USB.errBusy, USB.errAccess] -> throwIO InUse
          | USB.usbCode e == USB.errNotSupported -> throwIO (WrongDriver (USB.usbName e))
          | otherwise -> throwIO (OpenFailed e)

-- | Wait for the next USB transfer (up to 1 s) and decode it.
readPackets :: Camera -> IO [Packet]
readPackets cam = parseTransfer <$> USB.bulkRead (camHandle cam) inEp 16384 1000

send :: Camera -> Command -> IO ()
send cam cmd = do
  bytes <- atomicModifyIORef' (camSeed cam) (\s -> let (b, s') = encode s cmd in (s', b))
  USB.bulkWrite (camHandle cam) outEp bytes
  -- linuxtrack paces every v3 command like this; the camera may need it.
  threadDelay 50000

setThreshold :: Camera -> Int -> IO ()
setThreshold cam t = send cam (Cmd4 0x19 0x05 (fromIntegral (t `shiftR` 7)) (fromIntegral ((t `shiftL` 1) .&. 0xFF)))

setIrLed :: Camera -> Bool -> IO ()
setIrLed cam on = send cam (Cmd4 0x19 0x09 0 (if on then 1 else 0))

setIrBrightnessRaw :: Camera -> Int -> IO ()
setIrBrightnessRaw cam b = do
  send cam (Cmd4 0x23 0x35 0x02 (fromIntegral (b .&. 0xFF)))
  send cam (Cmd4 0x23 0x35 0x01 (fromIntegral (b `shiftR` 8)))
  send cam (Cmd4 0x23 0x35 0x00 0x00)
  send cam (Cmd4 0x23 0x3B 0x8F (fromIntegral ((b `shiftR` 4) .&. 0xFF)))
  send cam (Cmd4 0x23 0x3B 0x8E (fromIntegral (b `shiftR` 12)))

setIrBrightness :: Camera -> Int -> IO ()
setIrBrightness cam level =
  setIrBrightnessRaw cam ((case max 5 (min 7 level) of 5 -> 150; 6 -> 250; _ -> 350) `shiftL` 4)

setStatusLed :: Camera -> Bool -> IO ()
setStatusLed cam running = send cam (Cmd4 0x19 0x04 0x00 (if running then 0x22 else 0x33))

-- | Discard anything the camera has queued.
drain :: Camera -> IO ()
drain cam = do
  b <- USB.bulkRead (camHandle cam) inEp 16384 100
  if B.null b then pure () else drain cam

-- | Send a request and wait up to ~2 s for a reply matching the predicate.
request :: Camera -> Command -> (B.ByteString -> Maybe a) -> IO (Maybe a)
request cam cmd match = go (20 :: Int)
  where
    go 0 = pure Nothing
    go n = do
      send cam cmd
      deadline <- (+ 0.1) <$> getMonotonicTime
      r <- waitUntil deadline
      maybe (go (n - 1)) (pure . Just) r
    waitUntil deadline = do
      b <- USB.bulkRead (camHandle cam) inEp 16384 500
      case match b of
        Just x -> pure (Just x)
        Nothing -> do
          now <- getMonotonicTime
          if now > deadline then pure Nothing else waitUntil deadline

readStatus :: Camera -> IO (Maybe Status)
readStatus cam = do
  r <- request cam (Cmd2 0x1A 0x07) parseStatus
  camLog cam $ maybe "status: no reply" (\s -> "status: " ++ hex (statusRaw s)) r
  pure r

initialise :: Camera -> Settings -> IO ()
initialise cam _ = do
  camLog cam "initialising"
  send cam (Cmd2 0x1A 0x00)
  send cam (Cmd2 0x1A 0x00)
  send cam (Cmd1 0x13)
  void (readStatus cam)
  send cam (Cmd2 0x1A 0x01)
  void (readStatus cam)
  send cam (Cmd2 0x1A 0x02)
  void (readStatus cam)
  let waitReady 0 = camLog cam "warning: camera never reported ready"
      waitReady n = do
        send cam (Cmd2 0x1A 0x03)
        r <- readStatus cam
        if maybe False statusReady r then pure () else waitReady (n - 1 :: Int)
  waitReady 3
  drain cam
  info <- request cam (Cmd1 0x17) (\b -> if isDevInfo b then Just b else Nothing)
  camLog cam $ maybe "device info: no reply" (("device info: " ++) . hex) info
  setThreshold cam 0x96
  setIrBrightnessRaw cam 0x780
  setIrLed cam True
  send cam (Cmd4 0x19 0x03 0x00 0x05)
  send cam (Cmd4 0x19 0x04 0x00 0x00)

start :: Camera -> Settings -> IO ()
start cam s = do
  camLog cam "starting video"
  setIrLed cam True
  setIrBrightness cam (irBrightness s)
  replicateM_ 2 (setThreshold cam (max 30 (min 253 (threshold s))))
  send cam (Cmd4 0x19 0x04 0x00 0x00)
  send cam (Cmd4 0x19 0x03 0x00 0x05)
  send cam (Cmd2 0x1A 0x04)
  setIrLed cam True
  setStatusLed cam (statusLed s)

-- | Stop video and leave the camera as linuxtrack does on close.
shutdown :: Camera -> IO ()
shutdown cam = do
  camLog cam "stopping"
  r <- try @USB.USBError $ do
    send cam (Cmd2 0x1A 0x05)
    setIrLed cam False
    send cam (Cmd2 0x1A 0x06)
    send cam (Cmd1 0x13)
    setThreshold cam 0x96
    setIrBrightnessRaw cam 0x780
    setIrLed cam True
    send cam (Cmd4 0x19 0x03 0x00 0x05)
    setIrLed cam False
    send cam (Cmd2 0x1A 0x06)
    send cam (Cmd1 0x13)
    send cam (Cmd2 0x1A 0x00)
  either (\e -> camLog cam ("shutdown failed: " ++ show e)) pure r

hex :: B.ByteString -> String
hex = unwords . map byte . B.unpack
  where
    byte w = [digits !! fromIntegral (w `shiftR` 4), digits !! fromIntegral (w .&. 0xF)]
    digits = "0123456789ABCDEF"
