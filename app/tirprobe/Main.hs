-- | tirprobe: talk to a TrackIR 5 and print what it sees.
module Main (main) where

import Control.Exception (catch)
import Control.Monad (forM_, when)
import Data.IORef
import GHC.Clock (getMonotonicTime)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO
import Text.Printf (printf)
import Text.Read (readMaybe)

import TrackKat.Camera
import TrackKat.Frame
import qualified TrackKat.USB as USB

data Options = Options
  { optSettings :: Settings
  , optRaw :: Bool
  , optList :: Bool
  }

usage :: String
usage =
  unlines
    [ "usage: tirprobe [--threshold N] [--ir 5|6|7] [--no-status-led] [--raw]"
    , "       tirprobe --list"
    , ""
    , "  --threshold N     pixel brightness cut-off, 30..253 (default 140)"
    , "  --ir N            IR illuminator brightness, 5..7 (default 7)"
    , "  --no-status-led   leave the green light off"
    , "  --raw             also hex-dump every USB transfer"
    , "  --list            list all USB devices and whether we can open them"
    , ""
    , "Press Ctrl-C to stop."
    ]

parseArgs :: [String] -> Options -> Either String Options
parseArgs [] o = Right o
parseArgs ("--raw" : r) o = parseArgs r o {optRaw = True}
parseArgs ("--list" : r) o = parseArgs r o {optList = True}
parseArgs ("--no-status-led" : r) o = parseArgs r o {optSettings = (optSettings o) {statusLed = False}}
parseArgs ("--threshold" : n : r) o | Just v <- readMaybe n = parseArgs r o {optSettings = (optSettings o) {threshold = v}}
parseArgs ("--ir" : n : r) o | Just v <- readMaybe n = parseArgs r o {optSettings = (optSettings o) {irBrightness = v}}
parseArgs (a : _) _ = Left ("unrecognised argument: " ++ a)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  opts <- case parseArgs args (Options defaultSettings False False) of
    Right o -> pure o
    Left err -> hPutStrLn stderr (err ++ "\n\n" ++ usage) >> exitFailure
  let logger = putStrLn . ("# " ++)
  when (optList opts) $ do
    USB.withContext listAll
    exitSuccess
  (USB.withContext $ \ctx -> withCamera ctx logger (optSettings opts) (run opts))
    `catch` \e -> do
      hPutStrLn stderr (explain e)
      exitFailure

explain :: OpenError -> String
explain NoCamera = "No TrackIR 5 (USB 131D:0159) found. Is it plugged in?"
explain (WrongDriver err) =
  "Found the TrackIR, but it is not using the WinUSB driver (" ++ err ++ ").\n\
  \Use Zadig to switch it to WinUSB (see README)."
explain InUse =
  "The TrackIR is in use by another program. Quit the NaturalPoint\n\
  \TrackIR software (check the system tray) and try again."
explain (OpenFailed e) = "USB error: " ++ show e

listAll :: USB.Context -> IO ()
listAll ctx = do
  devs <- USB.listDevices ctx
  forM_ devs $ \d -> do
    status <-
      if USB.devOpenResult d == 0
        then pure "opens OK (WinUSB or similar)"
        else ("cannot open: " ++) <$> USB.errorName (USB.devOpenResult d)
    printf "%04X:%04X  %s%s\n" (USB.devVendor d) (USB.devProduct d) status
      (if USB.devVendor d == vendorId then "   <-- NaturalPoint" else "" :: String)

run :: Options -> Camera -> IO ()
run opts cam = do
  putStrLn "# tracking; Ctrl-C to stop"
  lastPrint <- newIORef 0
  frames <- newIORef (0 :: Int)
  let loop = do
        pkts <- readPackets cam
        forM_ pkts $ \p -> do
          when (optRaw opts) $ putStrLn ("raw " ++ describeRaw p)
          case p of
            Frame n stripes -> do
              modifyIORef' frames (+ 1)
              now <- getMonotonicTime
              t0 <- readIORef lastPrint
              when (now - t0 >= 0.1) $ do
                count <- readIORef frames
                writeIORef frames 0
                writeIORef lastPrint now
                let fps = fromIntegral count / (now - t0) :: Double
                    blobs = stripesToBlobs 4 2500 stripes
                putStrLn (summary n fps blobs)
            Corrupt why _ -> putStrLn ("# corrupt packet: " ++ why)
            _ -> pure ()
        loop
  loop

summary :: Integral a => a -> Double -> [Blob] -> String
summary n fps blobs =
  printf "frame %3d  %5.1f fps  %d blob%s " (fromIntegral n :: Int) (min fps 999) (length blobs)
    (if length blobs == 1 then "" else "s")
    ++ concatMap showBlob blobs
  where
    showBlob b = printf " (%6.1f, %6.1f) %4dpx" (blobX b) (blobY b) (blobPoints b)

describeRaw :: Packet -> String
describeRaw (Frame n ss) = "frame " ++ show n ++ " " ++ show ss
describeRaw (StatusReply b) = "status " ++ hex b
describeRaw (DevInfoReply b) = "devinfo " ++ hex b
describeRaw (Unknown b) = "unknown " ++ hex b
describeRaw (Corrupt why b) = "corrupt (" ++ why ++ ") " ++ hex b
