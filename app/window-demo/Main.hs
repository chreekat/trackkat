{-# LANGUAGE LambdaCase #-}

-- | trackkat-window-demo: trackkat's window driven by a scripted head and
-- status, for working on the window and taking screenshots without a
-- camera, on any platform. Prints what the window asks for; settings are
-- saved to a copy of the default settings kept in memory.
--
-- usage: trackkat-window-demo [--still SECONDS] [--trimmed]
--
-- --still holds the script at that moment; --trimmed starts with the view
-- trimmed to look at a left MFD.
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (poll, withAsyncBound)
import Control.Exception (displayException)
import Control.Monad (forM_, forever, when)
import Data.IORef
import GHC.Clock (getMonotonicTime)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hFlush, hPutStrLn, stderr, stdout)

import TrackKat.Config
import TrackKat.Number (parseNumber)
import TrackKat.Pose (HeadPose (..))
import TrackKat.Response
import TrackKat.Status
import TrackKat.Tracker (Seen (..))
import TrackKat.Window

data Options = Options
    { still :: Maybe Double
    , trimmed :: Bool
    }

options :: [String] -> Maybe Options
options = go (Options Nothing False)
  where
    go o ("--still" : t : rest) = parseNumber t >>= \s -> go o{still = Just s} rest
    go o ("--trimmed" : rest) = go o{trimmed = True} rest
    go o [] = Just o
    go _ _ = Nothing

main :: IO ()
main = do
    opts <-
        getArgs >>= \args -> case options args of
            Just o -> pure o
            Nothing -> hPutStrLn stderr "usage: trackkat-window-demo [--still SECONDS] [--trimmed]" >> exitFailure
    w <- newWindow
    let say l = putStrLn l >> hFlush stdout >> logLine w l
        settingsText =
            editSettings
                [("recenter", "F12, Throttle - HOTAS Warthog button 23"), ("trim", "Ctrl+F12"), ("yaw", "1:6, precision 40")]
                (renderConfig defaultConfig)
    file <- newIORef (either error id settingsText)
    readIORef file >>= showSettings w . settingValues
    mapM_
        say
        [ "Games will load trackkat's NPClient from C:\\Games\\trackkat"
        , "Joystick connected: Throttle - HOTAS Warthog"
        , "Recenter: F12, Throttle - HOTAS Warthog button 23   Pause: F9   Trim: Ctrl+F12   (Enter also recenters, c Enter calibrates level)"
        , "Tracking."
        , "Level learnt: camera tipped 12.4 degrees down and 0.8 to its right, from 1460 degrees of head turning."
        , "Neck pivot learnt: 92 mm below the clip, 71 behind it and 3 to its left, from 214 head turns."
        ]
    let forward = Trim (-42) 18
    when (trimmed opts) $ say "Trimmed: forward is now yaw -42 pitch +18."
    updateStatus w $ \st ->
        st
            { stCamera = Tracking
            , stGames = 1
            , stTrim = if trimmed opts then forward else NoTrim
            , stStats = "latency 0.11 ms (max 0.42)  gap max 9 ms  stripes max 31  GC 3 (max 0.21 ms)"
            , stCalibration =
                "Level learnt: camera tipped 12.4 degrees down and 0.8 to its right, from 1460 degrees of head turning.\n"
                    ++ "Neck pivot learnt: 92 mm below the clip, 71 behind it and 3 to its left, from 214 head turns."
            }
    capturedSince <- newIORef Nothing
    withAsyncBound (runWindow w OnScreen) $ \running -> do
        start <- getMonotonicTime
        forever $ do
            poll running >>= \case
                Nothing -> pure ()
                Just (Right (WindowFailure why)) -> hPutStrLn stderr ("No window: " ++ why) >> exitFailure
                Just (Left e) -> hPutStrLn stderr ("The window stopped: " ++ displayException e) >> exitFailure
            now <- getMonotonicTime
            let t = maybe (now - start) id (still opts)
            config <- either (const defaultConfig) id . parseConfig <$> readIORef file
            updateStatus w (script (response config) t)
            actions <- takeActions w
            forM_ actions $ \a -> do
                say ("Window asked: " ++ show a)
                case a of
                    RecenterAction -> updateStatus w (\st -> st{stTrim = NoTrim}) >> say "Recentered."
                    PauseAction -> updateStatus w $ \st -> st{stOutput = case stOutput st of Live -> Paused; Paused -> Live}
                    TrimAction -> updateStatus w (\st -> st{stTrim = forward}) >> say "Trimmed: forward is now yaw -42 pitch +18."
                    CalibrateAction -> say "Calibrating: turn your head left and right."
                    SaveAction edits -> do
                        old <- readIORef file
                        case editSettings edits old of
                            Left err -> showSettingsMessage w ("Not saved: " ++ err)
                            Right new
                                | new == old -> showSettingsMessage w "Nothing has changed."
                                | otherwise -> do
                                    writeIORef file new
                                    showSettings w (settingValues new)
                                    showSettingsMessage w "Saved."
            -- Any key or button the window waits for is, a second later,
            -- Ctrl+F7.
            capturing w >>= \case
                Nothing -> writeIORef capturedSince Nothing
                Just k ->
                    readIORef capturedSince >>= \case
                        Nothing -> writeIORef capturedSince (Just now)
                        Just since -> when (now - since > 1) $ addBinding w k (Key [Ctrl] "F7")
            threadDelay 16000

-- | The scripted head at a time in seconds: looking around, with the clip
-- out of sight for two seconds in every twelve, and a reflector hidden
-- behind a stray light for a while before that.
script :: Response -> Double -> Status -> Status
script r t st =
    st
        { stSighting = if lost then OutOfSight 2 else InSight pose seen
        , stView = case stOutput st of
            Paused -> stView st
            Live | lost -> stView st
            Live -> leaning (applyTrim (stTrim st) (curved pose))
        }
  where
    phase = t - 12 * fromIntegral (floor (t / 12) :: Int)
    lost = phase > 10
    leaning = case lean r of
        LeanWithView -> leanWithView
        LeanWithScreen -> id
    seen = if phase > 7 then Seen 1 0 1 else Seen 0 0 0
    pose =
        HeadPose
            { yaw = 14 * sin (t * 0.9)
            , pitch = -6 + 5 * sin (t * 1.3)
            , roll = 3 * sin (t * 0.7)
            , tx = 10 * sin (t * 0.5)
            , ty = -4 * sin (t * 0.8)
            , tz = 15 * sin (t * 0.4)
            }
    curved p =
        HeadPose
            (applyCurve (yawCurve r) (yaw p))
            (applyCurve (pitchCurve r) (pitch p))
            (applyCurve (rollCurve r) (roll p))
            (applyCurve (xCurve r) (tx p))
            (applyCurve (yCurve r) (ty p))
            (applyCurve (zCurve r) (tz p))
