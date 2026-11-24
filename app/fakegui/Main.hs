{-# LANGUAGE LambdaCase #-}

-- | fakegui: drive trackkat's window and tray icon ("TrackKat.Gui") with a
-- scripted pose and status, for working on the GUI without a camera. Prints
-- what the user asks for. Keys chosen for the bindings are all Ctrl+F7. A
-- second game, unnamed, comes and goes; the clip is calibrated, and every
-- twelve seconds its top reflector is out of sight for two, then the clip
-- for two more; a lamp is ignored, and a stray glint comes and goes; Check
-- for updates finds a newer version the second time, which Restart to
-- update takes back.
--
-- usage: fakegui [c | haskell] [seconds]: the window in C by default, as
-- trackkat's settings have it.
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, when)
import Data.Fixed (mod')
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hFlush, hPutStrLn, stderr, stdout)

import TrackKat.Config (KeyOrButton (..), Modifier (..), WhichWindow (..))
import TrackKat.Display
import TrackKat.Frame (Blob (..))
import TrackKat.Gui
import TrackKat.Linear (V3 (..))
import TrackKat.Pose (HeadPose (..), Reflector (..))
import TrackKat.Response (Aiming (..), Trim (..))
import TrackKat.Trackers (BlobRole (..))

settings :: [(String, String)]
settings =
    [ ("recenter", "F12, Throttle - HOTAS Warthog button 4 double")
    , ("pause", "Ctrl+F9")
    , ("trim", "Throttle - HOTAS Warthog button 4")
    , ("aim", "Throttle - HOTAS Warthog button 4 hold")
    , ("smoothing", "on")
    , ("steadiness", "90")
    , ("responsiveness", "45")
    , ("yaw", "1:6, curve 40")
    , ("pitch", "1:7")
    , ("roll", "1:4")
    , ("x", "1:3")
    , ("y", "1:4")
    , ("z", "1:2.5")
    , ("lean", "view")
    , ("npclient", "trackkat")
    , ("head view", "virtual")
    ]

-- | The state the script carries from frame to frame.
data Script = Script
    { sPaused :: Bool
    , sAiming :: Aiming
    , sChecking :: Int
    -- ^ frames left of a look for a newer version, or 0
    , sChecks :: Int
    -- ^ looks so far
    }

main :: IO ()
main = do
    (which, seconds) <-
        getArgs >>= \case
            "haskell" : rest -> pure (HaskellWindow, secondsIn rest)
            "c" : rest -> pure (CWindow, secondsIn rest)
            rest -> pure (CWindow, secondsIn rest)
    showUpdateButton UpdateButtonEnabled
    withGui which WindowShown $ \started -> do
        either (\e -> hPutStrLn stderr ("could not start the GUI (error " ++ show e ++ ")") >> exitFailure) pure started
        script seconds

secondsIn :: [String] -> Int
secondsIn [s] = read s
secondsIn _ = 30

script :: Int -> IO ()
script seconds = do
    forM_ settings (uncurry showSetting)
    guiLog "Games will load NPClient from C:\\Games\\trackkat"
    guiLog "Recenter: F12, Throttle - HOTAS Warthog button 4 double   Pause: Ctrl+F9   Trim: Throttle - HOTAS Warthog button 4   Aim: Throttle - HOTAS Warthog button 4 hold"
    showUpdateStatus (UpToDate "Up to date (0.1.151).")
    guiLog "Tracking."
    showCalibration
        "Camera tipped 12.4\176 down and 0.8\176 to its right, from 1460\176 of head turning.\n\
        \Neck pivot 92\177\&12 mm below the clip, 71\177\&10 behind, 3\177\&10 to your left; 214 turns."
    -- That calibration, with the default clip squarely facing the camera at
    -- the centre: its left, right and top reflectors, from the neck pivot.
    showClip (Just (V3 38 92 71, V3 (-32) 92 71, V3 3 148.2 (-16.9)))
    showCamera Tracking
    run (seconds * 60) 0 (Script False NotAiming 0 0)

run :: Int -> Int -> Script -> IO ()
run frames i s
    | i >= frames = pure ()
    | otherwise = do
        let t = fromIntegral i / 60 :: Double
            headPose = HeadPose (14 * sin (t * 0.9)) (-6 + 5 * sin (t * 1.3)) (3 * sin (t * 0.7)) (10 * sin (t * 0.5)) (-4 * sin (t * 0.8)) (15 * sin (t * 0.4))
            view = HeadPose (yaw headPose * 6) (pitch headPose * 7) (roll headPose * 4) (tx headPose * 3) (ty headPose * 4) (tz headPose * 2.5)
            lost = t `mod'` 12 > 10
            topHidden = t `mod'` 12 > 8
        when (i `mod` 60 == 0) $ showGames (if t `mod'` 20 > 15 then 2 else 1) (Just "DCS")
        showFrame (if lost then OutOfSight 2 else InSight headPose [TopReflector | topHidden]) view
        showBlobs (blobs t headPose lost topHidden) [((155, 292), 30)]
        when (i `mod` 60 == 0) $ showStats "latency 0.11 ms (max 0.42)  gap 9 ms  stripes 31  GC 3 (max 0.21 ms)"
        when (i `mod` 600 == 300) $
            guiLog (if i `mod` 1200 == 300 then "Recentered." else "Joystick button pressed: Throttle - HOTAS Warthog button 23")
        acts <- takeActions
        open <- windowOpen
        when (not (null acts)) $ do
            putStrLn ("actions " ++ show acts ++ ", window " ++ (if open then "open" else "closed"))
            hFlush stdout
        s' <- foldr (\a k st -> act view a st >>= k) pure acts s
        -- Check for updates takes two seconds, and finds this version.
        let counting = if CheckForUpdatesAction `elem` acts then 120 else sChecking s'
            left = max 0 (counting - 1)
        checks <-
            if counting > 0 && left == 0
                then do
                    let n = sChecks s' + 1
                    if n == 1
                        then showUpdateStatus (UpToDate "Up to date (0.1.151).")
                        else do
                            showUpdateStatus (UpdateNote "0.1.152 is ready.")
                            showOffer (Just (Notice "0.1.152 is ready. It starts next time trackkat does." "Restart to update" (Just "Restart to update to 0.1.152")))
                    pure n
                else pure (sChecks s')
        showUpdateCheck (if left > 0 then CheckRunning else CheckIdle)
        if QuitAction `elem` acts
            then pure ()
            else threadDelay 16000 >> run frames (i + 1) s'{sChecking = left, sChecks = checks}

-- | What the script does for something the user asked for.
act :: HeadPose -> Action -> Script -> IO Script
act view a s = case a of
    PauseAction -> do
        let paused = not (sPaused s)
        showOutput (if paused then Paused view else Live)
        pure s{sPaused = paused}
    -- Trimming looks at the left MFD.
    TrimAction -> do
        showTrim (Trim (HeadPose (-42) 18 2 35 (-20) (-60)))
        guiLog "Trimmed: forward is now yaw -42 pitch +18 roll +2, x +35 y -20 z -60 mm."
        pure s
    AimAction -> do
        let aiming = case sAiming s of NotAiming -> Aiming; Aiming -> NotAiming
        showAim aiming
        guiLog (case aiming of Aiming -> "Aiming: sensitivity reduced for finer control."; NotAiming -> "Aiming ended.")
        pure s{sAiming = aiming}
    SaveAction -> do
        saved <- savedSettings (map fst settings)
        forM_ saved $ \(name, value) -> putStrLn ("  " ++ name ++ " = " ++ value)
        showSettingsMessage "Saved."
        pure s
    KeyChosen k chosen -> do
        putStrLn ("  key " ++ show chosen)
        addBinding k (Key [Ctrl] "F7")
        pure s
    OfferAction -> showOffer Nothing >> pure s
    _ -> pure s

-- | The reflectors, wider than tall, move as the head turns; out of sight,
-- what is left of them is taken for strays. The lamp, tall, and its flicker
-- stay put.
blobs :: Double -> HeadPose -> Bool -> Bool -> [(Blob, BlobRole)]
blobs t p lost topHidden =
    [ spot (bx - 28) (by + 24) 350 role 34 22 1
    , spot (bx + 28) (by + 26) 345 role 33 22 (-1)
    ]
        ++ [spot bx (by - 6) 260 ReflectorBlob 24 20 0 | not topHidden]
        ++ [ spot 156 297 712 MaskedBlob 38 82 6
           , spot 150 279 64 MaskedBlob 5 6 0
           ]
        ++ [spot 548 64 18 StrayBlob 1.5 1.5 0 | t `mod'` 5 < 3]
  where
    bx = 296 - yaw p * 5 + tx p
    by = 110 + pitch p * 5 - ty p
    role = if lost then StrayBlob else ReflectorBlob
    spot x y pixels r vx vy cxy = (Blob x y pixels pixels vx vy cxy, r)
