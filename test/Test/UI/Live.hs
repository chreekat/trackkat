-- | What the live view shows beside the head view.
module Test.UI.Live (tests) where

import Test.Tasty (TestTree)

import Test.Check (check, group)
import TrackKat.Display (CameraState (..))
import TrackKat.Pose (HeadPose (..))
import TrackKat.Response (Aiming (..))
import TrackKat.UI.Draw
import TrackKat.UI.Live
import TrackKat.UI.Tray (Holding (..))

tests :: TestTree
tests = group "Live view" $ do
    let still = HeadPose 0 0 0 0 0 0
        st =
            LiveState
                { liveCamera = Tracking
                , liveClip = ClipSeen
                , liveHead = still
                , liveView = still
                , liveHolding = ViewLive
                , liveHeld = (0, 0)
                , liveTrim = NotTrimmed
                , liveAiming = NotAiming
                , liveGames = 0
                , liveGameName = Nothing
                , liveStats = "latency 0.11 ms"
                }
        says s = [t | Text _ _ _ _ _ t <- livePane 96 (700, 296) s]
    check "figures are written as printf writes them, halves to even and a minus for anything below zero" $
        [axisFigure 0 12.25, axisFigure 0 (-0.04), axisFigure 3 2.5, axisFigure 3 3.5, axisFigure 5 (-0.4)]
            == ["+12.2\176", "-0.0\176", "+2 mm", "+4 mm", "-0 mm"]
    check "the lights say what the camera, the clip and the games are doing" $
        all (`elem` says st{liveGames = 2, liveGameName = Just "DCS"}) ["Connected", "In sight", "DCS and 1 more"]
            && all (`elem` says st{liveClip = ClipLost 2, liveGames = 1}) ["Out of sight (2 of 3)", "1 connected"]
            && all (`elem` says st{liveCamera = CameraDown "", liveClip = ClipLost 0}) ["Not found", "\8212", "None"]
    check "the figures show only while the camera tracks" $
        "latency 0.11 ms" `elem` says st && "latency 0.11 ms" `notElem` says st{liveCamera = Searching}
    check "each switch says what it does, and what holds while it is lit" $
        map (switchWords st) [minBound .. maxBound] == [("RECENTER", ""), ("PAUSE", ""), ("TRIM", ""), ("AIM", "")]
            && map
                (switchWords st{liveHolding = ViewHeld, liveHeld = (-3.5, 12.4), liveTrim = Trimmed (-42) 18 MovesToo, liveAiming = Aiming})
                [PauseSwitch, TrimSwitch, AimSwitch]
                == [("PAUSED", "yaw \8722\&4 pitch +12"), ("TRIM", "yaw \8722\&42 pitch +18 \183 moved"), ("AIMING", "Sensitivity reduced")]
    check "a lit switch glows inside its edge; an unlit one doesn't" $
        let rounded s w = length [() | Rounded _ _ _ _ <- switchFace 96 (0, 0, 160, 52) Released s w]
         in (rounded st AimSwitch, rounded st{liveAiming = Aiming} AimSwitch) == (1, 6)
