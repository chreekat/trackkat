-- | The tray icon's look, tooltip and menu.
module Test.Tray (tests) where

import Test.Tasty (TestTree)

import Test.Check (check, group)
import TrackKat.Display (CameraState (..))
import TrackKat.Tray

tests :: TestTree
tests = group "Tray" $ do
    let tracking = TrayState Tracking ClipInSight 0 ViewLive Nothing
    check "the cat's eyes say what the camera sees" $
        map
            trayLook
            [tracking, tracking{trayClip = ClipOutOfSight}, tracking{trayCamera = Searching}, tracking{trayCamera = CameraDown "unplugged"}]
            == [LookTracking, LookClipLost, LookSearching, LookDown]
    check "a camera not tracking says so whether or not the clip was last seen" $
        trayLook tracking{trayCamera = Searching, trayClip = ClipOutOfSight} == LookSearching
    check "the tooltip says what the camera sees" $
        map
            trayTip
            [tracking, tracking{trayClip = ClipOutOfSight}, tracking{trayCamera = Searching}, tracking{trayCamera = CameraDown "unplugged"}]
            == ["trackkat\nTracking", "trackkat\nClip out of sight", "trackkat\nLooking for the camera", "trackkat\nNo camera"]
    check "the tooltip adds games, pausing and an update, in that order" $
        trayTip tracking{trayGames = 2, trayHolding = ViewHeld, trayOffer = Just "Restart to update to 0.1.152"}
            == "trackkat\nTracking\nGame connected\nPaused\nUpdate ready: restart to update"
    check "the menu opens, recenters, pauses, trims and quits" $
        trayMenu tracking
            == [ MenuItem OpenChoice "&Open trackkat" Unticked
               , MenuItem RecenterChoice "&Recenter" Unticked
               , MenuItem PauseChoice "&Pause" Unticked
               , MenuItem TrimChoice "&Trim: make this view forward" Unticked
               , MenuSeparator
               , MenuItem QuitChoice "&Quit" Unticked
               ]
    check "Pause is ticked while the view is held" $
        [t | MenuItem PauseChoice _ t <- trayMenu tracking{trayHolding = ViewHeld}] == [Ticked]
    check "an offer is its own item, above Quit" $
        drop 4 (trayMenu tracking{trayOffer = Just "Restart to update to 0.1.152"})
            == [MenuSeparator, MenuItem OfferChoice "Restart to update to 0.1.152" Unticked, MenuSeparator, MenuItem QuitChoice "&Quit" Unticked]
