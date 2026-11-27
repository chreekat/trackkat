-- | The tray icon's look, tooltip and menu.
module Test.UI.Tray (tests) where

import Test.Tasty (TestTree)

import Test.Check (check, group)
import TrackKat.Display (CameraState (..), Notice (..), Output (..), Shown (..), Sighting (..), nothingShown)
import TrackKat.Pose (HeadPose (..))
import TrackKat.UI.Tray

tests :: TestTree
tests = group "Tray" $ do
    let still = HeadPose 0 0 0 0 0 0
        tracking = nothingShown{shownCamera = Tracking, shownSighting = InSight still []}
        lost = OutOfSight 2
        held = Paused still
        offer = Just (Notice "0.1.152 is ready." "Restart to update" (Just "Restart to update to 0.1.152"))
    check "the cat's eyes say what the camera sees" $
        map
            trayLook
            [tracking, tracking{shownSighting = lost}, tracking{shownCamera = Searching}, tracking{shownCamera = CameraDown "unplugged"}]
            == [LookTracking, LookClipLost, LookSearching, LookDown]
    check "a camera not tracking says so whether or not the clip was last seen" $
        trayLook tracking{shownCamera = Searching, shownSighting = lost} == LookSearching
    check "the tooltip says what the camera sees" $
        map
            trayTip
            [tracking, tracking{shownSighting = lost}, tracking{shownCamera = Searching}, tracking{shownCamera = CameraDown "unplugged"}]
            == ["trackkat\nTracking", "trackkat\nClip out of sight", "trackkat\nLooking for the camera", "trackkat\nNo camera"]
    check "the tooltip adds games, pausing and an update, in that order" $
        trayTip tracking{shownGames = 2, shownOutput = held, shownOffer = offer}
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
        [t | MenuItem PauseChoice _ t <- trayMenu tracking{shownOutput = held}] == [Ticked]
    check "an offer is its own item, above Quit" $
        drop 4 (trayMenu tracking{shownOffer = offer})
            == [MenuSeparator, MenuItem OfferChoice "Restart to update to 0.1.152" Unticked, MenuSeparator, MenuItem QuitChoice "&Quit" Unticked]
