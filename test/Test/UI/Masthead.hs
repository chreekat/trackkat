-- | What the masthead across the top of the window says.
module Test.UI.Masthead (tests) where

import Test.Tasty (TestTree)

import Test.Check (check, group)
import TrackKat.Display (Notice (..), Shown (..), UpdateCheck (..), UpdateStatus (..), nothingShown)
import TrackKat.UI.Masthead

tests :: TestTree
tests = group "Masthead" $ do
    let quiet = nothingShown{shownVersion = "0.1.151 (4f2a9c1)"}
    check "the version is the build's number, its commit left to the title" $
        map (mastheadVersion . (\v -> quiet{shownVersion = v})) ["0.1.151 (4f2a9c1)", "development build"] == ["0.1.151", "development build"]
    check "before any look for a newer version, it says nothing more" $
        mastheadSays quiet == SaysNothing
    check "up to date is good news; anything else is plain" $
        map (\s -> mastheadSays quiet{shownStatus = Just s}) [UpToDate "Up to date (0.1.151).", UpdateNote "Can't look: no network."]
            == [SaysStatus Good "Up to date (0.1.151).", SaysStatus Plain "Can't look: no network."]
    check "a look running says so, whatever the last one found" $
        mastheadSays quiet{shownCheck = CheckRunning, shownStatus = Just (UpToDate "Up to date (0.1.151).")} == SaysStatus Plain "Checking\8230"
    check "an offer is all it says" $
        mastheadSays quiet{shownOffer = Just (Notice "0.1.152 is ready." "Restart to update" Nothing), shownCheck = CheckRunning} == SaysOffer "0.1.152 is ready."
