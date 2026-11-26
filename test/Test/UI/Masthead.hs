-- | What the masthead across the top of the window says.
module Test.UI.Masthead (tests) where

import Test.Tasty (TestTree)

import Test.Check (check, group)
import TrackKat.Display (UpdateCheck (..), UpdateStatus (..))
import TrackKat.UI.Masthead

tests :: TestTree
tests = group "Masthead" $ do
    let quiet = MastheadState "0.1.151 (4f2a9c1)" Nothing CheckIdle Nothing
    check "the version is the build's number, its commit left to the title" $
        map (mastheadVersion . (\v -> quiet{mastheadBuild = v})) ["0.1.151 (4f2a9c1)", "development build"] == ["0.1.151", "development build"]
    check "before any look for a newer version, it says nothing more" $
        mastheadSays quiet == SaysNothing
    check "up to date is good news; anything else is plain" $
        map (\s -> mastheadSays quiet{mastheadStatus = Just s}) [UpToDate "Up to date (0.1.151).", UpdateNote "Can't look: no network."]
            == [SaysStatus Good "Up to date (0.1.151).", SaysStatus Plain "Can't look: no network."]
    check "a look running says so, whatever the last one found" $
        mastheadSays quiet{mastheadCheck = CheckRunning, mastheadStatus = Just (UpToDate "Up to date (0.1.151).")} == SaysStatus Plain "Checking\8230"
    check "an offer is all it says" $
        mastheadSays quiet{mastheadOffer = Just "0.1.152 is ready.", mastheadCheck = CheckRunning} == SaysOffer "0.1.152 is ready."
