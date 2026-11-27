-- | The settings as the window keeps them.
module Test.UI.Settings (tests) where

import Test.Tasty (TestTree)

import Test.Check (check, group)
import TrackKat.UI.Settings

tests :: TestTree
tests = group "Settings" $ do
    let pushed = foldl (\b (n, v) -> push n v b) emptyDesk
    check "the file's pushes are shown once each, the latest of each setting" $
        let (shown, desk) = takePushes [] (pushed [("startup", "on"), ("updates", "off"), ("startup", "off")])
         in shown == [("updates", "off"), ("startup", "off")] && fst (takePushes [] desk) == []
    check "a push to a setting changed in the window and not yet saved is dropped" $
        fst (takePushes ["startup"] (pushed [("startup", "on"), ("updates", "off")])) == [("updates", "off")]
    check "the window's saved changes are taken once each, the latest of each, and only those asked for" $
        let desk = saveValues [("startup", "on")] (saveValues [("startup", "off"), ("lean", "view")] emptyDesk)
            (taken, rest) = takeSaved ["startup", "updates"] desk
         in taken == [("startup", "on")] && fst (takeSaved ["startup", "lean"] rest) == [("lean", "view")]
    check "a switch reads its own words, and anything else as its default" $
        map (toggleChecked (Toggle "trackir" "trackkat" Unchecked)) ["trackir", "trackkat", ""] == [Checked, Unchecked, Unchecked]
            && map (toggleChecked (Toggle "view" "screen" Checked)) ["view", "screen", "?"] == [Checked, Unchecked, Checked]
    check "a switch writes its own words" $
        map (toggleValue (Toggle "on" "off" Unchecked)) [Checked, Unchecked] == ["on", "off"]
