-- | How the window scrolls.
module Test.UI.Window (tests) where

import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (choose, elements, forAll, oneof, testProperty)

import Test.Check (check, group, tree)
import TrackKat.UI.Window

tests :: TestTree
tests = group "Window" $ do
    let page = 300
        most = contentHeight - page
    check "a notch of the wheel towards the user scrolls the content up 72 pixels at 96 dpi, and back" $
        scrolled 96 page 0 (Wheel (-120)) == 72 && scrolled 96 page 72 (Wheel 120) == 0
    check "a page down moves a window's height" $
        scrolled 96 page 0 PageDown == page
    check "the ends go to the top and as far down as the content goes" $
        scrolled 96 page 100 ToTop == 0 && scrolled 96 page 0 ToBottom == most
    check "a window taller than its content doesn't scroll" $
        scrolled 96 (contentHeight + 50) 0 ToBottom == 0
    tree . testProperty "the content always fills the window from its top, as far as it goes" $
        forAll (elements [96, 120, 144]) $ \dpi ->
            forAll (choose (100, 2000)) $ \height ->
                forAll (choose (-500, 3000)) $ \from ->
                    forAll (oneof [elements [LineUp, LineDown, PageUp, PageDown, ToTop, ToBottom], ScrollTo <$> choose (-500, 3000), Wheel <$> choose (-1200, 1200)]) $ \how ->
                        let to = scrolled dpi height from how
                         in to >= 0 && to <= max 0 ((contentHeight * dpi + 48) `div` 96 - height)
