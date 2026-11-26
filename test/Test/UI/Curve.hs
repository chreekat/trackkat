-- | What an axis's response plot shows.
module Test.UI.Curve (tests) where

import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (Positive (..), testProperty)

import Test.Check (check, group, tree)
import TrackKat.UI.Curve

tests :: TestTree
tests = group "Curve plot" $ do
    let plot text = curvePlot 180 text 0 (64, 64) 2
        plain = fmap plotPlain . plotted . plot
        plotted (Plotted p) = Just p
        plotted Unreadable = Nothing
    check "text that isn't a curve plots as unreadable" $
        plot "1:six" == Unreadable
    check "the head at centre is at the plot's centre" $
        fmap plotHead (plotted (plot "1:6, curve 40")) == Just (32, 32)
    check "the multiplier alone is shown beside a curve, not beside a plain multiplier or points" $
        map (fmap (/= Nothing) . plain) ["1:6, curve 40", "1:6", "1:6, curve 0", "10:10 45:120"] == map Just [True, False, False, False]
    check "a curve is drawn across the plot, its middle at the centre" $
        fmap ((\ps -> (fst (ps !! 0), fst (ps !! 128), ps !! 64)) . plotCurve) (plotted (plot "1:6, curve 40")) == Just (0, 64, (32, 32))
    tree . testProperty "the head is on the plot however far it goes" $ \x ->
        case curvePlot 180 "1:6, curve 40" x (64, 48) 2 of
            Plotted p -> let (hx, hy) = plotHead p in hx >= 0 && hx < 64 && hy >= 0 && hy < 48
            Unreadable -> False
    tree . testProperty "a curve stays within a plot's height of it, however steep" $ \(Positive n) ->
        case curvePlot 180 ("1:1 2:" ++ show (n * 1000 :: Int)) 0 (64, 48) 2 of
            Plotted p -> all (\(_, y) -> y >= -48 && y <= 96) (plotCurve p)
            Unreadable -> False
