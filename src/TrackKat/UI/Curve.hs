-- | What an axis's response plot shows. It is drawn by "TrackKat.Gui.Curve".
module TrackKat.UI.Curve (
    Point,
    CurvePlot (..),
    Plot (..),
    curvePlot,
) where

import TrackKat.Config (parseCurve)
import TrackKat.Response (Curve (..), Multiplier (..), applyCurve)
import TrackKat.UI.Draw (Point, lround)

data CurvePlot = Unreadable | Plotted Plot
    deriving (Eq, Show)

data Plot = Plot
    { plotPlain :: Maybe (Point, Point)
    -- ^ the multiplier alone, end to end, beside a curve made of it
    , plotCurve :: [Point]
    , plotHead :: Point
    -- ^ where the head is, and where the curve puts the view
    }
    deriving (Eq, Show)

-- | An axis's curve as typed, plotted in a width and height with a margin
-- above and below, given the end of its view's range and where the head is.
-- Up, the view from the end of its range on one side to the other; across,
-- the head a quarter further than the multiplier takes to reach it, so the
-- curve's shape fills the plot whatever the multiplier.
curvePlot :: Double -> String -> Double -> (Int, Int) -> Int -> CurvePlot
curvePlot range text x (w, h) margin = case parseCurve range text of
    Left _ -> Unreadable
    Right c ->
        Plotted
            Plot
                { plotPlain = case c of
                    Expo m | precision m > 0 -> let y = applyCurve (Expo m{precision = 0}) reach * sy in Just ((0, lround (h' / 2 + y)), (w, lround (h' / 2 - y)))
                    _ -> Nothing
                , plotCurve =
                    [ (lround (w' / 2 + u * sx), lround (h' / 2 - clamp (-h') h' (applyCurve c u * sy)))
                    | i <- [0 .. 128 :: Int]
                    , let u = reach * (fromIntegral i / 64 - 1)
                    ]
                , plotHead = (lround (clamp 0 (w' - 1) (w' / 2 + x * sx)), lround (clamp 0 (h' - 1) (h' / 2 - applyCurve c x * sy)))
                }
      where
        reach = 1.25 * range / (if slope c > 0 then slope c else 1)
        sx = w' / 2 / reach
        sy = (h' / 2 - fromIntegral margin) / range
  where
    w' = fromIntegral w
    h' = fromIntegral h
    slope (Expo m) = factor m
    slope (Points ps) = case reverse ps of (i, o) : _ -> o / i; [] -> 1
    clamp lo hi = max lo . min hi
