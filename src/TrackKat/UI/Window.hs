-- | trackkat's window as a whole: its content's size, and how it scrolls
-- when the window is shorter.
module TrackKat.UI.Window (
    contentWidth,
    contentHeight,
    Scroll (..),
    scrolled,
) where

import TrackKat.UI.Draw (scaledAt)

-- | The window's content, in pixels at 96 dpi, as cbits/gui.c's CLIENT_W
-- and CLIENT_H.
contentWidth, contentHeight :: Int
contentWidth = 720
contentHeight = 896

-- | A way to scroll the window, as its scroll bar and mouse wheel ask.
data Scroll
    = LineUp
    | LineDown
    | PageUp
    | PageDown
    | ToTop
    | ToBottom
    | -- | to a distance from the top
      ScrollTo Int
    | -- | the wheel turned, positive away from the user, by 120 a notch
      Wheel Int
    deriving (Show)

-- | How far the content is scrolled up, at a dpi, in a window of a height,
-- once a scroll takes it on from where it was.
scrolled :: Int -> Int -> Int -> Scroll -> Int
scrolled dpi height from how = max 0 (min (s contentHeight - height) to)
  where
    s = scaledAt dpi
    to = case how of
        LineUp -> from - s 24
        LineDown -> from + s 24
        PageUp -> from - height
        PageDown -> from + height
        ToTop -> 0
        ToBottom -> s contentHeight
        ScrollTo at -> at
        Wheel delta -> from - delta * s 72 `quot` 120
