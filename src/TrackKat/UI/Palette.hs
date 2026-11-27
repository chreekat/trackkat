-- | The colours of the dark live view and the response plots, as
-- cbits/gui.c's C_ colours.
module TrackKat.UI.Palette (
    backColour,
    boxColour,
    gridColour,
    textColour,
    dim,
    accent,
    accentFaint,
    headTick,
    green,
    amber,
    red,
    grey,
) where

import TrackKat.UI.Draw (Colour (..))

backColour, boxColour, gridColour, textColour, dim, accent, accentFaint, headTick, green, amber, red, grey :: Colour
backColour = RGB 30 34 40
boxColour = RGB 38 43 51
gridColour = RGB 56 62 72
textColour = RGB 222 226 232
dim = RGB 135 142 154
accent = RGB 90 170 255
accentFaint = RGB 66 112 166
headTick = RGB 255 200 80
green = RGB 80 200 120
amber = RGB 240 170 60
red = RGB 230 85 75
grey = RGB 120 126 138
