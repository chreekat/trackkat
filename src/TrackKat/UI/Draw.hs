-- | Pictures of parts of trackkat's window, as shapes in the order they are
-- drawn, each over those before it: what "TrackKat.Gui.Paint" draws.
module TrackKat.UI.Draw (
    Colour (..),
    Point,
    Rect,
    Stroke (..),
    Align (..),
    Font (..),
    Shape (..),
    scaledAt,
    lround,
) where

-- | Red, green and blue, each 0 to 255.
data Colour = RGB !Int !Int !Int
    deriving (Eq, Show)

-- | Pixels across and down from the top left.
type Point = (Int, Int)

-- | Left, top, right and bottom, the right and bottom just outside.
type Rect = (Int, Int, Int, Int)

data Stroke = Solid | Dotted | Dashed
    deriving (Eq, Show)

data Align = AlignLeft | AlignCentre | AlignRight
    deriving (Eq, Show)

-- | The window's fonts, as cbits/gui.c's hts_gui_font numbers them.
data Font = UiFont | BoldFont | SmallFont | CapsFont | TinyFont | MonoFont
    deriving (Eq, Show, Enum)

data Shape
    = Fill Colour Rect
    | -- | an outline a pixel wide just inside the rectangle
      Frame Colour Rect
    | -- | through the points, of a width
      Line Stroke Int Colour [Point]
    | -- | a fill; an outline's width and colour; the centre and radius
      Disc Colour Int Colour Point Int
    | -- | an outline's stroke, width and colour; the centre and radius
      Ring Stroke Int Colour Point Int
    | -- | filled, and outlined a pixel wide, in the colour
      Polygon Colour [Point]
    | -- | a fill or none, an outline a pixel wide, and corners rounded to a
      -- diameter
      Rounded (Maybe Colour) Colour Int Rect
    | -- | the dotted rectangle around what has the keyboard
      Focus Rect
    | -- | on one line, centred down the rectangle, ended with an ellipsis if
      -- it doesn't fit, with extra pixels between letters
      Text Font Colour Int Align Rect String
    | -- | broken into lines at spaces, from the top left
      Paragraph Font Colour Rect String
    | -- | the shapes, cut to the rectangle
      Clipped Rect [Shape]
    deriving (Eq, Show)

-- | A length at 96 dots per inch, at a number of dots per inch, to the
-- nearest pixel as cbits/gui.c's S scales it.
scaledAt :: Int -> Int -> Int
scaledAt dpi x = (x * dpi + 48) `div` 96

-- | To the nearest pixel, halves away from zero, as cbits/gui.c rounds.
lround :: Double -> Int
lround v = truncate (v + if v < 0 then -0.5 else 0.5)
