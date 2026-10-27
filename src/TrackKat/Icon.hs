-- | trackkat's tray icon: the cat of icons/tray.svg, coloured by state,
-- green tracking, amber with the clip out of sight, grey looking for the
-- camera, red without one; with a white dot when a game is connected (like
-- the camera's second light) and white bars when paused.
module TrackKat.Icon (
    Icon (..),
    Look (..),
    Mark (..),
    statusIcon,
    statusTip,
    iconPixels,
    catPathData,
) where

import qualified Data.ByteString as B
import Data.List (intercalate)
import Data.Word (Word8)

import TrackKat.Status

data Icon = Icon Look Mark
    deriving (Eq, Show)

-- | The disc's colour.
data Look = LookTracking | LookClipLost | LookSearching | LookDown
    deriving (Eq, Show)

-- | What is drawn on the disc.
data Mark = NoMark | GameMark | PausedMark
    deriving (Eq, Show)

statusIcon :: Status -> Icon
statusIcon st = Icon look mark
  where
    look = case (stCamera st, stSighting st) of
        (Tracking, InSight _ _) -> LookTracking
        (Tracking, OutOfSight _) -> LookClipLost
        (Searching, _) -> LookSearching
        (CameraDown _, _) -> LookDown
    mark = case stOutput st of
        Paused -> PausedMark
        Live | stGames st > 0 -> GameMark
        Live -> NoMark

-- | The tray icon's tooltip: a line each for trackkat, the camera, and any
-- game or pause.
statusTip :: Status -> String
statusTip st =
    intercalate "\n" $
        ["trackkat", what]
            ++ ["Game connected" | stGames st > 0]
            ++ ["Paused" | Paused <- [stOutput st]]
  where
    what = case statusIcon st of
        Icon LookTracking _ -> "Tracking"
        Icon LookClipLost _ -> "Clip out of sight"
        Icon LookSearching _ -> "Looking for the camera"
        Icon LookDown _ -> "No camera"

-- | The icon drawn at a size in pixels, as rows of red, green, blue and
-- alpha bytes from the top: the cat filled with the state's colour, with a
-- dark outline so that it shows on light and dark taskbars alike, and the
-- mark on its face. Antialiased by sampling each pixel 4 by 4 times.
iconPixels :: Int -> Icon -> B.ByteString
iconPixels size (Icon look mark) = B.pack (concat [pixel i j | j <- [0 .. size - 1], i <- [0 .. size - 1]])
  where
    s = fromIntegral size :: Double
    -- The cat, outline included, spans about 224 of the drawing's 256
    -- units, centred on (128, 138).
    k = s / 224
    toView (x, y) = ((x - s / 2) / k + 128, (y - s / 2) / k + 138)
    -- At least a pixel and a quarter of outline, however small the icon.
    halfOutline = max 5 (0.625 / k)
    outline = flatten catPath
    samples = [(fromIntegral a + 0.5) / 4 | a <- [0 .. 3 :: Int]]
    (fr, fg, fb) = colour look
    pixel :: Int -> Int -> [Word8]
    pixel i j =
        let colours =
                [ sampleAt (fromIntegral i + u, fromIntegral j + v)
                | u <- samples
                , v <- samples
                ]
            covered = [c | Just c <- colours]
            n = fromIntegral (length colours)
            alpha = fromIntegral (length covered) / n
            average f = if null covered then 0 else sum (map f covered) / fromIntegral (length covered)
         in [byte (average (\(r, _, _) -> r)), byte (average (\(_, g, _) -> g)), byte (average (\(_, _, b) -> b)), byte (alpha * 255)]
    sampleAt p
        | markAt p = Just (255, 255, 255)
        | edge < halfOutline = Just (16, 16, 16)
        | inside = Just (fr, fg, fb)
        | otherwise = Nothing
      where
        q = toView p
        inside = contains outline q
        edge = distanceTo outline q
    -- The mark, on the cat's face, in pixels from the face's centre.
    markAt (x, y) =
        let (cx, cy) = (s / 2, s / 2 + 10 * k)
            dx = x - cx
            dy = y - cy
         in case mark of
                NoMark -> False
                GameMark -> dx * dx + dy * dy <= (0.17 * s) ^ (2 :: Int)
                PausedMark -> abs (abs dx - s * 0.12) <= s * 0.06 && abs dy <= s * 0.2
    byte :: Double -> Word8
    byte = truncate . max 0 . min 255

colour :: Look -> (Double, Double, Double)
colour LookTracking = (60, 180, 100)
colour LookClipLost = (235, 160, 40)
colour LookSearching = (140, 145, 155)
colour LookDown = (215, 70, 60)

type Point = (Double, Double)

-- | A path's pieces after its start, as SVG draws them.
data Segment = LineTo Point | QuadTo Point Point | CubicTo Point Point Point

-- | The cat's outline, in a 256 by 256 drawing: as icons/tray.svg draws
-- it, which the tests check.
catPath :: (Point, [Segment])
catPath =
    ( (36, 168)
    ,
        [ CubicTo (36, 126) (44, 104) (50, 94)
        , LineTo (52, 46)
        , QuadTo (54, 24) (74, 36)
        , LineTo (106, 62)
        , CubicTo (118, 58) (138, 58) (150, 62)
        , LineTo (182, 36)
        , QuadTo (202, 24) (204, 46)
        , LineTo (206, 94)
        , CubicTo (212, 104) (220, 126) (220, 168)
        , CubicTo (220, 220) (180, 244) (128, 244)
        , CubicTo (76, 244) (36, 220) (36, 168)
        ]
    )

-- | The cat's outline as an SVG path's d attribute.
catPathData :: String
catPathData = unwords (("M" ++ point start) : map piece segments ++ ["Z"])
  where
    (start, segments) = catPath
    point (x, y) = number x ++ " " ++ number y
    number v = show (round v :: Int)
    piece (LineTo p) = "L" ++ point p
    piece (QuadTo c p) = "Q" ++ point c ++ " " ++ point p
    piece (CubicTo c d p) = "C" ++ point c ++ " " ++ point d ++ " " ++ point p

-- | A closed path as a polygon, its curves as 16 straight pieces each.
flatten :: (Point, [Segment]) -> [Point]
flatten (start, segments) = start : concat (zipWith pieces (start : map end segments) segments)
  where
    end (LineTo p) = p
    end (QuadTo _ p) = p
    end (CubicTo _ _ p) = p
    steps = [fromIntegral n / 16 | n <- [1 .. 16 :: Int]]
    pieces _ (LineTo p) = [p]
    pieces a (QuadTo c p) = [bezier [a, c, p] t | t <- steps]
    pieces a (CubicTo c d p) = [bezier [a, c, d, p] t | t <- steps]
    bezier [p] _ = p
    bezier ps t = bezier (zipWith (\(x0, y0) (x1, y1) -> (x0 + (x1 - x0) * t, y0 + (y1 - y0) * t)) ps (drop 1 ps)) t

-- | The polygon's edges, closing it.
edges :: [Point] -> [(Point, Point)]
edges ps = zip ps (drop 1 ps ++ take 1 ps)

contains :: [Point] -> Point -> Bool
contains ps (x, y) = odd (length [() | ((x0, y0), (x1, y1)) <- edges ps, (y0 > y) /= (y1 > y), x < x0 + (y - y0) * (x1 - x0) / (y1 - y0)])

distanceTo :: [Point] -> Point -> Double
distanceTo ps (x, y) = minimum (map from (edges ps))
  where
    from ((x0, y0), (x1, y1)) =
        let (dx, dy) = (x1 - x0, y1 - y0)
            len2 = dx * dx + dy * dy
            t = if len2 == 0 then 0 else max 0 (min 1 (((x - x0) * dx + (y - y0) * dy) / len2))
            (px, py) = (x0 + t * dx - x, y0 + t * dy - y)
         in sqrt (px * px + py * py)
