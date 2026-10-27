-- | trackkat's icon, in the tray and on its window: a disc coloured by
-- state, green tracking, amber with the clip out of sight, grey looking for
-- the camera, red without one; a white dot when a game is connected (like
-- the camera's second light) and white bars when paused.
module TrackKat.Icon (
    Icon (..),
    Look (..),
    Mark (..),
    statusIcon,
    statusTip,
    iconPixels,
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
-- alpha bytes from the top, antialiased, with a darker rim so that it
-- shows on light and dark taskbars alike.
iconPixels :: Int -> Icon -> B.ByteString
iconPixels size (Icon look mark) = B.pack (concat [pixel i j | j <- [0 .. size - 1], i <- [0 .. size - 1]])
  where
    s = fromIntegral size :: Double
    c = s / 2
    r = s / 2 - 0.5
    edge = s / 16
    (fr, fg, fb) = colour look
    pixel :: Int -> Int -> [Word8]
    pixel i j =
        let dx = fromIntegral i + 0.5 - c
            dy = fromIntegral j + 0.5 - c
            d = sqrt (dx * dx + dy * dy)
            a = coverage (r - d + 0.5)
            rim = coverage (d - (r - edge) + 0.5) * 0.45
            white = case mark of
                NoMark -> 0
                GameMark -> coverage (s * 0.2 - d + 0.5)
                PausedMark ->
                    let bx = abs (abs dx - s * 0.14)
                     in coverage (s * 0.07 - bx + 0.5) * coverage (s * 0.24 - abs dy + 0.5)
            channel f = let v = f * (1 - rim) in byte (v + (255 - v) * white)
         in [channel fr, channel fg, channel fb, byte (a * 255)]
    coverage = max 0 . min 1
    byte :: Double -> Word8
    byte = truncate . max 0 . min 255

colour :: Look -> (Double, Double, Double)
colour LookTracking = (60, 180, 100)
colour LookClipLost = (235, 160, 40)
colour LookSearching = (140, 145, 155)
colour LookDown = (215, 70, 60)
