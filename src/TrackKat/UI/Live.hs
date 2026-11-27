{-# LANGUAGE LambdaCase #-}

-- | What the live view across trackkat's window shows beside the head view
-- ("TrackKat.UI.HeadView"): each axis, the lights, the figures, and the
-- four switches. It is drawn by "TrackKat.Gui.Live".
module TrackKat.UI.Live (
    TrimMoves (..),
    trimMoves,
    livePane,
    Switch (..),
    Press (..),
    switchFace,
    switchWords,
    axisFigure,
) where

import TrackKat.Display (CameraState (..), Output (..), Shown (..), Sighting (..), Tracked (..), shownHead, tracked)
import TrackKat.Pose (HeadPose (..))
import TrackKat.Response (Aiming (..), Trim (..), leanRange, pitchRange, rollRange, yawRange)
import TrackKat.UI.Draw
import TrackKat.UI.Palette

-- | Whether a trim moves or tilts the view, besides turning it.
data TrimMoves = TurnsOnly | MovesToo
    deriving (Eq, Show)

-- | Whether a trim, as where forward is, does more than turn the view:
-- enough more to notice.
trimMoves :: HeadPose -> TrimMoves
trimMoves t
    | abs (roll t) >= 1 || any ((>= 5) . abs) [tx t, ty t, tz t] = MovesToo
    | otherwise = TurnsOnly

-- | The live view at a number of dots per inch, in a width and height of
-- pixels, behind its head view and switches: each axis, your head, what
-- games see, and a bar of the view with your head as a tick; the camera,
-- clip and games lights; and the figures.
livePane :: Int -> (Int, Int) -> Shown -> [Shape]
livePane dpi (width, height) st =
    Fill backColour (0, 0, width, height)
        : Text SmallFont dim 0 AlignRight (box (x0 + s 50) (s 6) (s 80) (s 18)) "head"
        : Text SmallFont dim 0 AlignRight (box (x0 + s 130) (s 6) (s 80) (s 18)) "games see"
        : Text SmallFont dim 0 AlignRight (box barL (s 6) (barR - barL) (s 18)) "Rest the mouse on anything for help"
        : concatMap row (zip [0 ..] axes)
        ++ light 0 cameraDot "Camera" cameraWords
        ++ light 1 clipDot "Clip" clipWords
        ++ light 2 (if shownGames st > 0 then accent else grey) "Games" games
        ++ [Text SmallFont dim 0 AlignLeft (box (s 8) (s statsY) (width - s 18) (s 18)) (shownStats st) | cameraOn, not (null (shownStats st))]
  where
    s = scaledAt dpi
    x0 = s (8 + headSize + 16)
    barL = x0 + s 226
    barR = width - s 10
    cameraOn = shownCamera st == Tracking
    tracking = tracked st == TrackedNow
    row (i, (name, part, range, minus, plus)) =
        let y = s 26 + i * s 24
            top = y + s 5
            bottom = y + s 18
            mid = (barL + barR) `div` 2
            half = (barR - barL) `div` 2
            -- The fraction of the range in single precision, as cbits/gui.c
            -- works it out.
            fraction v = max (-1) (min 1 (realToFrac (realToFrac (part v) / realToFrac range :: Float))) :: Double
            end = mid + lround (fraction (shownView st) * fromIntegral half)
            tick = mid + lround (fraction (shownHead st) * fromIntegral half)
         in [ Text UiFont textColour 0 AlignLeft (box x0 y (s 50) (s 24)) name
            , Text MonoFont (if tracking then textColour else dim) 0 AlignRight (box (x0 + s 50) y (s 80) (s 24)) (axisFigure i (part (shownHead st)))
            , Text MonoFont viewColour 0 AlignRight (box (x0 + s 130) y (s 80) (s 24)) (axisFigure i (part (shownView st)))
            , Fill boxColour (barL, top, barR, bottom)
            , Fill accent (min end mid, top, if end < mid then mid else end + 1, bottom)
            , Fill headTick (tick - s 1, top - s 2, tick + s 1 + 1, bottom + s 2)
            , Fill gridColour (mid, top - s 2, mid + 1, bottom + s 2)
            , Text SmallFont dim 0 AlignLeft (box (barL + s 4) top (s 50) (bottom - top)) minus
            , Text SmallFont dim 0 AlignRight (box (barR - s 54) top (s 50) (bottom - top)) plus
            ]
    light i dot label value =
        let x = s 8 + i * s lightW
            y = s lightsY
         in [ Disc dot 1 dot (x + s 5, y + s 9) (s 4)
            , Text SmallFont dim 0 AlignLeft (box (x + s 16) y (s 48) (s 18)) label
            , Text UiFont textColour 0 AlignLeft (box (x + s 64) y (s lightW - s 64) (s 18)) value
            ]
    -- The view held while paused is what games see, tracked or not.
    viewColour = case shownOutput st of
        Paused _ -> accent
        Live -> if tracking then accent else dim
    (cameraDot, cameraWords) = case shownCamera st of
        Tracking -> (green, "Connected")
        Searching -> (grey, "Looking for it\8230")
        CameraDown why -> (red, if null why then "Not found" else why)
    (clipDot, clipWords)
        | not cameraOn = (grey, "\8212")
        | OutOfSight n <- shownSighting st = (amber, "Out of sight (" ++ show n ++ " of 3)")
        | otherwise = (green, "In sight")
    -- A game by its name when it is known, and how many more.
    games = case (shownGames st, shownGameName st) of
        (0, _) -> "None"
        (1, Just name) -> name
        (n, Just name) -> name ++ " and " ++ show (n - 1) ++ " more"
        (1, Nothing) -> "1 connected"
        (n, Nothing) -> show n ++ " connected"
    axes =
        [ ("Yaw", yaw, yawRange, "left", "right")
        , ("Pitch", pitch, pitchRange, "up", "down")
        , ("Roll", roll, rollRange, "right", "left")
        , ("X", tx, leanRange, "right", "left")
        , ("Y", ty, leanRange, "up", "down")
        , ("Z", tz, leanRange, "fwd", "back")
        ]
    -- The live view's layout, in pixels at 96 dots per inch, as
    -- cbits/gui.c's build_window lays out the controls in it.
    headSize = 176
    lightsY = 192
    lightW = (700 - 16) `div` 3
    statsY = 274

-- | An axis's figure, numbered from yaw to z: degrees to a tenth for the
-- turns, whole mm for the moves.
axisFigure :: Int -> Double -> String
axisFigure i v
    | i < 3 = signedFixed 1 v ++ "\176"
    | otherwise = signedFixed 0 v ++ " mm"

data Switch = RecenterSwitch | PauseSwitch | TrimSwitch | AimSwitch
    deriving (Eq, Show, Enum, Bounded)

data Press = Pressed | Released
    deriving (Eq, Show)

-- | A switch's colours: unlit, or lit amber or blue like an annunciator.
data Lamp = Lamp {lampBack, lampEdge, lampName, lampLine :: Colour}
    deriving (Eq)

lampOff, lampAmber, lampBlue :: Lamp
lampOff = Lamp (RGB 42 47 57) (RGB 69 76 89) (RGB 230 233 239) (RGB 138 146 161)
lampAmber = Lamp (RGB 74 54 18) (RGB 240 180 69) (RGB 255 210 122) (RGB 231 194 120)
lampBlue = Lamp (RGB 20 42 69) (RGB 90 169 255) (RGB 185 220 255) (RGB 156 200 245)

-- | What a switch says: its name, bold, and while it is lit a line below
-- of its state.
switchWords :: Shown -> Switch -> (String, String)
switchWords st which = let (name, line, _) = switchLit st which in (name, line)

-- | What a switch says, and how it is lit.
switchLit :: Shown -> Switch -> (String, String, Lamp)
switchLit st = \case
    RecenterSwitch -> ("RECENTER", "", lampOff)
    PauseSwitch -> case shownOutput st of
        Paused p -> ("PAUSED", yawAndPitch p "", lampAmber)
        Live -> ("PAUSE", "", lampOff)
    TrimSwitch -> case shownTrim st of
        Trim t -> ("TRIM", yawAndPitch t (case trimMoves t of MovesToo -> " \183 moved"; TurnsOnly -> ""), lampBlue)
        NoTrim -> ("TRIM", "", lampOff)
    AimSwitch -> case shownAiming st of
        Aiming -> ("AIMING", "Sensitivity reduced", lampBlue)
        NotAiming -> ("AIM", "", lampOff)
  where
    yawAndPitch v after = "yaw " ++ signedDegrees (yaw v) ++ " pitch " ++ signedDegrees (pitch v) ++ after
    -- A whole number of degrees with its sign, minus as a true minus.
    signedDegrees x = let n = lround x in (if n < 0 then "\8722" else "+") ++ show (abs n)

-- | A switch's face at a number of dots per inch, in a rectangle: rounded,
-- and when lit a glow inside its edge; its name, and a line below it while
-- lit. A press darkens it and moves its words down and right.
switchFace :: Int -> Rect -> Press -> Shown -> Switch -> [Shape]
switchFace dpi r@(l, t, rt, b) press st which =
    Fill backColour r
        : Rounded (Just back) (lampEdge lamp) (s 8) r
        : [ Rounded Nothing (mix back (lampEdge lamp) (50 - 45 * i `div` (s 5 + 1))) (s 8) (l + i, t + i, rt - i, b - i)
          | lamp /= lampOff
          , i <- [1 .. s 5]
          ]
        ++ [ -- The name alone sits in the middle; with a line below, above it.
             Text BoldFont (lampName lamp) (s 2) AlignCentre (box (l + shift) (t + h `div` 2 - s (if null line then 10 else 19) + shift) w (s 20)) name
           , Text SmallFont (lampLine lamp) 0 AlignCentre (box (l + s 4 + shift) (t + h `div` 2 + shift) (w - s 8) (s 18)) line
           ]
  where
    s = scaledAt dpi
    (name, line, lamp) = switchLit st which
    back = case press of Pressed -> mix (lampBack lamp) (RGB 0 0 0) 25; Released -> lampBack lamp
    shift = case press of Pressed -> s 1; Released -> 0
    w = rt - l
    h = b - t

-- | A colour a percentage of the way to another.
mix :: Colour -> Colour -> Int -> Colour
mix (RGB r g b) (RGB r' g' b') p = RGB (part r r') (part g g') (part b b')
  where
    part x y = (x * (100 - p) + y * p) `div` 100

-- | A number with its sign and some decimals, as C's printf writes it with
-- %+.Nf: the exact value rounded half to even, and a minus for any value
-- below zero, however small.
signedFixed :: Int -> Double -> String
signedFixed d x = (if x < 0 || isNegativeZero x then '-' else '+') : show whole ++ fraction
  where
    n = round (abs (toRational x) * 10 ^ d) :: Integer
    (whole, rest) = n `divMod` (10 ^ d)
    fraction = if d > 0 then '.' : replicate (d - length (show rest)) '0' ++ show rest else ""

-- | x, y, width and height as a rectangle.
box :: Int -> Int -> Int -> Int -> Rect
box x y w h = (x, y, x + w, y + h)
