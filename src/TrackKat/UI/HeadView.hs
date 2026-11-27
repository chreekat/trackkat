-- | What the head view, in the corner of trackkat's window, shows: the head
-- as games see it, the head as measured with the clip on it, or the
-- camera's whole picture. It is drawn by "TrackKat.Gui.HeadView".
module TrackKat.UI.HeadView (
    HeadScene (..),
    Sight (..),
    Spot (..),
    Area (..),
    Facing (..),
    headScene,
    onHead,
    unseenReflectors,
) where

import Data.List (intercalate)

import TrackKat.Config (HeadView (..))
import TrackKat.Linear (V3 (..))
import TrackKat.Pose (HeadPose (..), Reflector (..))
import TrackKat.Trackers (BlobRole (..))
import TrackKat.UI.Draw
import TrackKat.UI.Palette
import TrackKat.UI.Tray (ClipSight (..), Holding (..))

data HeadScene = HeadScene
    { sceneView :: HeadView
    , sceneSight :: Sight
    , sceneHead :: HeadPose
    , scenePose :: HeadPose
    -- ^ as games see it
    , sceneHolding :: Holding
    , sceneMissing :: [Reflector]
    -- ^ out of sight in the last frame
    , sceneSpots :: [Spot]
    -- ^ the last frame's blobs, heaviest first
    , sceneAreas :: [Area]
    , sceneClip :: Maybe (V3, V3, V3)
    -- ^ the left, right and top reflectors, in mm from the neck pivot along
    -- the head's axes, once there is a calibration to put them by
    }
    deriving (Eq, Show)

-- | Whether the camera is tracking, and then whether it sees the clip.
data Sight = NoTracking | Tracking ClipSight
    deriving (Eq, Show)

-- | A blob in the camera's image, and what it was taken for.
data Spot = Spot
    { spotX :: Double
    , spotY :: Double
    -- ^ in the camera's image, in pixels
    , spotPixels :: Double
    , spotRole :: BlobRole
    , spotVarX :: Double
    , spotVarY :: Double
    , spotCovXY :: Double
    -- ^ the spread of its lit pixels, in pixels squared
    }
    deriving (Eq, Show)

-- | The area masked around a still light: its centre and radius in the
-- camera's image, in pixels.
data Area = Area Double Double Double
    deriving (Eq, Show)

-- | Whether a point on the head is on the side of it that faces us.
data Facing = Front | Back
    deriving (Eq, Show)

-- | The head drawn: its centre with the head turned no way, and its
-- radius, in pixels.
data Head = Head Double Double Double

-- | The head view at a number of dots per inch, in a square box of a size
-- in pixels: the view shown, its name and what it shows top left, and a dot
-- for each view at the bottom, the one shown lit. The virtual and physical
-- views have what the camera sees in the top right corner, and a note at
-- the bottom when there is something to say.
headScene :: Int -> Int -> HeadScene -> [Shape]
headScene dpi size scene =
    Fill boxColour (0, 0, size, size)
        : Clipped (0, 0, size, size) shown
        : Text CapsFont nameColour (s 1) AlignLeft (box (s 8) (s 5) (size - s 66) (s 14)) name
        : Text SmallFont dim 0 AlignLeft (box (s 8) (s 19) (size - s 66) (s 14)) what
        : [ Disc c 1 c (size `div` 2 + (i - 1) * s 12, size - s 10) (s 3)
          | (i, v) <- zip [0 ..] [VirtualView, PhysicalView, CameraView]
          , let c = if v == sceneView scene then textColour else dotOff
          ]
        ++ note
  where
    s = scaledAt dpi
    tracking = case sceneSight scene of NoTracking -> False; Tracking _ -> True
    live = sceneSight scene == Tracking ClipInSight
    (name, what, nameColour) = case sceneView scene of
        VirtualView -> ("VIRTUAL", "what games see", RGB 185 220 255)
        PhysicalView -> ("PHYSICAL", "your head, as measured", RGB 255 210 122)
        CameraView -> ("CAMERA", "what the sensor sees", textColour)
    shown = case sceneView scene of
        CameraView -> cameraView dpi size scene
        view ->
            [ Line Dotted 1 gridColour [(size `div` 2, 0), (size `div` 2, size)]
            , Line Dotted 1 gridColour [(0, size `div` 2), (size, size `div` 2)]
            ]
                -- Under the head, which can pass over it.
                ++ (if tracking then smallCamera dpi (size - s 54) (s 6) (s 48) (sceneSpots scene) else [])
                ++ (if view == VirtualView then virtualHead dpi size live scene else physicalHead dpi size live scene)
    centred = Text BoldFont amber 0 AlignCentre (box 0 (size - s 40) size (s 20))
    note = case sceneView scene of
        CameraView -> []
        _
            | not live -> [centred (if tracking then "Clip out of sight" else "No tracking")]
        VirtualView
            | sceneHolding scene == ViewHeld -> [centred "PAUSED"]
        PhysicalView
            | Nothing <- sceneClip scene ->
                [Paragraph SmallFont dim (box (s 8) (size - s 48) (size - s 16) (s 32)) "The clip shows once trackkat has learned your neck."]
            | otherwise -> [Text SmallFont dim 0 AlignLeft (box (s 8) (size - s 32) (size - s 16) (s 14)) (unseenReflectors (sceneMissing scene))]
        _ -> []

-- | The camera view: the camera's whole image, as big as fits the box below
-- the view's name: each blob at its size and shape, the clip's reflectors
-- blue and joined as a triangle, others amber, still lights grey, with the
-- area masked around each dashed; and a legend.
cameraView :: Int -> Int -> HeadScene -> [Shape]
cameraView dpi size scene =
    legend 0 accent "clip"
        ++ legend 11 amber "stray"
        ++ legend 22 grey "ignored"
        ++ [ Fill frameColour (fx, fy, fx + fw, fy + fh)
           , Line Solid 1 (RGB 40 44 53) [(fx + fw `div` 2, fy), (fx + fw `div` 2, fy + fh)]
           , Line Solid 1 (RGB 40 44 53) [(fx, fy + fh `div` 2), (fx + fw, fy + fh `div` 2)]
           ]
        ++ [Clipped (fx, fy, fx + fw, fy + fh) picture | tracking]
        ++ [Frame frameEdge (fx, fy, fx + fw, fy + fh)]
        ++ [Text BoldFont amber 0 AlignCentre (box fx fy fw fh) "No camera" | not tracking]
  where
    s = scaledAt dpi
    tracking = sceneSight scene /= NoTracking
    legend dy c word =
        let right = size - s 6
            y = s 6 + s dy
         in [ Text TinyFont dim 0 AlignRight (box (right - s 70) (y - s 1) (s 60) (s 14)) word
            , Disc c 1 c (right - s 4, y + s 6) (s 3)
            ]
    fy = s 40
    room = size - s 18 - fy
    fw = let w = size - s 12 in if w * cameraH `div` cameraW > room then room * cameraW `div` cameraH else w
    fh = fw * cameraH `div` cameraW
    fx = (size - fw) `div` 2
    k = fromIntegral fw / fromIntegral cameraW
    at x y = (fx + lround (x * k), fy + lround (y * k))
    picture =
        [Ring Dashed 1 grey (at x y) (lround (r * k)) | Area x y r <- sceneAreas scene]
            ++ triangle
            -- Masked lights first, then strays, so the clip shows over them.
            ++ [ Polygon c (blobOutline dpi k (fx, fy) b)
               | (role, c) <- [(MaskedBlob, grey), (StrayBlob, amber), (ReflectorBlob, accent)]
               , b <- sceneSpots scene
               , spotRole b == role
               ]
    -- The clip's triangle, between its three heaviest blobs.
    corners = take 3 [at (spotX b) (spotY b) | b <- sceneSpots scene, spotRole b == ReflectorBlob]
    triangle = case corners of
        [a, b, c] -> [Line Solid 1 accentFaint [p, q] | (p, q) <- [(a, b), (b, c), (c, a)]]
        [a, b] -> [Line Solid 1 accentFaint [a, b]]
        _ -> []

-- | A blob at its size and shape, scaled by k, in the camera's image drawn
-- with its top left corner at a point: an ellipse as long and wide as its
-- lit pixels spread, as many pixels in area, at least a dot.
blobOutline :: Int -> Double -> Point -> Spot -> [Point]
blobOutline dpi k (left, top) b =
    [ ( lround (fromIntegral left + spotX b * k + u * cos angle - v * sin angle)
      , lround (fromIntegral top + spotY b * k + u * sin angle + v * cos angle)
      )
    | i <- [0 .. 23 :: Int]
    , let t = 2 * pi * fromIntegral i / 24
          u = along * cos t
          v = across * sin t
    ]
  where
    -- The spread in single precision, as cbits/gui.c works it out.
    vx = realToFrac (spotVarX b) :: Float
    vy = realToFrac (spotVarY b) :: Float
    cv = realToFrac (spotCovXY b) :: Float
    mid = realToFrac ((vx + vy) / 2) :: Double
    half = sqrt (realToFrac ((vx - vy) * (vx - vy) / 4 + cv * cv)) :: Double
    major = mid + half
    minor = max (1 / 12) (mid - half)
    ratio = if major > minor then sqrt (major / minor) else 1
    angle = 0.5 * atan2 (realToFrac (2 * cv)) (realToFrac (vx - vy)) :: Double
    least = fromIntegral (scaledAt dpi 1) + 0.5
    along = max least (sqrt (spotPixels b / pi * ratio) * k)
    across = max least (sqrt (spotPixels b / pi / ratio) * k)

-- | What the camera sees, small, with its top left corner at left, top: each
-- blob a dot at its place, sized by its area but half as big again to be
-- seen, coloured by what it was taken for.
smallCamera :: Int -> Int -> Int -> Int -> [Spot] -> [Shape]
smallCamera dpi left top width spots =
    Fill frameColour (left, top, left + width, top + height)
        : Frame frameEdge (left, top, left + width, top + height)
        -- Masked lights first, so the clip shows over them.
        : [ Disc c 1 c (left + lround (spotX b * k), top + lround (spotY b * k)) r
          | masked <- [True, False]
          , b <- spots
          , (spotRole b == MaskedBlob) == masked
          , let c = case spotRole b of ReflectorBlob -> accent; StrayBlob -> amber; MaskedBlob -> grey
                r = max (scaledAt dpi 1) (lround (1.5 * sqrt (spotPixels b / pi) * k))
          ]
  where
    height = width * cameraH `div` cameraW
    k = fromIntegral width / fromIntegral cameraW

-- | The virtual view: the head as games see it, a face that turns, tilts and
-- moves with the view, a line out of the nose showing where it looks.
virtualHead :: Int -> Int -> Bool -> HeadScene -> [Shape]
virtualHead dpi size live scene =
    concatMap layer [Back, Front] ++ eyes ++ nose ++ mouth
  where
    v = scenePose scene
    h@(Head _ _ r) = headAt v size
    skin = if live then RGB 78 88 104 else RGB 58 62 70
    line = if live then RGB 205 212 224 else dim
    feature = if live then RGB 240 244 250 else dim
    w = scaledAt dpi 2
    at = project h v
    (centre, _) = at (0, 0, 0)
    -- What is behind the head is drawn before it, what faces us after.
    layer side =
        [Disc skin w line centre (lround r) | side == Front]
            ++ [Disc skin w line p (lround (r * 0.2)) | ear <- [-1, 1], let (p, f) = at (ear * 0.98, 0, 0), f == side]
            ++ [Line Solid w (if live then accent else dim) [p, fst (at (0, -0.05, 2.2))] | let (p, f) = at (0, -0.05, 1), f == side]
    eyes = [Disc feature 1 feature p (lround (r * 0.09)) | eye <- [-1, 1], (p, Front) <- [at (eye * 0.36, 0.22, 0.9)]]
    nose = [Line Solid w feature [p, fst (at (0, -0.12, 1.28))] | (p, Front) <- [at (0, -0.05, 1)]]
    mouth =
        let points =
                [ at (x, y, sqrt (1 - x * x - y * y))
                | i <- [0 .. 8 :: Int]
                , let x = -0.32 + 0.64 * fromIntegral i / 8
                      y = -0.48 + 0.08 * sin (fromIntegral i * pi / 8) * (-1)
                ]
         in [Line Solid w feature [p, q] | ((p, Front), (q, Front)) <- zip points (drop 1 points)]

-- | The physical view: the head as the camera measures it, dotted yellow,
-- turning about its neck pivot, a grey dot with the neck below it; and once
-- the pivot is learnt, the clip where it sits on it, all in the box.
physicalHead :: Int -> Int -> Bool -> HeadScene -> [Shape]
physicalHead dpi size live scene =
    clipSide Back
        ++ [ Line Solid (s 2) dotOff [pivot, neck]
           , Disc face 1 face centre (lround r)
           , -- Dotted, two pixels wide.
             Ring Dotted 1 outline centre (lround r)
           , Ring Dotted 1 outline centre (lround r - 1)
           , Disc dim 1 dim pivot (s 2)
           ]
        ++ [Disc eye 1 eye p (lround (r * 0.1)) | side <- [-1, 1], (p, Front) <- [at (side * 0.36, 0.22, 0.9)]]
        ++ [Line Solid (s 2) outline [p, fst (at (0, -0.05, 2.2))] | (p, Front) <- [at (0, -0.05, 1)]]
        ++ clipSide Front
  where
    s = scaledAt dpi
    v = sceneHead scene
    clip = if live then sceneClip scene else Nothing
    reflectors = maybe [] (\(l, rt, t) -> map clipPoint [l, rt, t]) clip
    h@(Head cx cy r) = fitted dpi size reflectors (headAt v size)
    at = project h v
    outline = if live then headTick else dim
    face = if live then RGB 52 58 72 else RGB 46 50 58
    eye = RGB 36 40 48
    (centre, _) = at (0, 0, 0)
    pivot = (lround cx, lround (cy + neckDrop * r))
    neck = (fst pivot, lround (cy + 1.35 * r))
    -- The clip's reflectors on the far side of the head, drawn before it,
    -- or the near side: its triangle faint, and each reflector a dot, blue
    -- if seen in the last frame and hollow if not.
    clipSide side =
        let placed = [(at p, which) | (p, which) <- zip reflectors [LeftReflector, RightReflector, TopReflector]]
         in [ Line Solid 1 accentFaint [p, q]
            | (((p, f), _), ((q, g), _)) <- zip placed (drop 1 placed ++ take 1 placed)
            , (if f == Front && g == Front then Front else Back) == side
            ]
                ++ [ if which `elem` sceneMissing scene then Ring Solid 1 accent p (s 3) else Disc accent 1 backColour p (s 3)
                   | ((p, f), which) <- placed
                   , f == side
                   ]

-- | The head at a pose, in a box of a size: 300 mm from the centre to the
-- edge; nearer is bigger. It is a ball 'headMM' in radius.
headAt :: HeadPose -> Int -> Head
headAt pose size =
    Head (half - tx pose * k) (half + ty pose * k) (fromIntegral size * 0.2 * 600 / (600 + max (-300) (tz pose)))
  where
    half = fromIntegral size / 2
    k = fromIntegral size / 2 / 300

-- | The head made smaller and moved if need be so that, turned no way, it,
-- its neck and the clip's reflectors on it are inside the box, a little in
-- from its edges. A pivot learnt far from the clip puts the clip well above
-- the head.
fitted :: Int -> Int -> [(Double, Double, Double)] -> Head -> Head
fitted dpi size reflectors (Head cx cy r) = Head cx' cy' r'
  where
    still = HeadPose 0 0 0 0 0 0
    placed = [(x, y) | p <- reflectors, let (x, y, _) = onHead still p]
    -- The head and its neck, and the clip.
    loX = minimum (-1 : map fst placed)
    hiX = maximum (1 : map fst placed)
    loY = minimum (-1 : map snd placed)
    hiY = maximum (1.35 : map snd placed)
    margin = fromIntegral (scaledAt dpi 5)
    room = fromIntegral size - 2 * margin
    r' = minimum [r, room / (hiX - loX), room / (hiY - loY)]
    cx' = min (fromIntegral size - margin - hiX * r') (max cx (margin - loX * r'))
    cy' = min (fromIntegral size - margin - hiY * r') (max cy (margin - loY * r'))

-- | Where a point on the head, in radii from its centre (x to the user's
-- right, y up, z out of the face), lands with the head turned about its
-- neck by the pose, as seen in a mirror: across and down from the centre of
-- the head turned no way, in radii; and whether it faces us.
onHead :: HeadPose -> (Double, Double, Double) -> (Double, Double, Facing)
onHead pose p = (x - nx, -(y - ny - neckDrop), if z > 0 then Front else Back)
  where
    (x, y, z) = turn pose p
    (nx, ny, _) = turn pose (0, -neckDrop, 0)

-- | Where a point on the head lands on screen, and whether it faces us.
project :: Head -> HeadPose -> (Double, Double, Double) -> (Point, Facing)
project (Head cx cy r) pose p = ((lround (cx + across * r), lround (cy + down * r)), facing)
  where
    (across, down, facing) = onHead pose p

-- | A point on the head turned by the pose's roll, then pitch, then yaw:
-- roll left, pitch down and yaw right are positive.
turn :: HeadPose -> (Double, Double, Double) -> (Double, Double, Double)
turn pose (x0, y0, z0) = (x3, y2, z3)
  where
    a = yaw pose * pi / 180
    b = pitch pose * pi / 180
    c = roll pose * pi / 180
    x1 = x0 * cos c - y0 * sin c
    y1 = x0 * sin c + y0 * cos c
    y2 = y1 * cos b - z0 * sin b
    z2 = y1 * sin b + z0 * cos b
    x3 = x1 * cos a + z2 * sin a
    z3 = -x1 * sin a + z2 * cos a

-- | A reflector on the head, in radii from its centre, from where it is in
-- mm from the neck pivot.
clipPoint :: V3 -> (Double, Double, Double)
clipPoint (V3 x y z) = (x / headMM, y / headMM - neckDrop, z / headMM)

-- | The head drawn is a ball this many mm in radius, about 180 mm across.
headMM :: Double
headMM = 90

-- | How far below the head's centre its neck pivot is, in radii: on the
-- midline and level with the ear lobes, about where the top of the neck is.
neckDrop :: Double
neckDrop = 0.35

-- | Which reflectors the last frame missed, as "top reflector not seen",
-- by the wearer's sides: the left reflector is the one on the left of the
-- camera's image, so on the wearer's right. Nothing for none or all.
unseenReflectors :: [Reflector] -> String
unseenReflectors missing = case [side | (which, side) <- order, which `elem` missing] of
    [one] -> one ++ " reflector not seen"
    two@[_, _] -> intercalate " and " two ++ " reflectors not seen"
    _ -> ""
  where
    order = [(TopReflector, "top"), (RightReflector, "left"), (LeftReflector, "right")]

-- | x, y, width and height as a rectangle.
box :: Int -> Int -> Int -> Int -> Rect
box x y w h = (x, y, x + w, y + h)

-- | The camera's image, in pixels, as frameWidth and frameHeight in
-- "TrackKat.Frame".
cameraW, cameraH :: Int
cameraW = 640
cameraH = 480

-- As cbits/gui.c's C_ colours.
frameColour, frameEdge, dotOff :: Colour
frameColour = RGB 24 27 33
frameEdge = RGB 58 64 76
dotOff = RGB 74 81 96
