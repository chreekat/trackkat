-- | The pose's axes and their response curves, as trackkat's window shows
-- and edits them.
module TrackKat.Window.Curves (
    PoseAxis (..),
    axisName,
    axisSetting,
    axisRange,
    axisSides,
    axisValue,
    formatAxis,
    CurveForm (..),
    readCurveForm,
    formFactor,
    multiplierText,
    stepMultiplier,
    describeReach,
    formatMultiplier,
) where

import Text.Printf (printf)

import TrackKat.Config (parseCurve)
import TrackKat.Number (renderNumber)
import TrackKat.Pose (HeadPose (..))
import TrackKat.Response

data PoseAxis = Yaw | Pitch | Roll | X | Y | Z
    deriving (Eq, Show, Enum, Bounded)

axisName :: PoseAxis -> String
axisName = show

-- | The axis's curve's name in the settings file.
axisSetting :: PoseAxis -> String
axisSetting Yaw = "yaw"
axisSetting Pitch = "pitch"
axisSetting Roll = "roll"
axisSetting X = "x"
axisSetting Y = "y"
axisSetting Z = "z"

-- | The end of the view's range, in degrees or mm.
axisRange :: PoseAxis -> Double
axisRange Yaw = yawRange
axisRange Pitch = pitchRange
axisRange Roll = rollRange
axisRange _ = leanRange

-- | Which way the head moves for values below and above zero, as trackkat
-- measures them.
axisSides :: PoseAxis -> (String, String)
axisSides Yaw = ("left", "right")
axisSides Pitch = ("up", "down")
axisSides Roll = ("right", "left")
axisSides X = ("right", "left")
axisSides Y = ("up", "down")
axisSides Z = ("fwd", "back")

axisValue :: PoseAxis -> HeadPose -> Double
axisValue Yaw = yaw
axisValue Pitch = pitch
axisValue Roll = roll
axisValue X = tx
axisValue Y = ty
axisValue Z = tz

-- | A value on an axis with its unit: degrees to a tenth, mm whole.
formatAxis :: PoseAxis -> Double -> String
formatAxis a v
    | a `elem` [Yaw, Pitch, Roll] = printf "%+.1f°" v
    | otherwise = printf "%+.0f mm" v

-- | A curve as the window edits it.
data CurveForm
    = -- | a multiplier, above 0, and a centre precision, 0 to 100
      MultiplierForm Double Double
    | -- | points of any other shape
      PointsForm Curve
    | -- | why the text isn't a curve
      Unreadable String
    deriving (Eq, Show)

-- | Read an axis's curve as the settings file writes it.
readCurveForm :: PoseAxis -> String -> CurveForm
readCurveForm a text = case parseCurve (axisRange a) text of
    Left err -> Unreadable err
    Right (Points [(i, o)]) | o > 0 -> MultiplierForm (o / i) 0
    Right (Expo m) -> MultiplierForm (factor m) (precision m)
    Right c -> PointsForm c

-- | A curve's multiplier, or for points the last one's output over its
-- input, if above 0.
formFactor :: CurveForm -> Maybe Double
formFactor (MultiplierForm f _) = Just f
formFactor (PointsForm (Points ps))
    | (i, o) : _ <- reverse ps, i > 0, o > 0 = Just (o / i)
formFactor _ = Nothing

-- | A multiplier and precision as the settings file writes them, leaving
-- out a precision of 0.
multiplierText :: Double -> Double -> String
multiplierText f p
    | p == 0 = "1:" ++ formatMultiplier f
    | otherwise = "1:" ++ formatMultiplier f ++ ", precision " ++ renderNumber p

-- | A multiplier with no more decimals than it needs, up to two.
formatMultiplier :: Double -> String
formatMultiplier f = renderNumber (fromIntegral (round (f * 100) :: Integer) / 100)

-- | Step a multiplier up or down to the next half, no lower than a half.
stepMultiplier :: Ordering -> Double -> Double
stepMultiplier direction f = max 1 halves / 2
  where
    halves :: Double
    halves = case direction of
        GT -> fromIntegral (floor (f * 2 + 1e-6) :: Integer) + 1
        _ -> fromIntegral (ceiling (f * 2 - 1e-6) :: Integer) - 1

-- | What a multiplier means: how far the head moves for the view to reach
-- the end of its range, like "30° turn looks behind".
describeReach :: PoseAxis -> Double -> String
describeReach a f = case a of
    Yaw -> printf "%s° turn looks behind" n
    Pitch -> printf "%s° nod looks straight up" n
    Roll -> printf "%s° tilt rolls 90°" n
    X -> printf "%s mm lean moves 300 mm" n
    Y -> printf "%s mm rise moves 300 mm" n
    Z -> printf "%s mm lean moves 300 mm" n
  where
    r = axisRange a / f
    n = renderNumber (if r < 10 then fromIntegral (round (r * 10) :: Integer) / 10 else fromIntegral (round r :: Integer))
