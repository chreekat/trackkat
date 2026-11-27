{-# LANGUAGE LambdaCase #-}

-- | What the head view shows.
module Test.UI.HeadView (tests) where

import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (Gen, choose, forAll, testProperty)

import Test.Check (check, group, tree)
import TrackKat.Config (HeadView (..))
import TrackKat.Display (CameraState (..), Output (..), Shown (..), Sighting (..), nothingShown)
import TrackKat.Linear (V3 (..))
import TrackKat.Pose (HeadPose (..), Reflector (..))
import TrackKat.UI.Draw
import TrackKat.UI.HeadView

tests :: TestTree
tests = group "Head view" $ do
    let still = HeadPose 0 0 0 0 0 0
        seen missing = InSight still missing
        scene =
            nothingShown
                { shownCamera = Tracking
                , shownSighting = seen []
                , shownClip = Just (V3 38 92 71, V3 (-32) 92 71, V3 3 148.2 (-16.9))
                }
        shapesIn view = flatten . headScene 96 176 view
        shapes = shapesIn PhysicalView
        saysIn view sc = [t | Text _ _ _ _ _ t <- shapesIn view sc] ++ [t | Paragraph _ _ _ t <- shapesIn view sc]
        says = saysIn PhysicalView
        hollow sc = length [() | Ring Solid _ _ _ _ <- shapes sc]
        (across, _, _) = onHead still{yaw = 30} (0, 0, 1)
    check "a head turned no way faces us, its nose at the centre" $
        onHead still (0, 0, 1) == (0, 0, Front) && thd (onHead still (0, 0, -1)) == Back
    check "turning right turns the face to the right, as in a mirror" $
        across > 0
    check "reflectors out of sight are named by the wearer's sides, top first" $
        map unseenReflectors [[], [TopReflector], [LeftReflector, TopReflector], [LeftReflector, RightReflector, TopReflector]]
            == ["", "top reflector not seen", "top and right reflectors not seen", ""]
    check "without tracking, or the clip, the head views say which" $
        map (\v -> filter (`elem` ["No tracking", "Clip out of sight"]) (saysIn v scene{shownCamera = Searching})) [VirtualView, PhysicalView]
            == [["No tracking"], ["No tracking"]]
            && "Clip out of sight" `elem` says scene{shownSighting = OutOfSight 2}
    check "the camera view says when there is no camera, and nothing of the clip" $
        "No camera" `elem` saysIn CameraView scene{shownCamera = Searching}
            && "Clip out of sight" `notElem` saysIn CameraView scene{shownSighting = OutOfSight 2}
    check "the virtual view says when the view is held" $
        "PAUSED" `elem` saysIn VirtualView scene{shownOutput = Paused still}
            && "PAUSED" `notElem` says scene{shownOutput = Paused still}
    check "the physical view says the clip waits for a calibration, then which reflectors are out of sight" $
        "The clip shows once trackkat has learned your neck." `elem` says scene{shownClip = Nothing}
            && "top reflector not seen" `elem` says scene{shownSighting = seen [TopReflector]}
    check "a reflector out of sight is drawn hollow" $
        map (\m -> hollow scene{shownSighting = seen m}) [[], [TopReflector], [LeftReflector, TopReflector]] == [0, 1, 2]
    tree . testProperty "the clip on a head turned no way is in the box, however far it is from the neck" $
        forAll ((,,) <$> reflector <*> reflector <*> reflector) $ \(l, r, t) ->
            forAll ((\x y z -> still{tx = x, ty = y, tz = z}) <$> mm <*> mm <*> mm) $ \at ->
                let dots = [p | Disc c _ _ p _ <- shapes scene{shownSighting = InSight at [], shownClip = Just (l, r, t)}, c == RGB 90 170 255]
                 in length dots == 3 && all (\(x, y) -> x >= 0 && x < 176 && y >= 0 && y < 176) dots
  where
    thd (_, _, c) = c
    flatten = concatMap (\case Clipped _ ss -> flatten ss; sh -> [sh])
    mm = choose (-300, 300) :: Gen Double
    reflector = V3 <$> choose (-400, 400) <*> choose (-400, 400) <*> choose (-400, 400)
