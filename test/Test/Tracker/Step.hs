-- | The stages put together: what a frame comes to (see 'step').
module Test.Tracker.Step (tests) where

import Data.Maybe (isJust, isNothing)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Test.Gen
import Test.Simulation (headset)
import TrackKat.Calibration (defaultCalibration)
import TrackKat.Frame (frameHeight, frameWidth)
import TrackKat.Pose
import TrackKat.Synth (Disc, allLit, pointDiscs, renderDiscs)
import TrackKat.Tracker

-- | Whatever a frame shows, every blob of it is accounted for, and what
-- was asked for is done on the first frame tracked and not before.
tests :: TestTree
tests =
    testGroup
        "Step"
        [ testProperty "every blob of a frame is told with what it was taken for, and counted, the clip made out or not" $
            \scene ->
                let stripes = renderDiscs (discs scene)
                    blobs = findBlobs stripes
                    (after, outcome, _) = step 0 stripes fresh
                    taken = trackerBlobs after
                    reflectors = length [() | (_, ReflectorBlob) <- taken]
                 in map fst taken === blobs .&&. case outcome of
                        Lost shown masked _ _ -> shown + masked === length blobs .&&. reflectors === 0
                        Tracked{outSeen = Seen missing _ strays masked} ->
                            conjoin
                                [ counterexample "reflectors" (reflectors >= 3 - length missing)
                                , counterexample "strays" (strays + masked <= length blobs - 2)
                                , length [() | (_, StrayBlob) <- taken] + length [() | (_, MaskedBlob) <- taken] + reflectors === length blobs
                                ]
        , testProperty "the first frame tracked recenters on its own pose, and a frame not tracked does nothing asked for" $
            \scene ->
                case step 0 (renderDiscs (discs scene)) fresh of
                    (after, Tracked{outPose = HeadPose y p r x up z}, events) ->
                        conjoin
                            [ counterexample (show events) (case events of [Recentered] -> True; _ -> False)
                            , counterexample (show [y, p, r, x, up, z]) (all ((< 1e-6) . abs) [y, p, r, x, up, z])
                            , property (isJust (trackerCentre after) && isJust (trackerPose after))
                            ]
                    (after, Lost{}, events) ->
                        counterexample (show events) (null events) .&&. property (isNothing (trackerCentre after) && isNothing (trackerPose after))
        , testProperty "the clip alone in view, facing the camera within 20 degrees, is tracked from the first frame" $
            \(Apart20 placing) ->
                case step 0 (renderDiscs (discs (Scene (Just placing) []))) fresh of
                    (_, Tracked{outSeen = seen, outChosen = chosen}, _) -> seen === Seen [] 0 0 0 .&&. chosen === Chosen FoundAfresh Nothing
                    (_, Lost{outWhy = why}, _) -> counterexample (explainWhy why) False
        ]
  where
    fresh = newTracker defaultIntrinsics headset MaskLights defaultCalibration

-- | What the camera sees: the clip, if it is there, and other lights.
data Scene = Scene (Maybe Placing) [Disc]
    deriving (Show)

discs :: Scene -> [Disc]
discs (Scene clip others) = maybe [] (pointDiscs defaultIntrinsics headset allLit . placed) clip ++ others

-- | The clip seen whole, or not there, and a few other lights of up to a
-- reflector's size or so. Simplest nothing at all; simpler with fewer
-- lights, without the clip, and with it turned less.
instance Arbitrary Scene where
    arbitrary = Scene <$> oneof [pure Nothing, Just <$> genClipApart 30 (400, 1500) headset] <*> others
      where
        others = do
            n <- chooseInt (0, 4)
            vectorOf n ((,) <$> ((,) <$> choose (20, fromIntegral frameWidth - 20) <*> choose (20, fromIntegral frameHeight - 20)) <*> choose (2, 14))
    shrink =
        shrinkParts
            [ \(Scene clip others) -> [Scene clip fewer | fewer <- shrinkList (const []) others]
            , \(Scene clip others) -> [Scene Nothing others | isJust clip] ++ [Scene (Just simpler) others | Just placing <- [clip], simpler <- shrinkClipApart (400, 1500) headset placing]
            ]
