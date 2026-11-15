-- | What is remembered of the reflectors' sizes, and what it says of how
-- they show (see "TrackKat.Tracker.Sizes").
module Test.Tracker.Sizes (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Test.Gen
import TrackKat.Linear (V3 (..), identity)
import TrackKat.Match (Kind (..), Sight (..), Size (..), Three (..), noSize)
import TrackKat.Pose (Pose (..))
import TrackKat.Tracker.Sizes

-- | A size is in pixels at 1 m, so it says the same of a reflector
-- wherever it was seen. Seen as a blob of its own, a reflector's latest
-- size is what the blob shows, and its usual size no less; unseen, or
-- sharing a blob, its sizes only fade.
tests :: TestTree
tests =
    testGroup
        "Sizes"
        [ testProperty "a reflector seen as a blob of its own is the size the blob shows, and usually no smaller" $
            forAllShrink genSize shrinkSize $ \size -> forAllShrink genOwn shrinkSight $ \sight -> forAll genSince $ \since -> forAll genMetres $ \z ->
                let after = resized since z size (Just sight)
                 in sizeLatest after === sightPixels sight * z * z .&&. property (sizeUsual after >= sizeLatest after)
        , testProperty "a blob says the same of its reflector's size at any distance, for the pixels it would light at 1 m" $
            forAllShrink genSize shrinkSize $ \size -> forAll (choose (6, 5000)) $ \atOneMetre -> forAll genSince $ \since -> forAll genMetres $ \z ->
                let seenAt d = resized since d size (Just (Sight (0, 0) (atOneMetre / (d * d)) Whole))
                 in close (1e-9 * atOneMetre) (sizeLatest (seenAt z)) (sizeLatest (seenAt 1))
                        .&&. close (1e-9 * (atOneMetre + sizeUsual size)) (sizeUsual (seenAt z)) (sizeUsual (seenAt 1))
        , testProperty "a reflector unseen, or sharing a blob, keeps its latest size, and its usual size only fades" $
            forAllShrink genSize shrinkSize $ \size -> forAllShrink (genSight 5000) shrinkSight $ \sight -> forAll genSince $ \since -> forAll genMetres $ \z ->
                conjoin
                    [ sizeLatest after === sizeLatest size .&&. property (sizeUsual after <= sizeUsual size && sizeUsual after >= 0)
                    | after <- [resized since z size Nothing, resized since z size (Just sight{sightKind = Merged})]
                    ]
        , testProperty "no time passing, a reflector unseen is the size it was" $
            forAllShrink genSize shrinkSize $ \size -> forAll genMetres $ \z ->
                let after = resized 0 z size Nothing
                 in sizeLatest after === sizeLatest size .&&. sizeUsual after === sizeUsual size
        , testProperty "a usual size fades as much over two times as over their sum, by e in the seconds sizes are remembered" $
            forAllShrink genSize shrinkSize $ \size -> forAll genSince $ \first -> forAll genSince $ \second ->
                let unseen since from = resized since 1 from Nothing
                 in close (1e-9 * sizeUsual size) (sizeUsual (unseen second (unseen first size))) (sizeUsual (unseen (first + second) size))
                        .&&. close (1e-9 * sizeUsual size) (sizeUsual (unseen sizeMemory size)) (sizeUsual size / exp 1)
        , testProperty "the clip has been seen once any reflector has, and not before" $
            forAllShrink (genSizes 3000) shrinkSizes $ \sizes -> forAllShrink genOwn shrinkSight $ \sight ->
                conjoin
                    [ everSeen (pure noSize) === False
                    , everSeen (resized 0 1 <$> pure noSize <*> Three Nothing (Just sight) Nothing) === True
                    , everSeen sizes === any ((> 0) . sizeUsual) sizes
                    ]
        , testProperty "the biggest reflector lights, at a pose, its usual size for the distance; nothing with no pose or no reflector seen" $
            forAllShrink (genSizes 3000) shrinkSizes $ \sizes -> forAll (choose (250, 2500)) $ \mm ->
                let pose = Pose identity (V3 0 0 mm)
                    Three l r top = sizeUsual <$> sizes
                 in conjoin
                        [ biggestAt sizes Nothing === Nothing
                        , biggestAt (pure noSize) (Just pose) === Nothing
                        , case biggestAt sizes (Just pose) of
                            Nothing -> everSeen sizes === False
                            Just lit -> close (1e-9 * lit) (lit * (mm / 1000) ^ (2 :: Int)) (maximum [l, r, top])
                        ]
        , testProperty "reflectors each a blob of their own all show, and with one unseen, one is out" $
            forAllShrink (genSizes 3000) shrinkSizes $ \sizes -> forAllShrink (genThree genOwn) (shrinkThree shrinkSight) $ \(Three l r top) -> forAll genMetres $ \z ->
                showing sizes z (Just <$> Three l r top) === AllShowing
                    .&&. conjoin [showing sizes z sights === OneOut | sights <- [Three Nothing (Just r) (Just top), Three (Just l) Nothing (Just top), Three (Just l) (Just r) Nothing]]
        , testProperty "two reflectors sharing a blob both show, or one is perhaps behind the other: never is one surely out" $
            forAllShrink (genSizes 3000) shrinkSizes $ \sizes -> forAllShrink (genThree genOwn) (shrinkThree shrinkSight) $ \(Three l r top) -> forAll genMetres $ \z ->
                let shared s = Just s{sightKind = Merged}
                 in property (showing sizes z (Three (shared l) (shared r) (Just top)) `elem` [AllShowing, OneBehind])
        , testProperty "the less of the clip shows, the shorter it is followed so" $
            once (property (showingSeconds AllShowing > showingSeconds OneBehind && showingSeconds OneBehind > showingSeconds OneOut && showingSeconds OneOut > 0))
        ]

-- | A reflector's size: not seen yet, or seen at up to 3000 pixels at
-- 1 m, its usual size no less than its latest.
genSize :: Gen Size
genSize = (\(Three size _ _) -> size) <$> genSizes 3000

shrinkSize :: Size -> [Size]
shrinkSize size = [noSize | sizeUsual size > 0 || sizeLatest size > 0]

-- | A reflector seen as a blob of its own, or as two taken for one.
genOwn :: Gen Sight
genOwn = genSight 5000 `suchThat` ((/= Merged) . sightKind)

-- | Seconds since the last frame tracked, up to a minute.
genSince :: Gen Double
genSince = oneof [pure 0, choose (0, 0.1), choose (0, 60)]

-- | Metres from the camera, within its range.
genMetres :: Gen Double
genMetres = choose (0.25, 2.5)
