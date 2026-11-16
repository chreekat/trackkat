-- | Matching a frame's blobs to the clip's reflectors.
module Test.Match (tests) where

import Data.Foldable (toList)
import Data.List (sort)
import Data.Maybe (catMaybes)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Test.Check
import Test.Gen
import Test.Simulation (bentWorn, clipBent, distance, headSeen)
import TrackKat.Frame (Blob (..))
import TrackKat.Match (Kind (..), Match (..), Sight (..), matches, reflectors)
import TrackKat.Pose
import TrackKat.Synth
import TrackKat.Term (Finding (..), Name (..), Score (..))
import qualified TrackKat.Term as Term
import TrackKat.Tracker (findBlobs)
import TrackKat.Tracker.Account (metres)
import TrackKat.Tracker.Reflectors (Expect (..))
import TrackKat.Tracker.Sizes (unlearnt)

tests :: TestTree
tests = group "Match" $ do
    -- A match's rank is what its terms come to: what the frame shows of
    -- each reflector, the shift taken out, and the shift.
    let face = headSeen bentWorn 0 0 0
        found = matches (expectedAt face) 5 ((findBlobs . renderDiscs) (((340, 288), 3) : pointDiscs defaultIntrinsics clipBent allLit (headSeen bentWorn 3 1 0)))
    checkShowing "a match's terms are its rank" (show (map (Term.renderFindings . matchJudged) (take 1 found))) $
        not (null found)
            && and [abs (sum (map (nats . findingScore) (matchJudged m)) - nats (matchRank m)) < 1e-9 | m <- found]
            && map (map (brief . findingName) . matchJudged) (take 1 found) == [["left shows", "left place", "left size", "right shows", "right place", "right size", "top shows", "top place", "top size", "alike", "shift"]]
    tree properties

-- | What is expected of the clip's reflectors at a pose, nothing learnt
-- of them.
expectedAt :: Pose -> Expect
expectedAt pose = Expect (seenAt defaultIntrinsics clipBent pose <$> reflectors) (metres pose) (pure unlearnt) Nothing

-- | The spread of how far the clip's picture shifts that the properties
-- match by, in pixels.
shiftSpread :: Double
shiftSpread = 5

-- | Ranks are sums of a few terms of up to hundreds of nats, right to
-- 1e-9.
properties :: TestTree
properties =
    testGroup
        "properties"
        [ testProperty "the clip seen just where it is predicted is the best ranked match, each reflector a blob of its own" $
            \scene -> forAnyOrder (blobsOf scene{sceneOff = []}) $ \blobs ->
                case matches (expectedAt (scenePose scene)) shiftSpread blobs of
                    [] -> counterexample "no match" False
                    best : _ ->
                        counterexample (show best) $
                            conjoin
                                [ fmap (fmap sightAt) (toList (matchSights best)) === map Just (predicted scene)
                                , property (all (maybe False ((== Whole) . sightKind)) (toList (matchSights best)))
                                , matchUnused best === length (sceneStrays scene)
                                ]
        , testProperty "two blobs are a reflector in pieces just when each one's centre is inside a disc of their pixels about the centre of both" $
            \scene broken ->
                let found = matches (expectedAt (scenePose scene)) shiftSpread (blobsOf (brokenIn broken scene))
                    rejoined = any (any (maybe False ((== Rejoined) . sightKind)) . matchSights) found
                 in counterexample (show (blobsOf (brokenIn broken scene))) (rejoined === (brokenReach broken < 1))
        , testProperty "blobs match the same in whatever order they come" $
            \scene -> forAnyOrder (blobsOf scene) $ \shuffled ->
                let ranks = sort . map (nats . matchRank) . matches (expectedAt (scenePose scene)) shiftSpread
                 in length (ranks shuffled) === length (ranks (blobsOf scene)) .&&. conjoin (zipWith (close 1e-9) (ranks shuffled) (ranks (blobsOf scene)))
        , testProperty "matches come best ranked first, each seeing two reflectors or more in blobs there are" $
            \scene ->
                let found = matches (expectedAt (scenePose scene)) shiftSpread (blobsOf scene)
                 in map (negate . nats . matchRank) found === sort (map (negate . nats . matchRank) found)
                        .&&. property (all (\m -> length (catMaybes (toList (matchSights m))) >= 2 && matchUnused m >= 0 && matchUnused m <= length (blobsOf scene) - 1) found)
        , testProperty "a match's rank is what its findings come to" $
            \scene ->
                conjoin
                    [ close 1e-9 (sum (map (nats . findingScore) (matchJudged m))) (nats (matchRank m))
                    | m <- matches (expectedAt (scenePose scene)) shiftSpread (blobsOf scene)
                    ]
        , testProperty "a reflector may be unseen whatever blobs are left over: of the clip seen where predicted, there is a match with each one out" $
            \scene ->
                let found = matches (expectedAt (scenePose scene)) shiftSpread (blobsOf scene{sceneOff = []})
                    seenAs m = fmap (fmap sightAt) (toList (matchSights m))
                 in conjoin [counterexample (show out) (any ((== out) . seenAs) found) | i <- [0 .. 2 :: Int], let out = [if i == j then Nothing else Just place | (j, place) <- zip [0 ..] (predicted scene)]]
        ]

-- | The clip where a pose puts it, its reflectors seen each a little off
-- and of some size, among stray blobs.
data SeenClip = SeenClip
    { scenePlacing :: Placing
    , sceneOff :: [(Double, Double)]
    -- ^ how far off each reflector's blob is; none for not at all
    , sceneSizes :: [Int]
    , sceneStrays :: [((Double, Double), Int)]
    }
    deriving (Show)

scenePose :: SeenClip -> Pose
scenePose = placed . scenePlacing

-- | Where the pose puts each reflector in the picture.
predicted :: SeenClip -> [(Double, Double)]
predicted scene = map (project defaultIntrinsics (scenePose scene)) [mLeft clipBent, mRight clipBent, mTop clipBent]

blobsOf :: SeenClip -> [Blob]
blobsOf scene =
    zipWith3 (\(u, v) (du, dv) size -> blob (u + du, v + dv) size) (predicted scene) (sceneOff scene ++ repeat (0, 0)) (sceneSizes scene)
        ++ map (uncurry blob) (sceneStrays scene)
  where
    blob (x, y) size = Blob x y size (200 * size) (fromIntegral size / (4 * pi)) (fromIntegral size / (4 * pi)) 0

-- | One of a scene's reflectors as two blobs: which, the bigger blob's
-- share of its pixels, which way the smaller is from the bigger, in
-- radians, and how far the smaller's centre is from the centre of both,
-- in radii of a disc of the pixels of both: up to 1.4, and not within a
-- hundredth of 1. Simplest the left one in halves, side by side, half a
-- radius out.
data Broken = Broken
    { brokenWhich :: Int
    , brokenShare :: Double
    , brokenAngle :: Double
    , brokenReach :: Double
    }
    deriving (Eq, Show)

instance Arbitrary Broken where
    arbitrary = Broken <$> chooseInt (0, 2) <*> choose (0.5, 0.8) <*> choose (0, 2 * pi) <*> oneof [choose (0.1, 0.99), choose (1.01, 1.4)]
    shrink =
        shrinkTo
            (Broken 0 0.5 0 0.5)
            ( \broken ->
                [broken{brokenWhich = 0}, broken{brokenShare = 0.5}, broken{brokenAngle = 0}]
                    ++ [broken{brokenReach = if brokenReach broken < 1 then 0.5 else 1.2}]
            )

-- | A scene with one of its reflectors in two pieces, and nothing else
-- but the clip, each other reflector just where it is predicted.
brokenIn :: Broken -> SeenClip -> SeenClip
brokenIn (Broken which share angle reach) scene =
    scene
        { sceneOff = [if i == which then (negate smaller * apart * cos angle, negate smaller * apart * sin angle) else (0, 0) | i <- [0 .. 2]]
        , sceneSizes = [if i == which then big else size | (i, size) <- zip [0 ..] (sceneSizes scene)]
        , sceneStrays = [((u + bigger * apart * cos angle, v + bigger * apart * sin angle), small)]
        }
  where
    whole = sceneSizes scene !! which
    big = round (share * fromIntegral whole) :: Int
    small = whole - big
    bigger = fromIntegral big / fromIntegral whole :: Double
    smaller = 1 - bigger
    apart = reach * sqrt (fromIntegral whole / pi) / bigger
    (u, v) = predicted scene !! which

-- | A clip seen whole with its reflectors well apart in the picture, each
-- blob within 3 pixels of where its reflector is, and up to four strays
-- well away from them. Simplest squarely facing the camera a metre
-- straight ahead, alone, its blobs the smallest and just where their
-- reflectors are; and simpler with fewer strays, one of those so, or a
-- simpler pose.
instance Arbitrary SeenClip where
    arbitrary = do
        placing <- genSeen 40 trackedRange clipBent `suchThat` (\placing -> laidOut (SeenClip placing [] [] []))
        off <- vectorOf 3 ((,) <$> choose (-3, 3) <*> choose (-3, 3))
        sizes <- vectorOf 3 (chooseInt (30, 300))
        count <- chooseInt (0, 4)
        strays <- vectorOf count (((,) <$> ((,) <$> choose (0, 640) <*> choose (0, 480)) <*> chooseInt (30, 300)) `suchThat` (\stray -> laidOut (SeenClip placing [] [] [stray])))
        pure (SeenClip placing off sizes strays)
    shrink =
        shrinkParts
            [ \scene -> [scene{sceneStrays = strays} | strays <- shrinkList (const []) (sceneStrays scene)]
            , \scene -> [scene{sceneOff = []} | not (null (sceneOff scene))]
            , \scene -> [scene{sceneSizes = [30, 30, 30]} | sceneSizes scene /= [30, 30, 30]]
            , \scene -> [simpler | placing <- shrinkSeen trackedRange clipBent (scenePlacing scene), let simpler = scene{scenePlacing = placing}, laidOut simpler]
            ]

-- | Whether a scene's reflectors are 40 pixels or more apart in the
-- picture, and its strays 60 or more from each.
laidOut :: SeenClip -> Bool
laidOut scene =
    and [distance a b >= 40 | (i, a) <- zip [0 :: Int ..] places, (j, b) <- zip [0 ..] places, i < j]
        && and [distance at place >= 60 | (at, _) <- sceneStrays scene, place <- places]
  where
    places = predicted scene
