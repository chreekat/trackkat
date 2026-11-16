-- | Matching a frame's blobs to the clip's reflectors.
module Test.Old.Match (tests) where

import Data.Foldable (toList)
import Data.List (sort)
import Data.Maybe (catMaybes)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Test.Check
import Test.Gen hiding (Sizes (..), genSizes, shrinkSizes)
import Test.Old.Simulation (bentWorn, clipBent, distance, headSeen)
import TrackKat.Frame (Blob (..))
import TrackKat.Old.Match (Kind (..), Match (..), Sight (..), matchScore, matches, noSize, sizeScore)
import TrackKat.Old.Tracker (findBlobs)
import TrackKat.Pose
import TrackKat.Synth
import TrackKat.Term (Finding (..), Name (..), Score (..))
import qualified TrackKat.Term as Term

tests :: TestTree
tests = group "Match" $ do
    -- A match's cost is what its terms come to.
    let face = headSeen bentWorn 0 0 0
        found = matches defaultIntrinsics clipBent face (pure noSize) ((findBlobs . renderDiscs) (((340, 288), 3) : pointDiscs defaultIntrinsics clipBent allLit (headSeen bentWorn 3 1 0)))
    checkShowing "a match's terms are its cost" (show (map (Term.renderFindings . matchJudged) (take 2 found))) $
        not (null found)
            && and [abs (sum (map (nats . findingScore) (matchJudged m)) - nats (matchScore m)) < 1e-9 && nats (matchScore m) == negate (matchCost m) / 2 | m <- found]
            && map (map (brief . findingName) . matchJudged) (take 1 found) == [["places", "sizes", "unseen"]]
    tree properties

-- | Costs are sums of a few squared distances of up to hundreds of pixels,
-- right to 1e-9.
properties :: TestTree
properties =
    testGroup
        "properties"
        [ testProperty "the clip seen just where it is predicted is the best match, each reflector a blob of its own, at no cost" $
            \scene -> forAnyOrder (blobsOf scene{sceneOff = []}) $ \blobs ->
                case matches defaultIntrinsics clipBent (scenePose scene) (pure noSize) blobs of
                    [] -> counterexample "no match" False
                    best : _ ->
                        counterexample (show best) $
                            conjoin
                                [ fmap (fmap sightAt) (toList (matchSights best)) === map Just (predicted scene)
                                , property (all (maybe False ((== Whole) . sightKind)) (toList (matchSights best)))
                                , close 1e-9 (matchCost best) 0
                                , matchUnused best === length (sceneStrays scene)
                                ]
        , testProperty "blobs match the same in whatever order they come" $
            \scene -> forAnyOrder (blobsOf scene) $ \shuffled ->
                let found = matches defaultIntrinsics clipBent (scenePose scene) (pure noSize)
                    costs = sort . map matchCost . found
                 in length (costs shuffled) === length (costs (blobsOf scene)) .&&. conjoin (zipWith (close 1e-9) (costs shuffled) (costs (blobsOf scene)))
        , testProperty "matches come best first, each seeing two reflectors or more in blobs there are" $
            \scene ->
                let found = matches defaultIntrinsics clipBent (scenePose scene) (pure noSize) (blobsOf scene)
                 in map matchCost found === sort (map matchCost found)
                        .&&. property (all (\m -> length (catMaybes (toList (matchSights m))) >= 2 && matchUnused m >= 0 && matchUnused m <= length (blobsOf scene) - 2) found)
        , testProperty "a match's score is what its findings come to, and its cost twice that, in squared pixels" $
            \scene ->
                conjoin
                    [ close 1e-9 (sum (map (nats . findingScore) (matchJudged m))) (nats (matchScore m)) .&&. nats (matchScore m) === negate (matchCost m) / 2
                    | m <- matches defaultIntrinsics clipBent (scenePose scene) (pure noSize) (blobsOf scene)
                    ]
        , testProperty "a blob of the size expected, or of no known size, is as likely as can be, and less so the more it differs either way" $
            \(Expected expected) (Times less) (Times more) ->
                let times k = nats (sizeScore expected (k * expected))
                 in conjoin
                        [ sizeScore expected expected === Term.possible
                        , sizeScore 0 expected === Term.possible
                        , property (times (less * more) <= times less && times (1 / (less * more)) <= times (1 / less))
                        , close 1e-9 (times less) (times (1 / less))
                        ]
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

-- | How many pixels a blob is expected to have, up to a thousand.
newtype Expected = Expected Double
    deriving (Show)

instance Arbitrary Expected where
    arbitrary = Expected <$> choose (1, 1000)
    shrink (Expected pixels) = Expected <$> shrinkWithin (1, 1000) 1 pixels

-- | How many times as big, from as big to ten times.
newtype Times = Times Double
    deriving (Show)

instance Arbitrary Times where
    arbitrary = Times <$> choose (1, 10)
    shrink (Times k) = Times <$> shrinkWithin (1, 10) 1 k
