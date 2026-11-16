-- | Choosing what to make of a frame (see "TrackKat.Tracker.Choose").
module Test.Tracker.Choose (tests) where

import Data.Foldable (toList)
import Data.List (sortOn)
import Data.Maybe (isJust)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Test.Check
import Test.Gen
import Test.Instances ()
import Test.Simulation (headset)
import Test.Tracker.Known
import TrackKat.Frame (Blob (..))
import TrackKat.Lights (noMask)
import TrackKat.Match (Kind (..), Sight (..))
import TrackKat.Pose
import TrackKat.Term
import TrackKat.Tracker.Account
import TrackKat.Tracker.Blobs
import TrackKat.Tracker.Choose
import TrackKat.Tracker.Follow (Showing (..), showingSeconds)
import TrackKat.Tracker.Outcome
import TrackKat.Tracker.Sighting
import TrackKat.Tracker.Trail
import TrackKat.World (Seconds, clockAt, inSeconds, seconds, (.+^))

-- | What is made of a frame is the best scored hypothesis about it, and
-- nothing but the scores says which: not the order the blobs come in, nor
-- a stray no hypothesis uses, nor how a hypothesis was come by.
tests :: TestTree
tests = group "Choose" $ do
    -- Three blobs that no pose of the clip puts its reflectors on, with
    -- nothing else known: likelier three strays than the clip.
    let unlike = snd (makeOut knowing (clockAt 0) noTrail (sortBlobs noMask [roundBlob place 106 | place <- [(272, 394), (394, 279), (328, 410)]]))
    checkShowing "three blobs no pose of the clip fits aren't found as it, for where they are" (either (\(why, rivals) -> explainWhy why ++ explainRivals rivals) (const "found") unlike) $ case unlike of
        Left (Why Afresh (Unfit [found]), Rival OutOfView _ : _) -> brief (findingName found) `elem` ["left place", "right place", "top place"]
        _ -> False
    tree properties

properties :: TestTree
properties =
    testGroup
        "properties"
        [ testProperty "the best of some things is the first of those that score highest, of those that can be; and the rest come best first" $
            \(Scores scores) ->
                let numbered = zip [0 :: Int ..] scores
                    can = [x | x@(_, s) <- numbered, not (isImpossible s)]
                 in case best snd numbered of
                        Nothing -> can === []
                        Just (top, rest) ->
                            conjoin
                                [ property (top `elem` can)
                                , property (all (\(i, s) -> s < snd top || (s == snd top && i >= fst top)) can)
                                , sortOn fst (top : rest) === can
                                , map snd rest === sortOn (negate . nats) (map snd rest)
                                ]
        , testProperty "which score is best goes only by the scores, whatever order they come in" $
            \(Scores scores) -> forAnyOrder scores $ \shuffled ->
                fmap fst (best id shuffled) === fmap fst (best id scores)
        , testProperty "doubt is what the best other three blobs have had over the pose followed, in what is the frame's own, and never less than nothing" $
            \(Apart30 placing) (Apart30 other) (Doubt before) ->
                placing /= other ==>
                    conjoin
                        [ close 1e-9 (doubted before continued [rival]) (max 0 (before + nats (judgedFrame rival) - nats (judgedFrame continued)))
                        | continued <- take 1 (judgeClip (taught placing) aFrameOn (Origins (Just (Origin (placed placing) (seconds (1 / 120)))) Nothing) (candidate placing placing))
                        , rival <- judgeClip (taught placing) aFrameOn (Origins Nothing Nothing) (candidate other other)
                        ]
        , testProperty "doubt starts over with a pose come afresh, with the clip away, and with no other three blobs to weigh" $
            \(Apart30 placing) (Apart30 other) (Doubt before) ->
                let origins = Origins (Just (Origin (placed placing) (seconds (1 / 120)))) Nothing
                    known = taught placing
                 in conjoin
                        [ conjoin
                            [ doubted before afresh [rival] === 0
                            , doubted before (judgeAway known aFrameOn Nothing) [rival] === 0
                            , doubted before continued [] === 0
                            , doubted before continued [afresh, judgeAway known aFrameOn Nothing] === 0
                            ]
                        | [continued, afresh] <- [judgeClip known aFrameOn origins (candidate placing placing)]
                        , rival <- judgeClip known aFrameOn (Origins Nothing Nothing) (candidate other other)
                        ]
        , testProperty "the clip just where the last pose has it, as big as it was learnt, is made out as continuing it, every reflector seen and nothing else" $
            \(Apart30 placing) ->
                case snd (makeOut (taught placing) aFrameOn (tracking placing) (sortBlobs noMask (clipBlobs headset placing))) of
                    Right (Chosen way _ _ _, Sighting pose fit sights strays masked _) ->
                        conjoin
                            [ way === Continued
                            , property (all (maybe False ((== Whole) . sightKind)) sights)
                            , (strays, masked) === (0, 0)
                            , counterexample ("fit " ++ show fit) (fit < 0.5)
                            , counterexample ("turned " ++ show (poseChange pivot (placed placing) pose)) (fst (poseChange pivot (placed placing) pose) < 1)
                            ]
                    Left (why, _) -> counterexample (explainWhy why) False
        , testProperty "what is made of a frame scored no less than any other hypothesis about it, and a frame lost is likeliest the clip away or a pose not to be told" $
            \(Viewed placing strays) ->
                case snd (makeOut (taught placing) aFrameOn (tracking placing) (sortBlobs noMask (clipBlobs headset placing ++ strays))) of
                    Right (Chosen _ _ won rivals, _) -> property (and [won >= scored | Rival _ scored <- rivals])
                    Left (why, Rival guess _ : rivals) ->
                        property (and [a >= b | (Rival _ a, Rival _ b) <- zip rivals (drop 1 rivals)])
                            .&&. counterexample (explainWhy why) (guess == OutOfView || whyRejectedBy why == ["partly seen"])
                    Left (_, []) -> counterexample "nothing scored" False
        , testProperty "a frame is made of the same whatever order its blobs come in" $
            \(Viewed placing strays) -> forAnyOrder (clipBlobs headset placing ++ take 2 strays) $ \shuffled ->
                let made blobs = snd (makeOut (taught placing) aFrameOn (tracking placing) (sortBlobs noMask blobs))
                 in sameMade (made shuffled) (made (clipBlobs headset placing ++ take 2 strays))
        , testProperty "a stray far from the clip, which no likely hypothesis uses, changes nothing of what is made of the frame but the strays counted" $
            \(Apart30 placing) ->
                let blobs = clipBlobs headset placing
                    far stray = all (\b -> distance2 (at b) (at stray) > 200 * 200) blobs
                    made shown = snd (makeOut (taught placing) aFrameOn (tracking placing) (sortBlobs noMask shown))
                 in forAllShrink (genBlob 400 `suchThat` far) (filter far . shrinkBlob) $ \stray ->
                        case (made blobs, made (blobs ++ [stray])) of
                            (Right (Chosen way _ won _, alone), Right (Chosen way' _ won' _, beside)) ->
                                conjoin
                                    [ way' === way
                                    , close 1e-9 (nats won') (nats won)
                                    , sightingSights beside === sightingSights alone
                                    , sightingStrays beside === sightingStrays alone + 1
                                    , closeV 1e-9 (poseT (sightingPose beside)) (poseT (sightingPose alone))
                                    ]
                            _ -> counterexample "lost" False
        , testProperty "nothing is found afresh, with nothing else known, at a pose that fits its blobs worse than fifteen pixels" $
            \(ReflectorBlobs three) ->
                case snd (makeOut knowing (clockAt 0) noTrail (sortBlobs noMask (toList three))) of
                    Right (Chosen way _ _ _, sighting) -> way === Appeared .&&. counterexample ("fit " ++ show (sightingFit sighting)) (sightingFit sighting < 15)
                    Left _ -> property True
        , testProperty "the clip where it was, every reflector's blob in its place, is made out however long since all three last showed" $
            \(Apart30 placing) (Since since) ->
                let last' = (lastAt (placed placing)){recentAllSeen = lastTracked .+^ seconds (negate (inSeconds since))}
                 in case snd (makeOut (taught placing) aFrameOn (Trail (Just last') Nothing) (sortBlobs noMask (clipBlobs headset placing))) of
                        Right (Chosen way _ _ _, sighting) -> way === Continued .&&. property (all isJust (sightingSights sighting))
                        Left (why, _) -> counterexample (explainWhy why) False
        , testProperty "no pose is told of the clip showing two reflectors for longer than it may since all three showed, and the frame lost says so" $
            \(Apart30 placing) (Since since) ->
                let last' = (lastAt (placed placing)){recentAllSeen = aFrameOn .+^ seconds (negate (inSeconds since))}
                    two = take 2 (clipBlobs headset placing)
                 in case snd (makeOut (taught placing) aFrameOn (Trail (Just last') Nothing) (sortBlobs noMask two)) of
                        Right (_, sighting) -> property (since <= showingSeconds OneOut) .&&. length (filter isJust (toList (sightingSights sighting))) === 2
                        Left (why, _) -> counterexample (explainWhy why) (since > showingSeconds OneOut) .&&. whyRejectedBy why === ["partly seen"]
        , testProperty "one blob isn't the clip, whatever it is, and no blobs are nothing to go by" $
            \(Apart30 placing) (Stray b) ->
                let made known trail blobs = snd (makeOut known aFrameOn trail (sortBlobs noMask blobs))
                 in conjoin
                        [ either (const (property True)) (const (counterexample "made out of one blob" False)) (made (taught placing) (tracking placing) [b])
                        , either (\(why, _) -> why === Why Following NothingMatches) (const (counterexample "made out of nothing" False)) (made (taught placing) (tracking placing) [])
                        , either (\(why, _) -> whyRejectedBy why === ["blobs"]) (const (counterexample "made out of nothing" False)) (made knowing noTrail [b])
                        ]
        ]

-- | The clip made out as the three blobs it shows at one pose, taken to
-- be at another.
candidate :: Placing -> Placing -> Candidate
candidate shown at' = Candidate (placed at') 0 (maybe (pure Nothing) (fmap whole) (asReflectors shown)) 0 0 ByLayout

-- | That two frames were made of alike: lost both, or the clip made out
-- the same way of the same blobs at the same pose.
sameMade :: Either (Why, [Rival]) (Chosen, Sighting) -> Either (Why, [Rival]) (Chosen, Sighting) -> Property
sameMade (Right (Chosen way _ _ _, a)) (Right (Chosen way' _ _ _, b)) =
    conjoin
        [ way === way'
        , sightingSights a === sightingSights b
        , closeV 1e-6 (poseT (sightingPose a)) (poseT (sightingPose b))
        , sightingStrays a === sightingStrays b
        ]
sameMade (Left (why, _)) (Left (why', _)) = counterexample (explainWhy why ++ " and " ++ explainWhy why') (sameWhy why why')
sameMade _ _ = counterexample "one lost, one not" False

-- | The clip at a pose where each reflector shows as a blob of its own,
-- and up to four strays of up to 400 pixels well away from it. Simplest
-- alone; simpler with fewer strays, and at a simpler pose.
data Viewed = Viewed Placing [Blob]
    deriving (Show)

instance Arbitrary Viewed where
    arbitrary = do
        Apart30 placing <- arbitrary
        count <- chooseInt (0, 4)
        Viewed placing <$> vectorOf count (genBlob 400 `suchThat` clearOf placing)
    shrink (Viewed placing strays) =
        [Viewed placing fewer | fewer <- shrinkList (const []) strays]
            ++ [Viewed simpler strays | Apart30 simpler <- shrink (Apart30 placing), all (clearOf simpler) strays]

-- | Whether a blob is 60 pixels or more from each of the clip's at a pose.
clearOf :: Placing -> Blob -> Bool
clearOf placing stray = all (\b -> distance2 (at b) (at stray) >= 60 * 60) (clipBlobs headset placing)

-- | A few scores, some of what can't be. Simplest none.
newtype Scores = Scores [Score]
    deriving (Show)

instance Arbitrary Scores where
    arbitrary = Scores <$> listOf arbitrary
    shrink (Scores scores) = Scores <$> shrinkList shrink scores

-- | Doubt of the pose followed, up to a hundred nats; simplest none.
newtype Doubt = Doubt Double
    deriving (Show)

instance Arbitrary Doubt where
    arbitrary = Doubt <$> choose (0, 100)
    shrink (Doubt d) = Doubt <$> shrinkWithin (0, 100) 0 d

-- | Seconds since all three reflectors showed, up to four.
newtype Since = Since Seconds
    deriving (Show)

instance Arbitrary Since where
    arbitrary = Since . seconds <$> choose (0, 4)
    shrink (Since x) = Since . seconds <$> shrinkWithin (0, 4) 0 (inSeconds x)
