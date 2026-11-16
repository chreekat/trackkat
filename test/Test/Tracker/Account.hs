-- | The account every hypothesis about a frame is scored by (see
-- "TrackKat.Tracker.Account").
module Test.Tracker.Account (tests) where

import Data.Foldable (toList)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Test.Check
import Test.Gen
import Test.Instances ()
import Test.Simulation (headset)
import Test.Tracker.Known
import TrackKat.Linear
import TrackKat.Match (Three (..), reflectors)
import TrackKat.Pose
import TrackKat.Term
import TrackKat.Tracker.Account
import TrackKat.Tracker.Blobs (whole)
import TrackKat.Tracker.Presence
import TrackKat.World (Seconds, inSeconds, seconds, (.+^))

-- | A hypothesis's score is the sum of the terms of three parts and
-- nothing else; each term is greatest where its figure is as expected and
-- no greater further off; and nothing is impossible but a pose outside
-- the camera's range.
tests :: TestTree
tests = group "Account" $ do
    -- What the account's own numbers come to, for docs/heuristics.md: the
    -- room a pose has a frame, a tenth and half a second after another,
    -- and anywhere, with three reflectors seen a metre off; and what a
    -- blob just where a reflector should be says.
    let aMetreOff = Pose identity (V3 0 0 1000)
        seen = poseInformation defaultIntrinsics headset pivot aMetreOff [Observation which 1 (0, 0) | which <- toList reflectors]
        rooms = map (\dt -> room (followingRoom (seconds dt)) seen) [1 / 120, 0.1, 0.5] ++ [room anywhereRoom seen]
    checkShowing "the room of a pose a frame, 0.1 s and 0.5 s after another, and anywhere; and a blob's gain" (show rooms) $
        and (zipWith (>) rooms (drop 1 rooms)) && all (< 0) rooms && rooms !! 0 > -8 && rooms !! 3 < -20 && rooms !! 3 > -32
    tree properties

properties :: TestTree
properties =
    testGroup
        "properties"
        [ testProperty "a head can be anywhere in the camera's range, and nowhere else: nothing else is impossible" $
            \placing (CentredAt centre) ->
                let pose = placed placing
                    distance = norm (poseT pose)
                 in accepts (whereItIs (Reach pivot (placed <$> centre))) pose === (distance >= fst trackedRange && distance <= snd trackedRange)
        , testProperty "where a head was recentered nothing is against it, nor toward or away from the camera there; turned or aside further, no more for it" $
            \(SeenWhole placing) (Turn turn) (Fraction less) (Aside aside) ->
                let centre = placed placing
                    judge = nats . score (whereItIs (Reach pivot (Just centre)))
                    along d = let c = poseAt centre pivot in Pose (poseR centre) (poseT centre ^+^ (d *^ normalize c))
                    turned by = turnedAbout pivot (rodrigues by) centre
                    moved by = Pose (poseR centre) (poseT centre ^+^ (by *^ sideways centre))
                    reaching = inRange centre && inRange (along 50)
                 in reaching ==>
                        conjoin
                            [ judge centre === 0
                            , close 1e-6 (judge (along 50)) 0
                            , property (not (inRange (turned turn) && inRange (turned (less *^ turn))) || judge (turned turn) <= judge (turned (less *^ turn)) + 1e-9)
                            , property (not (inRange (moved aside) && inRange (moved (less * aside))) || judge (moved aside) <= judge (moved (less * aside)) + 1e-9)
                            ]
        , testProperty "a pose that neither turned nor moved continues the last as well as any can; further is no likelier, and longer since no less" $
            \(Jump (turn, move, dt)) (Fraction less) (FrameGap longer) ->
                let came since a b = sum [nats (findingScore f) | f <- findings (howItCame (Just since) 0 (Just since)) (Came a b 0), brief (findingName f) `elem` ["turning", "moving"]]
                    later = seconds (inSeconds dt + inSeconds longer)
                 in conjoin
                        [ came dt 0 0 === 0
                        , property (came dt turn move <= came dt (less * turn) (less * move) + 1e-9)
                        , property (came later turn move >= came dt turn move - 1e-9)
                        ]
        , testProperty "what is seen leaves no room that wasn't there: none taken with nothing seen, less left the more is seen, and the more room there was" $
            \(SeenWhole placing) (FrameGap dt) ->
                let pose = placed placing
                    information which = poseInformation defaultIntrinsics headset pivot pose [Observation each 1 (0, 0) | each <- which]
                    left spreads which = room spreads (information which)
                    Three l r top = reflectors
                 in conjoin
                        [ close 1e-9 (left anywhereRoom []) 0
                        , property (left anywhereRoom [l, r, top] <= left anywhereRoom [l, r] + 1e-9)
                        , property (left anywhereRoom [l, r] <= left anywhereRoom [l] + 1e-9)
                        , property (left anywhereRoom [l] <= 0)
                        , property (left anywhereRoom [l, r, top] <= left (followingRoom dt) [l, r, top] + 1e-9)
                        ]
        , testProperty "the most room a pose can be left, by how far it is and how much its reflectors count, is no less than it is left" $
            \(SeenWhole placing) (Weights weights) (FrameGap dt) ->
                let pose = placed placing
                    information = poseInformation defaultIntrinsics headset pivot pose (zipWith (\which w -> Observation which w (0, 0)) (toList reflectors) weights)
                 in conjoin
                        [ property (roomAtMost defaultIntrinsics headset spreads (metres pose) weights >= room spreads information - 1e-9)
                        | spreads <- [anywhereRoom, followingRoom dt]
                        ]
        , testProperty "the least a pose can lose for turning and moving, by how far its reflectors are seen from where the last pose has them, is no more than it loses" $
            \(SeenWhole placing) (Moved (turn, move)) (FrameGap dt) ->
                let from = placed placing
                    Pose r t = turnedAbout pivot (rodrigues turn) from
                    to = Pose r (t ^+^ move)
                    (turned, moved) = poseChange pivot from to
                    lost = negate (sum [nats (findingScore f) | f <- findings (howItCame (Just dt) 0 (Just dt)) (Came turned moved 0), brief (findingName f) `elem` ["turning", "moving"]])
                    offs =
                        [ (sqrt ((u - x) ^ (2 :: Int) + (v - y) ^ (2 :: Int)), depth)
                        | which <- toList reflectors
                        , let (u, v) = seenAt defaultIntrinsics headset from which
                              (x, y) = seenAt defaultIntrinsics headset to which
                              V3 _ _ depth = poseAt from (reflectorPoint headset which)
                        ]
                    ahead = and [z > 50 | which <- toList reflectors, pose <- [from, to], let V3 _ _ z = poseAt pose (reflectorPoint headset which)]
                 in ahead ==> counterexample (show (movedAtLeast defaultIntrinsics headset pivot dt offs, lost)) (movedAtLeast defaultIntrinsics headset pivot dt offs <= lost + 1e-9)
        , testProperty "the clip made out is judged as each way it could have come there, and scores what its terms found, part by part" $
            \(Apart30 placing) (Apart30 other) (HasLast hasLast) (HasLeft hasLeft) ->
                let known = taught placing
                    origins = Origins (if hasLast then Just (Origin (placed placing) (seconds (1 / 120))) else Nothing) (if hasLeft then Just (Origin (placed other) (seconds 0.1)) else Nothing)
                    judged = judgeClip known aFrameOn origins (candidate placing)
                 in map judgedGuess judged === [InView Continued ByLayout | hasLast] ++ [InView Resumed ByLayout | hasLeft] ++ [InView Appeared ByLayout]
                        .&&. conjoin
                            [ conjoin
                                [ close 1e-9 (nats (judgedScore j)) (sum (map (nats . findingScore) (judgedFindings j)))
                                , map fst (judgedParts j) === [Where, How, Shows]
                                , close 1e-9 (nats (judgedFrame j)) (sum [nats (findingScore f) | (part, found) <- judgedParts j, f <- found, part /= How || brief (findingName f) `elem` ["turning", "moving"]])
                                , judgedFindings j === concatMap snd (judgedParts j)
                                ]
                            | j <- judged
                            ]
        , testProperty "the clip where it was, seen as it was, is likelier continuing than come afresh: by the room it had anywhere over the room it had there, and by its not having left" $
            \(Apart30 placing) ->
                case judgeClip (taught placing) aFrameOn (Origins (Just (Origin (placed placing) (seconds (1 / 120)))) Nothing) (candidate placing) of
                    [continued, afresh] ->
                        let found name j = sum [nats (findingScore f) | f <- judgedFindings j, brief (findingName f) == name]
                         in property (judgedScore continued > judgedScore afresh)
                                .&&. close 1e-6 (nats (judgedScore continued) - nats (judgedScore afresh)) (found "room" continued - found "room" afresh + found "in view" continued - found "left" afresh)
                                .&&. close 1e-9 (found "left" afresh) (passing clipComings (seconds (1 / 120)) Present Gone)
                    others -> counterexample (show (map judgedGuess others)) False
        , testProperty "a pose come afresh before the clip was ever made out has it in view as likely as it usually is" $
            \(Apart30 placing) ->
                [nats (findingScore f) | j <- judgeClip (taught placing){knownMadeOut = Nothing} aFrameOn (Origins Nothing Nothing) (candidate placing), f <- judgedFindings j, brief (findingName f) == "in view"]
                    === [usually clipComings Present]
        , testProperty "the clip away says where it was, if a pose says, and that it went from view since it was made out, and nothing of the blobs" $
            \(Apart30 placing) (FrameGap since) ->
                let known = taught placing
                    now = lastTracked .+^ since
                    away = judgeAway known now (Just (placed placing))
                    nowhere = judgeAway known now Nothing
                 in conjoin
                        [ judgedGuess away === OutOfView
                        , map fst (judgedParts away) === [Where, How]
                        , map (brief . findingName) (judgedFindings nowhere) === ["out of view"]
                        , close 1e-9 (nats (judgedScore nowhere)) (passing clipComings since Present Gone)
                        , close 1e-9 (nats (judgedScore (judgeAway known{knownMadeOut = Nothing} now Nothing))) (usually clipComings Gone)
                        , close 1e-9 (nats (judgedScore away)) (sum (map (nats . findingScore) (judgedFindings away)))
                        ]
        , testProperty "doubt of the pose followed is credited to a pose come afresh as three blobs, and to no other" $
            \(Apart30 placing) (Doubt doubt) ->
                let origins = Origins (Just (Origin (placed placing) (seconds (1 / 120)))) Nothing
                    scores known = map (nats . judgedScore) (judgeClip known aFrameOn origins (candidate placing))
                 in case (scores (taught placing), scores (taught placing){knownDoubt = doubt}) of
                        ([continued, afresh], [continued', afresh']) -> close 1e-9 continued' continued .&&. close 1e-9 afresh' (afresh + doubt)
                        _ -> counterexample "not two ways" False
        ]
  where
    inRange pose = let d = norm (poseT pose) in d >= fst trackedRange && d <= snd trackedRange
    -- A direction square to the line of sight to a pose's pivot.
    sideways pose = let c = normalize (poseAt pose pivot) in normalize (cross c (if abs (dot c (V3 0 1 0)) < 0.9 then V3 0 1 0 else V3 1 0 0))

-- | The clip made out as its three blobs at a pose where each reflector
-- shows as a blob of its own, at that pose.
candidate :: Placing -> Candidate
candidate placing = Candidate (placed placing) 0 (maybe (pure Nothing) (fmap whole) (asReflectors placing)) 0 0 ByLayout

-- | Where a head was recentered, if anywhere.
newtype CentredAt = CentredAt (Maybe Placing)
    deriving (Show)

instance Arbitrary CentredAt where
    arbitrary = CentredAt <$> oneof [pure Nothing, Just <$> genSeen 60 trackedRange headset]
    shrink (CentredAt centre) = CentredAt <$> maybe [] (\placing -> Nothing : map Just (shrinkSeen trackedRange headset placing)) centre

-- | How far aside a head moved, up to a metre.
newtype Aside = Aside Double
    deriving (Show)

instance Arbitrary Aside where
    arbitrary = Aside <$> choose (0, 1000)
    shrink (Aside d) = Aside <$> shrinkWithin (0, 1000) 0 d

-- | Degrees turned and mm moved from the last pose, and the time since:
-- up to 60 degrees and 300 mm.
newtype Jump = Jump (Double, Double, Seconds)
    deriving (Show)

instance Arbitrary Jump where
    arbitrary = (\turn move (FrameGap dt) -> Jump (turn, move, dt)) <$> choose (0, 60) <*> choose (0, 300) <*> arbitrary
    shrink (Jump (turn, move, dt)) =
        Jump
            <$> shrinkParts
                [ \(a, b, c) -> [(a', b, c) | a' <- shrinkWithin (0, 60) 0 a]
                , \(a, b, c) -> [(a, b', c) | b' <- shrinkWithin (0, 300) 0 b]
                , \(a, b, c) -> [(a, b, c') | FrameGap c' <- shrink (FrameGap c)]
                ]
                (turn, move, dt)

-- | How a head turned and moved since the last pose: by up to 25 degrees
-- and 60 mm each way.
newtype Moved = Moved (V3, V3)
    deriving (Show)

instance Arbitrary Moved where
    arbitrary = Moved <$> ((,) <$> genTurn 25 <*> genV3 60)
    shrink (Moved (turn, move)) = Moved <$> shrinkParts [\(a, b) -> [(a', b) | a' <- shrinkTurn a], \(a, b) -> [(a, b') | b' <- shrinkV3 b]] (turn, move)

-- | How much each reflector's place counts, from a twentieth to all.
newtype Weights = Weights [Double]
    deriving (Show)

instance Arbitrary Weights where
    arbitrary = Weights <$> vectorOf 3 (choose (0.05, 1))
    shrink (Weights weights) = [Weights [1, 1, 1] | weights /= [1, 1, 1]]

-- | Whether a pose still predicts, and whether one was left behind.
newtype HasLast = HasLast Bool
    deriving (Show)

instance Arbitrary HasLast where
    arbitrary = HasLast <$> arbitrary
    shrink (HasLast has) = [HasLast False | has]

newtype HasLeft = HasLeft Bool
    deriving (Show)

instance Arbitrary HasLeft where
    arbitrary = HasLeft <$> arbitrary
    shrink (HasLeft has) = [HasLeft False | has]

-- | Doubt of the pose followed, up to a hundred nats.
newtype Doubt = Doubt Double
    deriving (Show)

instance Arbitrary Doubt where
    arbitrary = Doubt <$> choose (0.001, 100)
    shrink (Doubt d) = Doubt <$> shrinkWithin (0.001, 100) 1 d
