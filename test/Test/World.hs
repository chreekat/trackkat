-- | Quantities and the world's facts (see "TrackKat.World").
module Test.World (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import Test.Tasty.QuickCheck

import TrackKat.World

-- | A quantity is its number and no more: nothing is computed with one that
-- could round differently from computing with the number (see Note [One
-- world]).
tests :: TestTree
tests =
    testGroup
        "World"
        [ testProperty "a quantity is the number it was made from" $ \x ->
            conjoin
                [ inDegrees (degrees x) === x
                , inMillimetres (millimetres x) === x
                , inPixels (pixels x) === x
                , inSeconds (seconds x) === x
                , inPerSecond (perSecond x :: DegreesPerSecond) === x
                ]
        , testProperty "quantities compare as their numbers do" $ \x y ->
            compare (millimetres x) (millimetres y) === compare x y .&&. (millimetres x == millimetres y) === (x == y)
        , testProperty "a multiple of a quantity is that multiple of its number" $ \k x ->
            inMillimetres (times k (millimetres x)) === k * x
        , testProperty "a rate comes to its number times the seconds" $ \rate t ->
            inDegrees (during (perSecond rate) (seconds t)) === rate * t
        , testCase "the world's facts agree with each other" $ do
            let w = world
                perFrame speed = inPerSecond speed / inPerSecond (frameRate w)
            assertBool "the camera's range" $ 0 < inMillimetres (cameraNearest w) && cameraNearest w < cameraFarthest w
            assertBool "a neck is nearer the clip than the farthest it is looked for" $ neckBehindClip w < neckWithin w
            assertBool "blobs of one reflector differ" $ sameSizeFactor w > 1
            -- The fastest a head goes is at two spreads of its speed.
            assertBool "a followed pose's step allows the fastest head" $
                inDegrees (headStepTurn w) >= 2 * perFrame (headTurnSpeed w) && inMillimetres (headStepMove w) >= 2 * perFrame (headMoveSpeed w)
        ]
