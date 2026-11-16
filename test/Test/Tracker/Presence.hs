-- | Things that come and go (see "TrackKat.Tracker.Presence").
module Test.Tracker.Presence (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Test.Gen
import Test.Instances ()
import TrackKat.Tracker.Presence
import TrackKat.World (Seconds, inSeconds, seconds)

-- | The chances of being there or gone after a time follow from how long
-- something stays and stays away, whatever the time: they are chances,
-- they come to the same over two times as over their sum, and what being
-- gone costs is one going and then its time away, however many frames
-- that is cut into.
tests :: TestTree
tests =
    testGroup
        "Presence"
        [ testProperty "from either state, something is then there or gone: the two chances come to one" $
            \(Comes comings) (Within since) from ->
                close 1e-12 (exp (passing comings since from Present) + exp (passing comings since from Gone)) 1
        , testProperty "over two times, one after the other, the chances are those over their sum, by way of either state between" $
            \(Comes comings) (Within first) (Within second) from to ->
                close
                    1e-12
                    (exp (passing comings (seconds (inSeconds first + inSeconds second)) from to))
                    (sum [exp (passing comings first from between + passing comings second between to) | between <- [Present, Gone]])
        , testProperty "no time on, it is as it was, and can't be otherwise" $
            \(Comes comings) from ->
                conjoin
                    [ close 1e-12 (passing comings (seconds 0) from from) 0
                    , property (isInfinite (passing comings (seconds 0) from (other from)))
                    ]
        , testProperty "long after, how it was says nothing: it is there for its share of the time" $
            \(Comes comings) from to ->
                let long = seconds (100 * (inSeconds (staysFor comings) + inSeconds (awayFor comings)))
                 in close 1e-9 (passing comings long from to) (usually comings to)
                        .&&. close 1e-12 (exp (usually comings Present)) (inSeconds (staysFor comings) / (inSeconds (staysFor comings) + inSeconds (awayFor comings)))
        , testProperty "it is no likelier to have changed after a shorter time, and never likelier than with nothing known of how it was" $
            \(Comes comings) (Within since) (Fraction sooner) from ->
                let changed dt = passing comings dt from (other from)
                 in property (changed (seconds (sooner * inSeconds since)) <= changed since + 1e-12 && changed since <= usually comings (other from) + 1e-12)
        , testProperty "going between two frames is unlikely by the frame's share of the time it stays, to within that share" $
            \(Comes comings) (Rate rate) ->
                let share = 1 / (rate * inSeconds (staysFor comings))
                 in property (exp (passing comings (seconds (1 / rate)) Present Gone) <= share)
                        .&&. close (share * (share + 1 / (rate * inSeconds (awayFor comings)))) (exp (passing comings (seconds (1 / rate)) Present Gone)) share
        , testProperty "staying gone costs by the time and not by the frames: at 60 and at 120 frames a second it comes to the same within a tenth" $
            \(Comes comings) (Within lasting) ->
                let gone rate = rate * inSeconds lasting * passing comings (seconds (1 / rate)) Gone Gone
                 in close (0.1 * abs (gone 120) + 1e-9) (gone 60) (gone 120)
        ]

other :: Presence -> Presence
other Present = Gone
other Gone = Present

-- | How something comes and goes: staying from a second to two minutes at
-- a time, and away from half a second to half a minute. Simplest a second
-- of each, and simpler at whole seconds.
newtype Comes = Comes Comings
    deriving (Show)

instance Arbitrary Comes where
    arbitrary = Comes <$> (Comings . seconds <$> choose (1, 120) <*> (seconds <$> choose (0.5, 30)))
    shrink (Comes comings) =
        Comes
            <$> shrinkParts
                [ \c -> [c{staysFor = seconds stay} | stay <- shrinkWithin (1, 120) 1 (inSeconds (staysFor c))]
                , \c -> [c{awayFor = seconds away} | away <- shrinkWithin (0.5, 30) 1 (inSeconds (awayFor c))]
                ]
                comings

-- | A length of time of up to a minute; simplest none.
newtype Within = Within Seconds
    deriving (Show)

instance Arbitrary Within where
    arbitrary = Within . seconds <$> choose (0, 60)
    shrink (Within d) = Within . seconds <$> shrinkWithin (0, 60) 0 (inSeconds d)

-- | Frames a second, of a camera's rates.
newtype Rate = Rate Double
    deriving (Show)

instance Arbitrary Rate where
    arbitrary = Rate <$> elements [30, 60, 120, 240]
    shrink (Rate rate) = Rate <$> earlier [30, 60, 120, 240] rate
