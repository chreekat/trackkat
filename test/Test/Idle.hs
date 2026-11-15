-- | When a trackkat that a game started quits for want of use.
module Test.Idle (tests) where

import Data.List (mapAccumL)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Test.Gen (earlier)
import Test.Instances ()
import TrackKat.CommandLine (Start (..))
import TrackKat.Idle
import TrackKat.Log
import TrackKat.World (clockAt)

tests :: TestTree
tests =
    testGroup
        "Idle"
        [ testProperty "started by hand or at login, trackkat never quits for want of use, and says nothing of it" $
            \(Looking looks) (NotByGame start) ->
                map snd (run start looks) === map (const (Busy, [])) looks
        , testProperty "started by a game, trackkat is to quit just once every look for a minute has found it unused" $
            \(Looking looks) ->
                conjoin
                    [ counterexample (show (at, state)) $
                        (state == Quitting) === any (\since -> at - since >= fromIntegral idleSeconds) (unusedSince at looks)
                    | ((at, _), (state, _)) <- beforeQuitting (run ByGame looks)
                    ]
        , testProperty "the log says when the wait to quit begins, when it is called off and when it ends, each once" $
            \(Looking looks) ->
                let states = map (fst . snd) (run ByGame looks)
                 in conjoin
                        [ counterexample (show (was, now, said)) $ case (was, now) of
                            (Busy, IdleSince _) -> length said === 1
                            (IdleSince _, Busy) -> length said === 1
                            (IdleSince _, Quitting) -> length said === 1
                            _ -> said === []
                        | (was, (_, (now, said))) <- zip (Busy : states) (run ByGame looks)
                        ]
                        .&&. property (and [isUser line | (_, (_, said)) <- run ByGame looks, line <- said])
        , testProperty "once to quit, trackkat stays so" $
            \(Looking looks) start use ->
                idle start (clockAt (sum (map fst looks))) use Quitting === (Quitting, [])
        ]
  where
    isUser (ForUser _) = True
    isUser (ForDeveloper _) = False

-- | Looks at whether trackkat is in use: how long after the look before
-- each comes, in seconds, and what it finds.
type Looks = [(Double, Use)]

-- | Each look with when it was, and what came of it.
run :: Start -> Looks -> [((Double, Use), (Idle, [Line]))]
run start = snd . mapAccumL look (0, Busy)
  where
    look (clock, was) (after, use) =
        let at = clock + after
            (now, said) = idle start (clockAt at) use was
         in ((at, now), ((at, use), (now, said)))

-- | The looks up to and with the one that says to quit.
beforeQuitting :: [((Double, Use), (Idle, [Line]))] -> [((Double, Use), (Idle, [Line]))]
beforeQuitting made = case break ((== Quitting) . fst . snd) made of
    (before, quit : _) -> before ++ [quit]
    (before, []) -> before

-- | When the run of looks finding trackkat unused, up to the look at the
-- time given, began; none if that look found it in use.
unusedSince :: Double -> Looks -> [Double]
unusedSince at looks = case span ((== Unused) . snd) (reverse (takeWhile ((<= at) . fst) timed)) of
    ([], _) -> []
    (unused, _) -> [fst (last unused)]
  where
    timed = drop 1 (scanl (\(clock, _) (after, use) -> (clock + after, use)) (0, Used) looks)

-- | Looks half a second to a minute apart, in runs of the same finding;
-- simpler of fewer, each half a second after the last.
newtype Looking = Looking Looks
    deriving (Show)

instance Arbitrary Looking where
    arbitrary = Looking . concat <$> listOf (do use <- arbitrary; n <- chooseInt (1, 8); vectorOf n ((,) <$> gap <*> pure use))
      where
        gap = oneof [pure 0.5, choose (0.5, 20), elements [59.5, 60]]
    shrink (Looking looks) = Looking <$> shrinkList (\(after, use) -> [(0.5, use) | after /= 0.5]) looks

-- | A way trackkat is started that isn't by a game.
newtype NotByGame = NotByGame Start
    deriving (Show)

instance Arbitrary NotByGame where
    arbitrary = NotByGame <$> elements [ByHand, AtLogin]
    shrink (NotByGame start) = NotByGame <$> earlier [ByHand, AtLogin] start
