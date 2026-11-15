-- | Choosing among the hypotheses about a frame (see
-- "TrackKat.Tracker.Choose").
module Test.Tracker.Choose (tests) where

import Data.Either (isRight)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import TrackKat.Match (unseenScore)
import TrackKat.Term
import TrackKat.Tracker.Choose
import TrackKat.Tracker.Layout (Layout (..))
import TrackKat.Tracker.Outcome

-- Note [What choosing promises]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The choice is between what the hypotheses made of a frame, and it can
-- make nothing else of it: here following makes 'ByFollowing' and the
-- layout 'ByLayout', and those are all that can come out. It goes by
-- scores alone, so it is tested with scores made up, on both sides of
-- every comparison and on them.
--
-- A hypothesis that isn't needed isn't worked out: where one must not be
-- looked at, the test gives one that fails if it is.
tests :: TestTree
tests =
    testGroup
        "Choose"
        [ testProperty "a frame is made of what following or the layout made of it, in the way that one says" $
            forAllShrink genHypotheses shrinkHypotheses $ \h -> case (decide h, followed h, byLayout h) of
                (Right (Chosen Followed _, made), Right _, _) -> made === ByFollowing
                (Right (Chosen way _, made), _, Just (Right found)) -> made === ByLayout .&&. way === layoutWay found
                (Left _, Left _, _) -> property True
                (chosen, _, _) -> counterexample (show chosen) False
        , testProperty "following that isn't weak is taken, whatever else there is, and nothing else is worked out" $
            forAllShrink genHypotheses shrinkHypotheses $ \h -> forAll (choose (nats weakScore, 0)) $ \strong ->
                decide h{followed = Right (Score strong, ByFollowing), byLayout = unwanted, stillWhereItWas = unwanted}
                    === Right (Chosen Followed Nothing, ByFollowing)
        , testProperty "what following made out isn't lost" $
            forAllShrink genHypotheses shrinkHypotheses $ \h ->
                isRight (followed h) ==> property (isRight (decide h))
        , testProperty "of weak following and a layout, the better scored is taken, following if they score alike, and told with both scores" $
            forAllShrink genHypotheses shrinkHypotheses $ \h -> forAllShrink genLayout shrinkLayout $ \found -> forAll genWeak $ \weak ->
                decide h{followed = Right (weak, ByFollowing), byLayout = Just (Right found), stillWhereItWas = unwanted}
                    === Right
                        ( if layoutScore found > weak
                            then (Chosen (layoutWay found) (Just (Tried (Outscored (layoutScore found) weak) (layoutJump found))), ByLayout)
                            else (Chosen Followed (Just (Tried (Outscored weak (layoutScore found)) (layoutJump found))), ByFollowing)
                        )
        , testProperty "weak following is kept when the layout comes to nothing, and says why it did if it was tried" $
            forAllShrink genHypotheses shrinkHypotheses $ \h -> forAll genReason $ \whyNot -> forAll genWeak $ \weak ->
                let kept = h{followed = Right (weak, ByFollowing), stillWhereItWas = unwanted}
                 in decide kept{byLayout = Nothing} === Right (Chosen Followed Nothing, ByFollowing)
                        .&&. decide kept{byLayout = Just (Left whyNot)} === Right (Chosen Followed (Just (Tried (Failed (Why Afresh whyNot)) [])), ByFollowing)
        , testProperty "with following rejected, a layout that continues the last pose is taken, and says why following wasn't" $
            forAllShrink genHypotheses shrinkHypotheses $ \h -> forAllShrink genLayout shrinkLayout $ \found -> forAll genWhy $ \why ->
                let refound = found{layoutWay = Refound}
                 in decide h{followed = Left why, byLayout = Just (Right refound), stillWhereItWas = unwanted}
                        === Right (Chosen Refound (Just (Tried (Failed why) (layoutJump refound))), ByLayout)
        , testProperty "with following rejected, a layout elsewhere is taken just when likelier than the clip still where it was; held otherwise, for following's reason" $
            forAllShrink genHypotheses shrinkHypotheses $ \h -> forAllShrink genLayout shrinkLayout $ \found -> forAll genWhy $ \why -> forAll (elements [Resumed, FoundAfresh]) $ \way ->
                let elsewhere = found{layoutWay = way}
                    (missing, held) = stillWhereItWas h
                 in decide h{followed = Left why, byLayout = Just (Right elsewhere)}
                        === if layoutScore elsewhere > held
                            then Right (Chosen way (Just (Tried (Failed why) (layoutJump elsewhere))), ByLayout)
                            else Left (why, Just (Held (layoutScore elsewhere) missing held))
        , testProperty "with following rejected and no clip by layout, the frame is lost for following's reason, nothing held" $
            forAllShrink genHypotheses shrinkHypotheses $ \h -> forAll genReason $ \whyNot -> forAll genWhy $ \why ->
                conjoin
                    [ decide h{followed = Left why, byLayout = layout, stillWhereItWas = unwanted} === Left (why, Nothing)
                    | layout <- [Nothing, Just (Left whyNot)]
                    ]
        , testProperty "a layout taken would be taken scoring better, and following kept would be kept scoring better" $
            forAllShrink genHypotheses shrinkHypotheses $ \h -> forAll (choose (0, 30)) $ \more ->
                let by = fmap snd . decide
                    better (Score s) = Score (s + more)
                    likelierLayout = h{byLayout = fmap (fmap (\found -> found{layoutScore = better (layoutScore found)})) (byLayout h)}
                    likelierFollowing = h{followed = fmap (\(s, made) -> (better s, made)) (followed h)}
                 in counterexample (show (by h, by likelierLayout, by likelierFollowing)) $
                        (by h /= Right ByLayout || by likelierLayout == Right ByLayout)
                            && (by h /= Right ByFollowing || by likelierFollowing == Right ByFollowing)
        ]

-- | The choice among hypotheses that say only which they are.
decide :: Hypotheses Made -> Either (Why, Maybe Held) (Chosen, Made)
decide = followOrFind

-- | What a hypothesis made of the frame: which hypothesis it was.
data Made = ByFollowing | ByLayout
    deriving (Eq, Show)

-- | A hypothesis that mustn't be looked at.
unwanted :: a
unwanted = error "a hypothesis was worked out that wasn't needed"

-- | Scores on both sides of every limit the choice goes by, and on them:
-- nothing against, the score below which following is weak, and so many
-- reflectors out of sight.
genScore :: Gen Score
genScore = Score <$> oneof [elements limits, (+) <$> elements limits <*> choose (-1, 1), choose (-80, 0)]
  where
    limits = [0, nats weakScore, nats unseenScore, 2 * nats unseenScore, 3 * nats unseenScore]

-- | A score below which following is weak.
genWeak :: Gen Score
genWeak = genScore `suchThat` (< weakScore)

genWhy :: Gen Why
genWhy = Why Following <$> genReason

genReason :: Gen Reason
genReason = elements [NoPoseFits, NothingMatches, Unfit [Finding (Name "turn" "the head would have turned faster than heads turn") impossible Nothing]]

genLayout :: Gen (Layout Made)
genLayout = do
    scored <- genScore
    way <- elements [Refound, Resumed, FoundAfresh]
    jump <- elements [[], [Finding (Name "turn" "turned") possible Nothing], [Finding (Name "move" "moved") impossible Nothing]]
    pure (Layout scored way jump ByLayout)

-- | A layout with nothing against it, and with nothing said of its jump.
shrinkLayout :: Layout Made -> [Layout Made]
shrinkLayout found = [found{layoutScore = possible} | layoutScore found /= possible] ++ [found{layoutJump = []} | not (null (layoutJump found))]

genHypotheses :: Gen (Hypotheses Made)
genHypotheses = do
    following <- oneof [Left <$> genWhy, (\s -> Right (s, ByFollowing)) <$> genScore]
    layout <- oneof [pure Nothing, Just . Left <$> genReason, Just . Right <$> genLayout]
    missing <- chooseInt (0, 3)
    held <- elements [Score (fromIntegral missing * nats unseenScore), impossible]
    pure (Hypotheses following layout (missing, held))

-- | Hypotheses with no layout, and with a simpler one.
shrinkHypotheses :: Hypotheses Made -> [Hypotheses Made]
shrinkHypotheses h = case byLayout h of
    Nothing -> []
    Just (Left _) -> [h{byLayout = Nothing}]
    Just (Right found) -> h{byLayout = Nothing} : [h{byLayout = Just (Right found')} | found' <- shrinkLayout found]
