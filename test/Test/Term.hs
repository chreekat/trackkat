-- | What tracking judges by, and says of it.
module Test.Term (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Test.Check
import TrackKat.Term (Bound (..), Finding (..), Measure (..), Name (..), Score (..), Term, Verdict (..))
import qualified TrackKat.Term as Term

tests :: TestTree
tests = group "Term" $ do
    -- Terms: gates and soft terms add up, and say what each found.
    let small = Term.atMost (Name "small" "too big") "mm" 10 fst :: Term (Double, Double)
        positive = Term.atLeast (Name "positive" "negative") "mm" 0 fst
        likely = Term.gaussian (Name "likely" "unlikely") "degrees" 2 snd
        dear = Term.cost (Name "dear" "costly") snd
        both = small <> positive <> likely <> dear
    let explained h = [(brief name, s) | (name, s) <- Term.explain both h]
    checkShowing "terms add up" (show (explained (4, 3))) $
        explained (4, 3) == [("small", Score 0), ("positive", Score 0), ("likely", Score (-1.125)), ("dear", Score (-3))]
            && Term.score both (4, 3) == Score (-4.125)
            && Term.accepts both (4, 3)
            && Term.verdict both (4, 3) == Accepted (Score (-4.125))
    checkShowing "a gate rejects, by name" (show (Term.verdict both (12, 3))) $
        not (Term.accepts both (12, 3))
            && Term.isImpossible (Term.score both (12, 3))
            && Term.rejectedBy (Term.verdict both (12, 3)) == ["small"]
            && map findingMeasure (Term.rejections both (12, 3)) == [Just (Measure 12 AtMost 10 "mm")]
    check "each gate that rejects is told" $ Term.rejectedBy (Term.verdict (small <> positive <> Term.within (Name "low" "low", Name "high" "high") "" (0, 1) snd) (12, 3)) == ["small", "high"]
    check "no terms accept anything" $ Term.verdict (mempty :: Term ()) () == Accepted Term.possible
    check "a term for a part judges the whole" $ Term.rejectedBy (Term.verdict (Term.about (\x -> (x, 0)) small) (11 :: Double)) == ["small"]
    checkShowing "findings read as figures against limits" (Term.renderFindings (Term.findings both (400.3, 3))) $
        Term.renderFindings (Term.findings both (400.3, 3)) == "small 400.3 mm > 10 mm, positive 400.3 mm >= 0 mm, likely 3 degrees (spread 2) (-1.125 nats), dear (-3 nats)"
    let counted = foldr Term.tally Term.noTally [Term.verdict both h | h <- [(4, 3), (12, 3), (-1, 0), (11, 0), (5, 5)]]
    checkShowing "verdicts are counted by what rejected" (Term.renderTally counted) $
        Term.tallyCounts counted == [("taken", 2), ("small", 2), ("positive", 1)]
            && Term.tallyRejected counted == 3
            && map (brief . findingName) (Term.tallyLast counted) == ["small"]
    tree properties

-- Note [What terms promise]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
-- A term is known by what it scores a hypothesis and what it finds of it,
-- so two terms are the same when those are. Findings are lists, and what is
-- said of them holds exactly. Scores are sums of floating-point numbers, in
-- an order that follows how the terms were put together, so what is said of
-- them holds to within the rounding of a sum ('sameScore'), and exactly of
-- which hypotheses are impossible.
properties :: TestTree
properties =
    testGroup
        "properties"
        [ testProperty "no terms change nothing, before or after" $
            forAllShrink genSpecs shrinkSpecs $ \specs h ->
                let t = built specs
                 in said (mempty <> t) h === said t h .&&. said (t <> mempty) h === said t h
        , testProperty "terms add up the same however they are grouped" $
            forAllShrink genSpecs shrinkSpecs $ \a -> forAllShrink genSpecs shrinkSpecs $ \b -> forAllShrink genSpecs shrinkSpecs $ \c h ->
                let (x, y, z) = (built a, built b, built c)
                 in Term.findings ((x <> y) <> z) h === Term.findings (x <> (y <> z)) h
                        .&&. sameScore (Term.findings (x <> y <> z) h) (Term.score ((x <> y) <> z) h) (Term.score (x <> (y <> z)) h)
        , testProperty "a score is the sum of what each term scored" $
            forAllShrink genSpecs shrinkSpecs $ \specs h ->
                let t = built specs
                 in sameScore (Term.findings t h) (Term.score t h) (Score (sum (map (nats . snd) (Term.explain t h))))
        , testProperty "terms together accept just what each of them accepts" $
            forAllShrink genSpecs shrinkSpecs $ \specs h ->
                Term.accepts (built specs) h === all (\s -> Term.accepts (built [s]) h) specs
        , testProperty "one impossible term makes the sum impossible, wherever it is" $
            forAllShrink genSpecs shrinkSpecs $ \before -> forAllShrink genSpecs shrinkSpecs $ \after h ->
                let never = Term.gate (Name "never" "never") (const False) (const (Measure 0 AtMost 0 "")) :: Term Figures
                 in Term.score (built before <> never <> built after) h === Term.impossible
        , testProperty "accepting, an accepting verdict, no rejections and a possible score go together" $
            forAllShrink genSpecs shrinkSpecs $ \specs h ->
                let t = built specs
                    accepted = Term.accepts t h
                 in conjoin
                        [ null (Term.rejections t h) === accepted
                        , not (Term.isImpossible (Term.score t h)) === accepted
                        , case Term.verdict t h of
                            Accepted s -> property accepted .&&. sameScore (Term.findings t h) s (Term.score t h)
                            Rejected failed -> property (not accepted) .&&. failed === Term.rejections t h
                        ]
        , testProperty "what rejects is among what is found, in order, and named by the verdict" $
            forAllShrink genSpecs shrinkSpecs $ \specs h ->
                let t = built specs
                 in Term.rejections t h === filter (Term.isImpossible . findingScore) (Term.findings t h)
                        .&&. Term.rejectedBy (Term.verdict t h) === map (brief . findingName) (Term.rejections t h)
        , testProperty "each term finds one thing, in the order they were added" $
            forAllShrink genSpecs shrinkSpecs $ \specs h ->
                map (brief . findingName) (Term.findings (built specs) h) === map specName specs
        , testProperty "a term about a part judges the whole as it judges the part" $
            forAllShrink genSpecs shrinkSpecs $ \specs -> forAllShrink genPart shrinkPart $ \part h ->
                said (Term.about (taking part) (built specs)) h === said (built specs) (taking part h)
        , testProperty "a term about a part of a part is about the part's part" $
            forAllShrink genSpecs shrinkSpecs $ \specs -> forAllShrink genPart shrinkPart $ \f -> forAllShrink genPart shrinkPart $ \g h ->
                said (Term.about (taking f) (Term.about (taking g) (built specs))) h === said (Term.about (taking g . taking f) (built specs)) h
        , testProperty "verdicts are counted once each, acceptances first" $
            forAllShrink genSpecs shrinkSpecs $ \specs hs ->
                let verdicts = map (Term.verdict (built specs)) hs
                    counted = foldr Term.tally Term.noTally verdicts
                    rejected = [failed | Rejected failed <- verdicts]
                 in conjoin
                        [ sum (map snd (Term.tallyCounts counted)) === length verdicts
                        , Term.tallyRejected counted === length rejected
                        , lookup "taken" (Term.tallyCounts counted) === (if length rejected == length verdicts then Nothing else Just (length verdicts - length rejected))
                        , Term.tallyLast counted === concat (take 1 rejected)
                        ]
        ]

-- | What a term says of a hypothesis.
said :: Term h -> h -> (Score, [Finding])
said t h = (Term.score t h, Term.findings t h)

-- | Whether two scores are the same sum of the given findings' scores,
-- added in different orders: both impossible, or apart by no more than
-- rounding could make them, which is the unit roundoff times the sum of
-- the scores' sizes for each addition.
sameScore :: [Finding] -> Score -> Score -> Property
sameScore found a b =
    counterexample (show a ++ " and " ++ show b ++ " differ by more than rounding, " ++ show slack) $
        (Term.isImpossible a && Term.isImpossible b) || abs (nats a - nats b) <= slack
  where
    sizes = map (abs . nats . findingScore) found
    slack = fromIntegral (length sizes) * 2 ** (-53) * sum sizes

-- | Three figures to judge.
type Figures = (Double, Double, Double)

-- | Which of the figures a term looks at.
data Which = First | Second | Third
    deriving (Bounded, Enum, Eq, Show)

figure :: Which -> Figures -> Double
figure First (x, _, _) = x
figure Second (_, y, _) = y
figure Third (_, _, z) = z

-- | A term over the figures, as what to make it from: a gate on a figure,
-- with its limit; a gaussian of one, with its spread; or a cost of so many
-- nats for each unit of one.
data Spec
    = MostSpec Which Double
    | LeastSpec Which Double
    | GaussianSpec Which (Positive Double)
    | CostSpec Which (NonNegative Double)
    deriving (Show)

specName :: Spec -> String
specName MostSpec{} = "most"
specName LeastSpec{} = "least"
specName GaussianSpec{} = "likely"
specName CostSpec{} = "dear"

built :: [Spec] -> Term Figures
built = foldMap one
  where
    one spec = case spec of
        MostSpec w limit -> Term.atMost (named spec) "mm" limit (figure w)
        LeastSpec w limit -> Term.atLeast (named spec) "mm" limit (figure w)
        GaussianSpec w (Positive sigma) -> Term.gaussian (named spec) "mm" sigma (figure w)
        CostSpec w (NonNegative each) -> Term.cost (named spec) ((each *) . abs . figure w)
    named spec = Name (specName spec) ("it is " ++ specName spec)

genSpecs :: Gen [Spec]
genSpecs = listOf (oneof [MostSpec <$> which <*> arbitrary, LeastSpec <$> which <*> arbitrary, GaussianSpec <$> which <*> arbitrary, CostSpec <$> which <*> arbitrary])
  where
    which = arbitraryBoundedEnum

-- | Fewer terms, about earlier figures, with simpler limits.
shrinkSpecs :: [Spec] -> [[Spec]]
shrinkSpecs = shrinkList one
  where
    one (MostSpec w limit) = [MostSpec w' limit' | (w', limit') <- shrinkPair (w, limit)]
    one (LeastSpec w limit) = [LeastSpec w' limit' | (w', limit') <- shrinkPair (w, limit)]
    one (GaussianSpec w sigma) = [GaussianSpec w' sigma' | (w', sigma') <- shrinkPair (w, sigma)]
    one (CostSpec w each) = [CostSpec w' each' | (w', each') <- shrinkPair (w, each)]
    shrinkPair (w, x) = [(w', x) | w' <- shrinkBoundedEnum w] ++ [(w, x') | x' <- shrink x]

-- | A way of taking figures from other figures, as what to make it from:
-- them turned about, or all of them scaled.
data Part = TurnedAbout | Scaled Double
    deriving (Show)

taking :: Part -> Figures -> Figures
taking TurnedAbout (x, y, z) = (y, z, x)
taking (Scaled k) (x, y, z) = (k * x, k * y, k * z)

genPart :: Gen Part
genPart = oneof [pure TurnedAbout, Scaled <$> arbitrary]

shrinkPart :: Part -> [Part]
shrinkPart TurnedAbout = []
shrinkPart (Scaled k) = TurnedAbout : map Scaled (shrink k)
