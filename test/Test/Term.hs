-- | What tracking judges by, and says of it.
module Test.Term (tests) where

import Test.Tasty (TestTree)

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
