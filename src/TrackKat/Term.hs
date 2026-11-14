-- | Judging a hypothesis by named terms in one currency, so that whatever
-- accepts or rejects it can say which term did, and by how much.
--
-- See Note [Terms].
module TrackKat.Term (
    -- * Scores
    Score (..),
    possible,
    impossible,
    isImpossible,

    -- * Terms
    Name (..),
    Term,
    about,

    -- ** Hard gates
    Bound (..),
    Measure (..),
    gate,
    atMost,
    atLeast,
    within,

    -- ** Soft terms
    gaussian,
    cost,
    likelihood,

    -- * Judging
    score,
    accepts,
    Finding (..),
    findings,
    explain,
    rejections,
    Verdict (..),
    verdict,
    rejectedBy,

    -- * Counting verdicts
    Tally,
    noTally,
    tally,
    tallyCounts,
    tallyRejected,
    tallyLast,
    renderTally,

    -- * Saying
    renderMeasure,
    renderFinding,
    renderFindings,
) where

import Data.List (intercalate)
import Numeric (showFFloat)

-- Note [Terms]
-- ~~~~~~~~~~~~
-- Tracking decides by many small judgements, and a wrong decision used to
-- say nothing of which one made it: a limit on how far a head moves would
-- veto three clear reflectors, and they showed as strays. A 'Term' is one
-- judgement of a hypothesis (a pose, a way of matching blobs, a blob that
-- may be a light) with a name, and terms add up, so a decision is a sum of
-- named terms that can be asked what each said ('explain') and whether
-- they accept the hypothesis, or which rejected it ('verdict').
--
-- The currency is the log of a likelihood, in nats: 0 for nothing against
-- the hypothesis, less for less likely, minus infinity for impossible. A
-- hard gate ('atMost', 'atLeast', 'within', 'gate') says 0 or minus
-- infinity. A soft term says how unlikely: 'gaussian' for something with
-- a spread, 'cost' for a penalty already reckoned in nats, 'likelihood'
-- for any log-likelihood. Sums of scores are then log-likelihoods too, and
-- differences of them log-odds, which is what TrackKat.Branch has always
-- weighed its evidence in.
--
-- The embedding is shallow: a term is the function that scores, and the
-- function that tells its findings, and '<>' composes each. Scoring is
-- the fast path, used on every frame: it allocates nothing of its own,
-- and stops at the first impossible term, as the chains of '&&' it stands
-- for did. Explaining is a separate, slower path that runs every term,
-- for when someone asks or something is rejected.
--
-- Terms are built where the limits are known, often inside a function
-- from what it was given (the reference pose, the time since the last
-- frame), and 'about' points a term for a part at the whole.
--
-- A term's name has two forms: a word or two for the developer reading
-- tirprobe's output beside a number and its limit ("aside 512 mm > 400
-- mm"), and a plain phrase for the user, who is told only what is wrong
-- ("too far to the side of where you recentered").
--
-- What is not here yet: hypotheses are still accepted or rejected one at
-- a time, by gates. Choosing among several by their scores, with gates
-- softened into likelihoods, is what the terms are for next (see
-- docs/heuristics.md).

-- | The log of a likelihood, in nats: at most 0 from a gate, and minus
-- infinity for what can't be.
newtype Score = Score {nats :: Double}
    deriving (Eq, Ord)

instance Show Score where
    showsPrec d (Score x) = showsPrec d x

-- | Nothing against it.
possible :: Score
possible = Score 0

impossible :: Score
impossible = Score never

isImpossible :: Score -> Bool
isImpossible (Score x) = x == never

never :: Double
never = -1 / 0

-- | What a term is called: briefly, as beside a figure in a developer's
-- output, and plainly, as what is wrong when it rejects, in a user's
-- words.
data Name = Name
    { brief :: String
    , plain :: String
    }
    deriving (Eq, Show)

-- | A judgement of a hypothesis, or the sum of several.
data Term h = Term
    { termScore :: !(h -> Double)
    , termFindings :: !(h -> [Finding] -> [Finding])
    }

-- | Scores add. Scoring stops at an impossible term.
instance Semigroup (Term h) where
    Term s f <> Term s' f' =
        Term
            (\h -> let a = s h in if a == never then a else a + s' h)
            (\h -> f h . f' h)
    {-# INLINE (<>) #-}

instance Monoid (Term h) where
    mempty = Term (const 0) (const id)
    {-# INLINE mempty #-}

-- | A term for a part, as a term for the whole it is part of.
about :: (a -> h) -> Term h -> Term a
about part (Term s f) = Term (s . part) (f . part)
{-# INLINE about #-}

-- | Which side of its limit a figure must be, or that the limit is the
-- spread of something soft.
data Bound = AtMost | AtLeast | Spread
    deriving (Eq, Show)

-- | A figure a term judged, against its limit or spread, in a unit.
data Measure = Measure
    { measureValue :: !Double
    , measureBound :: !Bound
    , measureLimit :: !Double
    , measureUnit :: String
    }
    deriving (Eq, Show)

-- | What one term said of a hypothesis.
data Finding = Finding
    { findingName :: Name
    , findingScore :: !Score
    , findingMeasure :: !(Maybe Measure)
    }
    deriving (Eq, Show)

-- | A hard gate: possible where the test holds, impossible elsewhere; its
-- finding gives the figure tested.
gate :: Name -> (h -> Bool) -> (h -> Measure) -> Term h
gate name test measure =
    Term
        (\h -> if test h then 0 else never)
        (\h -> (Finding name (if test h then possible else impossible) (Just (measure h)) :))
{-# INLINE gate #-}

-- | A figure, in the given unit, that can't be above a limit.
atMost :: Name -> String -> Double -> (h -> Double) -> Term h
atMost name u most f = gate name (\h -> f h <= most) (\h -> Measure (f h) AtMost most u)
{-# INLINE atMost #-}

-- | A figure that can't be below a limit.
atLeast :: Name -> String -> Double -> (h -> Double) -> Term h
atLeast name u least f = gate name (\h -> f h >= least) (\h -> Measure (f h) AtLeast least u)
{-# INLINE atLeast #-}

-- | A figure that can't be outside a range, named for being below it and
-- for being above it.
within :: (Name, Name) -> String -> (Double, Double) -> (h -> Double) -> Term h
within (below, above) u (least, most) f = atLeast below u least f <> atMost above u most f
{-# INLINE within #-}

-- | A figure spread about zero as a gaussian of the given standard
-- deviation, less the constant that makes the peak 0.
gaussian :: Name -> String -> Double -> (h -> Double) -> Term h
gaussian name u sigma f =
    Term
        (\h -> fall (f h))
        (\h -> (Finding name (Score (fall (f h))) (Just (Measure (f h) Spread sigma u)) :))
  where
    fall x = negate ((x / sigma) ^ (2 :: Int)) / 2
{-# INLINE gaussian #-}

-- | A penalty in nats, taken off.
cost :: Name -> (h -> Double) -> Term h
cost name f = likelihood name (negate . f)
{-# INLINE cost #-}

-- | A log-likelihood, however it is worked out.
likelihood :: Name -> (h -> Double) -> Term h
likelihood name f = Term f (\h -> (Finding name (Score (f h)) Nothing :))
{-# INLINE likelihood #-}

-- | What the terms come to for a hypothesis. This is the fast path.
score :: Term h -> h -> Score
score t h = Score (termScore t h)
{-# INLINE score #-}

-- | Whether nothing makes the hypothesis impossible. This is the fast
-- path.
accepts :: Term h -> h -> Bool
accepts t h = termScore t h /= never
{-# INLINE accepts #-}

-- | What each term said, in the order they were added.
findings :: Term h -> h -> [Finding]
findings t h = termFindings t h []

-- | What each term scored, by its brief name.
explain :: Term h -> h -> [(String, Score)]
explain t h = [(brief (findingName f), findingScore f) | f <- findings t h]

-- | Whether a hypothesis is accepted, with what its terms come to, or
-- rejected, with what the terms that made it impossible found.
data Verdict = Accepted Score | Rejected [Finding]
    deriving (Eq, Show)

verdict :: Term h -> h -> Verdict
verdict t h = case [f | f <- found, isImpossible (findingScore f)] of
    [] -> Accepted (Score (sum (map (nats . findingScore) found)))
    failed -> Rejected failed
  where
    found = findings t h

-- | What the terms that make a hypothesis impossible found; none if it is
-- accepted.
rejections :: Term h -> h -> [Finding]
rejections t h = [f | f <- findings t h, isImpossible (findingScore f)]

-- | The brief names of the terms that rejected.
rejectedBy :: Verdict -> [String]
rejectedBy (Accepted _) = []
rejectedBy (Rejected failed) = map (brief . findingName) failed

-- | How many hypotheses were accepted, and how many rejected by each
-- term, by its brief name; and what the last one rejected was rejected
-- for. For decisions too many to tell one by one, like windows of turning
-- twice a second. All evaluated, so that keeping one holds on to nothing.
data Tally = Tally ![(String, Int)] ![Finding]
    deriving (Eq, Show)

noTally :: Tally
noTally = Tally [] []

-- | What accepted hypotheses are counted under.
taken :: String
taken = "taken"

-- | Count a verdict: an acceptance, or a rejection under the first term
-- that rejected.
tally :: Verdict -> Tally -> Tally
tally v (Tally counts lastRejected) = case v of
    Accepted _ -> Tally (counted taken) lastRejected
    Rejected failed ->
        let evaluated = foldr (\f rest -> whole f `seq` rest) failed failed
         in Tally (counted (concatMap (brief . findingName) (take 1 failed))) evaluated
  where
    whole (Finding name _ m) = length (brief name) `seq` maybe 0 (length . measureUnit) m
    counted name =
        let bumped = case lookup name counts of
                Nothing -> counts ++ [(name, 1)]
                Just _ -> [(n, if n == name then c + 1 else c) | (n, c) <- counts]
         in foldr (\(n, c) rest -> length n `seq` c `seq` rest) bumped bumped

-- | The counts: those accepted first, as "taken", then each term's
-- rejections, in the order each first rejected.
tallyCounts :: Tally -> [(String, Int)]
tallyCounts (Tally counts _) = [(n, c) | (n, c) <- counts, n == taken] ++ [(n, c) | (n, c) <- counts, n /= taken]

-- | How many were rejected.
tallyRejected :: Tally -> Int
tallyRejected (Tally counts _) = sum [c | (n, c) <- counts, n /= taken]

-- | What the last hypothesis rejected was rejected for; none if none was.
tallyLast :: Tally -> [Finding]
tallyLast (Tally _ lastRejected) = lastRejected

-- | The counts on a line, like "12 taken, 30 turn, 2 fit"; "none" if
-- nothing was counted.
renderTally :: Tally -> String
renderTally t = case tallyCounts t of
    [] -> "none"
    counts -> intercalate ", " [show c ++ " " ++ n | (n, c) <- counts]

-- | A figure against its limit, like "512 mm > 400 mm", or against its
-- spread, like "30 degrees (spread 40)". Figures are given to four
-- significant digits, so that one just past its limit shows as past it.
renderMeasure :: Measure -> String
renderMeasure (Measure x b l u) = case b of
    AtMost -> with x ++ (if x <= l then " <= " else " > ") ++ with l
    AtLeast -> with x ++ (if x >= l then " >= " else " < ") ++ with l
    Spread -> with x ++ " (spread " ++ figure l ++ ")"
  where
    with v = if null u then figure v else figure v ++ " " ++ u

-- | A number to four significant digits, without trailing zeros.
figure :: Double -> String
figure v
    | isInfinite v = if v > 0 then "no limit" else "nothing"
    | isNaN v = "not a number"
    | v == 0 = "0"
    | otherwise = trim (showFFloat (Just decimals) v "")
  where
    decimals = max 0 (min 6 (3 - floor (logBase 10 (abs v)))) :: Int
    trim str
        | '.' `elem` str = case dropWhile (== '0') (reverse str) of
            '.' : whole -> reverse whole
            digits -> reverse digits
        | otherwise = str

-- | A finding for a developer: its brief name, its figure and limit if it
-- has them, and, of a soft term, its score.
renderFinding :: Finding -> String
renderFinding (Finding name s m) = unwords ([brief name] ++ maybe [] (pure . renderMeasure) m ++ [worth | soft])
  where
    soft = maybe True ((== Spread) . measureBound) m
    worth
        | isImpossible s = "(impossible)"
        | otherwise = "(" ++ figure (nats s) ++ " nats)"

renderFindings :: [Finding] -> String
renderFindings = intercalate ", " . map renderFinding
