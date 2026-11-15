-- | How likely the tracked pose and the pose recentered on each are the
-- head's or its mirror image: the belief "TrackKat.Branch" keeps, and what
-- happens to it. See Note [Which pose is the head's] there.
module TrackKat.Belief (
    Chance,
    chance,
    inChance,
    Belief (..),
    unknown,
    trackedMirrorChance,
    referenceMirrorChance,
    leastChance,
    normalise,
    crossed,
    evidence,
    weighed,
    agreed,
    swapTracked,
    swapReference,
) where

import TrackKat.Term (LogOdds (..), Score (..))

-- | How likely something is: from 0, for what can't be, to 1, for what is
-- certain. One made by 'chance' is in that range; one read off a belief
-- ('trackedMirrorChance', 'referenceMirrorChance') is as far as the
-- belief's probabilities are, so to within rounding.
newtype Chance = Chance Double
    deriving (Eq, Ord)

instance Show Chance where
    showsPrec d (Chance p) = showsPrec d p

-- | The chance a number says, taking one below 0 for 0 and one above 1
-- for 1.
chance :: Double -> Chance
chance p = Chance (max 0 (min 1 p))

-- | A chance as a bare number, to compute with.
inChance :: Chance -> Double
inChance (Chance p) = p

-- | Probabilities of the four combinations of the tracked pose and the
-- reference being the head's or the mirror image.
data Belief = Belief
    { bothTrue :: !Double
    , referenceMirrored :: !Double
    , trackedMirrored :: !Double
    , bothMirrored :: !Double
    }
    deriving (Eq, Show)

-- | Nothing known: each combination as likely as the others.
unknown :: Belief
unknown = Belief 0.25 0.25 0.25 0.25

-- | How likely the tracked pose is the mirror image.
trackedMirrorChance :: Belief -> Chance
trackedMirrorChance b = Chance (trackedMirrored b + bothMirrored b)

-- | How likely the reference is the mirror image.
referenceMirrorChance :: Belief -> Chance
referenceMirrorChance b = Chance (referenceMirrored b + bothMirrored b)

-- | The least any combination's probability gets, so that a long run of
-- evidence one way doesn't make the other unrecoverable.
leastChance :: Chance
leastChance = Chance 1e-3

-- | Each combination raised to 'leastChance' if it is below it, and all
-- scaled to add up to one.
normalise :: Belief -> Belief
normalise b = Belief (f bothTrue) (f referenceMirrored) (f trackedMirrored) (f bothMirrored)
  where
    floored = Belief (g bothTrue) (g referenceMirrored) (g trackedMirrored) (g bothMirrored)
    g sel = max (inChance leastChance) (sel b)
    total = bothTrue floored + referenceMirrored floored + trackedMirrored floored + bothMirrored floored
    f sel = sel floored / total

-- | The tracked pose's truth becomes, with the given chance, the opposite
-- of what it was.
crossed :: Chance -> Belief -> Belief
crossed (Chance crossing) (Belief tt tm mt mm) =
    Belief (stay tt mt) (stay tm mm) (stay mt tt) (stay mm tm)
  where
    stay same other = (1 - crossing) * same + crossing * other

-- | The belief given something so much likelier if the tracked pose is
-- the mirror image than if it is the head's: the odds for the mirror
-- image multiplied by e to that power.
evidence :: LogOdds -> Belief -> Belief
evidence (LogOdds logOdds) (Belief tt tm mt mm) = normalise (Belief tt tm (e * mt) (e * mm))
  where
    e = exp logOdds

-- | The belief given something that scores the first if the tracked pose
-- and the reference are alike, both the head's or both the mirror image,
-- and the second if they aren't. Not of something impossible either way,
-- which no belief allows for.
weighed :: Score -> Score -> Belief -> Belief
weighed (Score ifAlike) (Score ifUnalike) b =
    Belief (alike * bothTrue b / total) (unalike * referenceMirrored b / total) (unalike * trackedMirrored b / total) (alike * bothMirrored b / total)
  where
    alike = exp ifAlike
    unalike = exp ifUnalike
    total = alike * (bothTrue b + bothMirrored b) + unalike * (referenceMirrored b + trackedMirrored b)

-- | The tracked pose became the reference: they are the head's, or the
-- mirror image, together.
agreed :: Belief -> Belief
agreed (Belief tt tm mt mm) = Belief (tt + tm) 0 0 (mt + mm)

-- | The tracked pose was switched for its mirror image.
swapTracked :: Belief -> Belief
swapTracked (Belief tt tm mt mm) = Belief mt mm tt tm

-- | The reference was switched for its mirror image.
swapReference :: Belief -> Belief
swapReference (Belief tt tm mt mm) = Belief tm tt mm mt
