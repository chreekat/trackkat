-- | How likely the tracked pose and the pose recentered on each are the
-- head's or its mirror image: the belief "TrackKat.Branch" keeps, and what
-- happens to it. See Note [Which pose is the head's] there.
module TrackKat.Belief (
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
trackedMirrorChance :: Belief -> Double
trackedMirrorChance b = trackedMirrored b + bothMirrored b

-- | How likely the reference is the mirror image.
referenceMirrorChance :: Belief -> Double
referenceMirrorChance b = referenceMirrored b + bothMirrored b

-- | The least any combination's probability gets, so that a long run of
-- evidence one way doesn't make the other unrecoverable.
leastChance :: Double
leastChance = 1e-3

-- | Each combination raised to 'leastChance' if it is below it, and all
-- scaled to add up to one.
normalise :: Belief -> Belief
normalise b = Belief (f bothTrue) (f referenceMirrored) (f trackedMirrored) (f bothMirrored)
  where
    floored = Belief (g bothTrue) (g referenceMirrored) (g trackedMirrored) (g bothMirrored)
    g sel = max leastChance (sel b)
    total = bothTrue floored + referenceMirrored floored + trackedMirrored floored + bothMirrored floored
    f sel = sel floored / total

-- | The tracked pose's truth becomes, with the given chance, the opposite
-- of what it was.
crossed :: Double -> Belief -> Belief
crossed chance (Belief tt tm mt mm) =
    Belief (stay tt mt) (stay tm mm) (stay mt tt) (stay mm tm)
  where
    stay same other = (1 - chance) * same + chance * other

-- | The odds for the tracked pose being the mirror image multiplied by
-- e to the given power.
evidence :: Double -> Belief -> Belief
evidence logOdds (Belief tt tm mt mm) = normalise (Belief tt tm (e * mt) (e * mm))
  where
    e = exp logOdds

-- | The belief given something as likely as the first number if the
-- tracked pose and the reference are alike, both the head's or both the
-- mirror image, and as the second if they aren't.
weighed :: Double -> Double -> Belief -> Belief
weighed alike unalike b =
    Belief (alike * bothTrue b / total) (unalike * referenceMirrored b / total) (unalike * trackedMirrored b / total) (alike * bothMirrored b / total)
  where
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
