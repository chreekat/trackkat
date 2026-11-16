-- | What the old tracker takes of the world that the tracker no longer
-- does. See Note [The old tracker] in "TrackKat.Old.Tracker".
module TrackKat.Old.World (
    headTurnRate,
    headMoveRate,
) where

import TrackKat.World (DegreesPerSecond, MillimetresPerSecond, World, perSecond)

-- | How much more the most a pose followed from one frame to the next
-- turns, and its pivot moves ('TrackKat.World.headStepTurn' and
-- 'TrackKat.World.headStepMove'), for each second between two sightings
-- of the clip.
headTurnRate :: World -> DegreesPerSecond
headTurnRate _ = perSecond 100

headMoveRate :: World -> MillimetresPerSecond
headMoveRate _ = perSecond 200
