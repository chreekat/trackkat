-- | The poses a frame can be followed from: the last tracked, and one
-- left a moment ago for a pose found afresh; for how long they predict,
-- and what a tracked frame carries on from them.
module TrackKat.Tracker.Trail (
    Recent (..),
    LeftBehind (..),
    Trail (..),
    noTrail,
    predictionSeconds,
    stillPredicting,
    Carried (..),
    carryOn,
) where

import TrackKat.Branch (Branch, foundAfresh)
import TrackKat.Pose (Pose)
import TrackKat.Tracker.Outcome (Way (..))

-- | The last pose tracked.
data Recent = Recent
    { recentPose :: !Pose
    , recentTime :: !Double
    -- ^ in seconds on the tracker's clock
    , recentAllSeen :: !Double
    -- ^ when all three reflectors last showed
    , recentSince :: !Double
    -- ^ since when the clip has been tracked in every frame
    }

-- | A pose that was left for one found afresh elsewhere, and what was
-- believed of it being the head's or the mirror image.
data LeftBehind = LeftBehind !Recent !Branch

-- | The poses a frame can be followed from.
data Trail = Trail
    { trailRecent :: !(Maybe Recent)
    -- ^ the last pose tracked
    , trailLeft :: !(Maybe LeftBehind)
    -- ^ a pose left for one found afresh (see Note [Following or finding
    -- afresh] in "TrackKat.Tracker.Choose")
    }

-- | Nothing tracked yet.
noTrail :: Trail
noTrail = Trail Nothing Nothing

-- | For how long, in seconds, a pose predicts where the reflectors are. After
-- longer out of sight, the clip is found only afresh, with nothing said of
-- where it was.
predictionSeconds :: Double
predictionSeconds = 0.5

-- | Of a trail, the poses that still predict where the clip is at the
-- given time.
stillPredicting :: Double -> Trail -> Trail
stillPredicting now (Trail recent left) = Trail (recent >>= kept id) (left >>= kept (\(LeftBehind r _) -> r))
  where
    kept pose a
        | now - recentTime (pose a) <= predictionSeconds = Just a
        | otherwise = Nothing

-- | What a tracked frame carries on of what goes by continuity.
data Carried = Carried
    { carriedBelief :: Branch
    -- ^ what is believed of its pose being the head's, before the frame
    -- is weighed
    , carriedSince :: Double
    -- ^ since when the clip has been tracked in every frame
    , carriedLeft :: Maybe LeftBehind
    -- ^ the pose left behind that may yet be resumed
    }

-- | What a frame tracked in the given way, at the given time, carries on
-- from the poses still predicting then, given what is believed of the
-- last pose. A pose that continues the last carries on what goes by
-- continuity; one resumed takes up what the pose left behind had; one
-- found afresh starts it over (see Note [Following or finding afresh] in
-- "TrackKat.Tracker.Choose").
--
-- A pose still predicting that is left for one found afresh is kept while
-- it predicts, unless one left before it still does.
carryOn :: Way -> Double -> Branch -> Trail -> Carried
carryOn way now belief (Trail following left) = case (way, left) of
    (FoundAfresh, _) -> Carried (foundAfresh belief) now leaving
    (Resumed, Just (LeftBehind was believed)) -> Carried believed (recentSince was) Nothing
    _ -> Carried belief (maybe now recentSince following) leaving
  where
    leaving = case (way, left, following) of
        (Resumed, _, _) -> Nothing
        (FoundAfresh, Nothing, Just r) -> Just (LeftBehind r belief)
        _ -> left
