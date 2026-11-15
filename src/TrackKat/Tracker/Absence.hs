-- | Telling a user, now and then, why the clip stays out of sight.
module TrackKat.Tracker.Absence (
    Absence,
    noAbsence,
    absence,
) where

import Data.Maybe (fromMaybe)

import TrackKat.Tracker.Outcome

-- | Since when the clip has been out of sight in every frame, if it is,
-- and when a user was last told why, with the reason given.
data Absence = Absence !(Maybe Double) !(Maybe (Double, Why))

-- | The clip in sight, and nothing told.
noAbsence :: Absence
noAbsence = Absence Nothing Nothing

-- | Seconds the clip is out of sight, with lights enough in view to be
-- it, before a user is told why; and the least between two tellings.
absenceSeconds, absenceQuiet :: Double
absenceSeconds = 1
absenceQuiet = 10

-- | Take in a frame's outcome at the given time: a line for a user's log,
-- once the clip has been out of sight for 'absenceSeconds' with three
-- lights or more in view, saying why they aren't taken for it. It is told
-- once each time the clip goes out of sight, and again if the reason
-- changes, but never within 'absenceQuiet' of the last time.
absence :: Double -> Outcome -> Absence -> (Absence, Maybe String)
absence _ Tracked{} (Absence _ told) = (Absence Nothing told, Nothing)
absence now Lost{outBlobs = n, outWhy = why} (Absence since told)
    | n >= 3 && now - began > absenceSeconds && due =
        ( began `seq` Absence (Just began) (Just (now, why))
        , Just ("The clip is out of sight, though " ++ show n ++ " lights are in view: " ++ describeWhy why ++ ".")
        )
    | otherwise = (began `seq` Absence (Just began) told, Nothing)
  where
    began = fromMaybe now since
    due = case told of
        Nothing -> True
        Just (at, was) -> now - at >= absenceQuiet && (at < began || not (sameWhy was why))
