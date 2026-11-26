-- | Telling a user, now and then, why the clip stays out of sight.
module TrackKat.Tracker.Absence (
    Absence,
    noAbsence,
    absence,
    Telling,
    noTelling,
    telling,
) where

import Data.Maybe (fromMaybe)

import TrackKat.Tracker.Outcome
import TrackKat.World (Instant, Seconds, seconds, (.-.))

-- | Since when the clip has been out of sight in every frame, if it is,
-- and when a user was last told why, with the reason given.
data Telling why = Telling !(Maybe Instant) !(Maybe (Instant, why))

-- | The clip in sight, and nothing told.
noTelling :: Telling why
noTelling = Telling Nothing Nothing

-- | What is told of the clip staying out of sight, as this tracker gives
-- reasons.
type Absence = Telling Why

-- | See 'noTelling'.
noAbsence :: Absence
noAbsence = noTelling

-- | How long the clip is out of sight, with lights enough in view to be
-- it, before a user is told why; and the least between two tellings.
absenceSeconds, absenceQuiet :: Seconds
absenceSeconds = seconds 1
absenceQuiet = seconds 10

-- | Take in a frame's outcome at the given time: a line for a user's log,
-- as 'telling' says.
absence :: Instant -> Outcome -> Absence -> (Absence, Maybe String)
absence now outcome = telling sameWhy describeWhy now $ case outcome of
    Tracked{} -> Nothing
    Lost{outBlobs = n, outWhy = why} -> Just (n, why)

-- | Take in a frame at the given time, the clip in sight, or out of sight
-- with so many blobs in view, for a reason that compares with another
-- and is told as given: a line for a user's log, once the clip has been
-- out of sight for 'absenceSeconds' with three lights or more in view,
-- saying why they aren't taken for it. It is told once each time the clip
-- goes out of sight, and again if the reason changes, but never within
-- 'absenceQuiet' of the last time.
telling :: (why -> why -> Bool) -> (why -> String) -> Instant -> Maybe (Int, why) -> Telling why -> (Telling why, Maybe String)
telling _ _ _ Nothing (Telling _ told) = (Telling Nothing told, Nothing)
telling same describe now (Just (n, why)) (Telling since told)
    | n >= 3 && now .-. began > absenceSeconds && due =
        ( began `seq` Telling (Just began) (Just (now, why))
        , Just ("The clip is out of sight, though " ++ show n ++ " lights are in view: " ++ describe why ++ ".")
        )
    | otherwise = (began `seq` Telling (Just began) told, Nothing)
  where
    began = fromMaybe now since
    due = case told of
        Nothing -> True
        Just (at, was) -> now .-. at >= absenceQuiet && (at < began || not (same was why))
