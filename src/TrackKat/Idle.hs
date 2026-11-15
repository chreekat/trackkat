-- | When a trackkat that a game started quits for want of use.
module TrackKat.Idle (
    Use (..),
    Idle (..),
    idleSeconds,
    idle,
) where

import Text.Printf (printf)

import TrackKat.CommandLine (Start (..))
import TrackKat.Log (Line (..))
import TrackKat.World (Instant, seconds, (.-.))

-- | Whether a game is tracking or trackkat's window is open.
data Use = Used | Unused
    deriving (Eq, Show)

-- | How long trackkat has gone unused.
data Idle
    = Busy
    | -- | unused since this time
      IdleSince Instant
    | -- | unused for so long that it is to quit
      Quitting
    deriving (Eq, Show)

-- | When started by a game, how long trackkat waits, with no game tracking
-- and its window closed, before it quits. Long enough for a game to be
-- restarted.
idleSeconds :: Int
idleSeconds = 60

-- | Take in whether trackkat is in use at the time given:
-- started by a game, it is to quit once it has gone unused for
-- 'idleSeconds', and the log says when that wait begins, is called off and
-- ends; started otherwise, it stays.
idle :: Start -> Instant -> Use -> Idle -> (Idle, [Line])
idle start now use was = case (start, use, was) of
    (_, _, Quitting) -> (Quitting, [])
    (ByHand, _, _) -> (Busy, [])
    (AtLogin, _, _) -> (Busy, [])
    (ByGame, Used, Busy) -> (Busy, [])
    (ByGame, Used, IdleSince _) -> (Busy, [ForUser "A game or the window is using trackkat again; not quitting."])
    (ByGame, Unused, Busy) ->
        (IdleSince now, [ForUser (printf "No game or window is using trackkat; quitting in %d s unless one starts." idleSeconds)])
    (ByGame, Unused, IdleSince since)
        | now .-. since >= seconds (fromIntegral idleSeconds) ->
            (Quitting, [ForUser (printf "Quitting: no game or window used trackkat for %d s." idleSeconds)])
        | otherwise -> (was, [])
