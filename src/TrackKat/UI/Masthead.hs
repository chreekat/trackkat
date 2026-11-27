-- | What the masthead across the top of trackkat's window says. It is drawn by
-- "TrackKat.Gui.Masthead".
module TrackKat.UI.Masthead (
    mastheadVersion,
    MastheadSays (..),
    Verdict (..),
    mastheadSays,
) where

import Data.List (isPrefixOf)

import TrackKat.Display (Notice (..), Shown (..), UpdateCheck (..), UpdateStatus (..))

-- | The build's number, leaving its commit to the title and the log window.
mastheadVersion :: Shown -> String
mastheadVersion s = go (shownVersion s)
  where
    go v@(c : rest)
        | " (" `isPrefixOf` v = ""
        | otherwise = c : go rest
    go [] = []

-- | What the masthead says after the version.
data MastheadSays
    = -- | on amber, in place of anything else
      SaysOffer String
    | SaysStatus Verdict String
    | SaysNothing
    deriving (Eq, Show)

-- | Whether a status is good news, shown in green with a tick.
data Verdict = Good | Plain
    deriving (Eq, Show)

mastheadSays :: Shown -> MastheadSays
mastheadSays s = case (shownOffer s, shownCheck s, shownStatus s) of
    (Just offer, _, _) -> SaysOffer (noticeText offer)
    (Nothing, CheckRunning, _) -> SaysStatus Plain "Checking\8230"
    (Nothing, CheckIdle, Just (UpToDate t)) -> SaysStatus Good t
    (Nothing, CheckIdle, Just (UpdateNote t)) -> SaysStatus Plain t
    (Nothing, CheckIdle, Nothing) -> SaysNothing
