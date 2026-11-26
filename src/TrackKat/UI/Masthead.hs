-- | What the masthead across the top of trackkat's window says. It is drawn by
-- "TrackKat.Gui.Masthead".
module TrackKat.UI.Masthead (
    MastheadState (..),
    mastheadVersion,
    MastheadSays (..),
    Verdict (..),
    mastheadSays,
) where

import Data.List (isPrefixOf)

import TrackKat.Display (UpdateCheck (..), UpdateStatus (..))

-- | What the masthead tells of trackkat.
data MastheadState = MastheadState
    { mastheadBuild :: String
    -- ^ the version, as the window's title has it
    , mastheadOffer :: Maybe String
    , mastheadCheck :: UpdateCheck
    , mastheadStatus :: Maybe UpdateStatus
    -- ^ what the last look for a newer version found, if there was one
    }
    deriving (Eq, Show)

-- | The build's number, leaving its commit to the title and the log window.
mastheadVersion :: MastheadState -> String
mastheadVersion s = go (mastheadBuild s)
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

mastheadSays :: MastheadState -> MastheadSays
mastheadSays s = case (mastheadOffer s, mastheadCheck s, mastheadStatus s) of
    (Just offer, _, _) -> SaysOffer offer
    (Nothing, CheckRunning, _) -> SaysStatus Plain "Checking\8230"
    (Nothing, CheckIdle, Just (UpToDate t)) -> SaysStatus Good t
    (Nothing, CheckIdle, Just (UpdateNote t)) -> SaysStatus Plain t
    (Nothing, CheckIdle, Nothing) -> SaysNothing
