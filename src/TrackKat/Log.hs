-- | trackkat's two logs, told apart by who reads them.
module TrackKat.Log (
    Line (..),
) where

-- | A line to log.
data Line
    = -- | for the user: in the log trackkat's window shows, and on the
      -- terminal
      ForUser String
    | -- | for whoever is diagnosing trackkat: on the terminal alone, or
      -- the file or pipe its output goes to
      ForDeveloper String
    deriving (Eq, Show)
