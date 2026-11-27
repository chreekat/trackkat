-- | The log as the window keeps it: the latest lines of everything logged,
-- for the log window ("TrackKat.Gui.Log") to show and copy.
module TrackKat.UI.Log (
    LogBook,
    emptyLog,
    record,
    Mark,
    start,
    since,
    logMax,
) where

-- | The latest lines logged, about 'logMax' characters of them, and how
-- many came before them.
data LogBook = LogBook
    { bookOldest :: ![String]
    -- ^ the older lines kept, oldest first
    , bookNewest :: ![String]
    -- ^ the rest, newest first
    , bookKept :: !Int
    -- ^ lines kept
    , bookDropped :: !Int
    -- ^ lines logged before the first kept
    , bookChars :: !Int
    -- ^ in the lines kept
    }
    deriving (Eq, Show)

emptyLog :: LogBook
emptyLog = LogBook [] [] 0 0 0

-- | How much of the log is kept, in characters, as LOG_MAX in cbits/gui.c.
logMax :: Int
logMax = 256 * 1024

-- | Log a line, no longer than a quarter of what is kept, dropping the
-- oldest lines to keep to 'logMax'.
record :: String -> LogBook -> LogBook
record line b = trim b{bookNewest = cut : bookNewest b, bookKept = bookKept b + 1, bookChars = bookChars b + length cut}
  where
    cut = take (logMax `div` 4) line
    trim k
        | bookChars k <= logMax = k
        | oldest : rest <- bookOldest k =
            trim k{bookOldest = rest, bookKept = bookKept k - 1, bookDropped = bookDropped k + 1, bookChars = bookChars k - length oldest}
        | null (bookNewest k) = k
        | otherwise = trim k{bookOldest = reverse (bookNewest k), bookNewest = []}

-- | How many lines had been logged at some moment.
newtype Mark = Mark Int
    deriving (Eq, Show)

-- | Before anything was logged.
start :: Mark
start = Mark 0

-- | The lines logged since a mark that are still kept, and the mark for
-- now.
since :: Mark -> LogBook -> ([String], Mark)
since (Mark seen) b =
    ( drop (seen - bookDropped b) (bookOldest b ++ reverse (bookNewest b))
    , Mark (bookDropped b + bookKept b)
    )
