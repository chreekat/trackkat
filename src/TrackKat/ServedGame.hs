-- | The game trackkat serves through NaturalPoint's NPClient, remembered in
-- a file so that a trackkat started while the game still runs serves it
-- again: NaturalPoint's DLL tells the software about a game only once, when
-- the game starts head tracking.
module TrackKat.ServedGame (
    ServedGame (..),
    HeadTracking (..),
    renderServedGame,
    parseServedGame,
    GameProcess (..),
    stillServed,
    describeServedAgain,
    Note (..),
    note,
) where

import Control.Monad (guard)
import Data.Char (isSpace)
import Data.Word (Word64)
import Text.Printf (printf)

import TrackKat.Number (parseNatural, renderNatural)

-- | A game served, as cbits/trackir.c's @hts_trackir_served@ reports it.
data ServedGame = ServedGame
    { servedProcess :: Int
    -- ^ the game's process ID
    , servedStarted :: Maybe Word64
    -- ^ when the process started, as a Windows FILETIME, if Windows said
    , servedWindow :: Word64
    -- ^ the window the game registered
    , servedProfile :: Maybe Int
    -- ^ the profile ID the game registered, which picks the key its data is
    -- scrambled with
    , servedTracking :: HeadTracking
    }
    deriving (Eq, Show)

-- | Whether the game has started head tracking, and not stopped it, so
-- counts among the games using trackkat.
data HeadTracking = TrackingStarted | TrackingStopped
    deriving (Eq, Show)

-- | The file that remembers the game served.
renderServedGame :: ServedGame -> String
renderServedGame g =
    unlines
        [ "# The game trackkat serves through NaturalPoint's NPClient, so that"
        , "# trackkat, restarted while the game runs, serves it again. trackkat"
        , "# writes it, and deletes it once the game is done."
        , "process = " ++ whole (servedProcess g)
        , "started = " ++ maybe "unknown" whole (servedStarted g)
        , "window = " ++ whole (servedWindow g)
        , "profile = " ++ maybe "none" whole (servedProfile g)
        , "tracking = " ++ case servedTracking g of
            TrackingStarted -> "yes"
            TrackingStopped -> "no"
        ]
  where
    whole n = renderNatural (toInteger n)

-- | Read what 'renderServedGame' wrote, or say what is wrong with it.
parseServedGame :: String -> Either String ServedGame
parseServedGame str = do
    settings <- traverse setting (filter (not . all isSpace) (map (takeWhile (/= '#')) (lines str)))
    let field name parse = case [v | (n, v) <- settings, n == name] of
            [v] -> maybe (Left (name ++ ": can't read " ++ show v)) Right (parse v)
            [] -> Left ("no " ++ name)
            _ -> Left (name ++ " is given twice")
    ServedGame
        <$> field "process" (between 1 (maxBound :: Int))
        <*> field "started" (\v -> if v == "unknown" then Just Nothing else Just <$> between 0 (maxBound :: Word64) v)
        <*> field "window" (between 0 (maxBound :: Word64))
        <*> field "profile" (\v -> if v == "none" then Just Nothing else Just <$> between 0 (65535 :: Int) v)
        <*> field "tracking" tracking
  where
    setting l = case break (== '=') l of
        (name, '=' : value) -> Right (trim name, trim value)
        _ -> Left ("expected name = value, not " ++ show (trim l))
    trim = f . f where f = reverse . dropWhile isSpace
    -- A whole number in digits, no less than the first and no more than
    -- the second.
    between :: (Integral a) => a -> a -> String -> Maybe a
    between least most v = do
        n <- parseNatural v
        guard (n >= toInteger least && n <= toInteger most)
        pure (fromInteger n)
    tracking "yes" = Just TrackingStarted
    tracking "no" = Just TrackingStopped
    tracking _ = Nothing

-- | What became of the process with a remembered game's process ID.
data GameProcess
    = -- | it has exited
      ProcessGone
    | -- | it runs, and started at this FILETIME, if Windows says: anti-cheat
      -- may keep it from saying
      ProcessRunning (Maybe Word64)
    deriving (Eq, Show)

-- | The remembered game, if it is still running: the process with its ID
-- started when the game did, or, where Windows won't say when either
-- started, still has the window the game registered, given here as the
-- process that window belongs to now, if it is still open. Otherwise the
-- ID now belongs to another process, or none.
stillServed :: ServedGame -> GameProcess -> Maybe Int -> Maybe ServedGame
stillServed g process windowOwner = case (process, servedStarted g) of
    (ProcessGone, _) -> Nothing
    (ProcessRunning (Just now), Just before) -> if now == before then Just g else Nothing
    (ProcessRunning _, _) -> if windowOwner == Just (servedProcess g) then Just g else Nothing

-- | The log's line for a game served again, given the game's name, if its
-- profile has one known.
describeServedAgain :: ServedGame -> Maybe String -> String
describeServedAgain g name = "Serving " ++ game ++ " again, which was running before trackkat restarted."
  where
    game = case (servedProfile g, name) of
        (Just i, Just n) -> printf "%s (game %d)" n i
        (Just i, Nothing) -> "game " ++ show i
        (Nothing, _) -> "the game in process " ++ show (servedProcess g)

-- | What to do to the file that remembers the game served.
data Note
    = -- | replace it whole with this text
      Write String
    | -- | delete it
      Delete
    deriving (Eq, Show)

-- | What to do to the file, if anything, given how many times the game
-- served had changed when the file was last brought up to date, how many
-- times it has now, and the game served now, if any.
note :: Int -> Int -> Maybe ServedGame -> Maybe Note
note seen changes game
    | changes == seen = Nothing
    | otherwise = Just (maybe Delete (Write . renderServedGame) game)
