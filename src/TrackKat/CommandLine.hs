{-# LANGUAGE LambdaCase #-}

-- | trackkat's command line: what it asks for, and the arguments that
-- start another trackkat the way this one was started.
module TrackKat.CommandLine (
    Command (..),
    Start (..),
    WindowAtStart (..),
    Uninstall (..),
    parseCommand,
    usage,
    startArgs,
    afterArgs,
) where

import Data.Maybe (fromMaybe)

import TrackKat.Number (parseCount, renderNatural)

-- | How trackkat was started.
data Start
    = ByHand
    | -- | by NPClient, for a game (@--by-game@): in the tray, and quitting
      -- when nothing has used it for a while
      ByGame
    | -- | by Windows at login (@--tray@): in the tray, until quit
      AtLogin
    deriving (Eq, Show)

-- | Whether the window opens at start; the tray icon always shows.
data WindowAtStart = WindowShown | WindowHidden
    deriving (Eq, Show)

-- | How to uninstall: asking whether to keep settings, or quietly keeping
-- them.
data Uninstall = AskFirst | Quietly
    deriving (Eq, Show)

-- | What the command line asks for.
data Command
    = -- | track, after the process given, if any, has quit
      Run Start WindowAtStart (Maybe Int)
    | -- | install this copy, and start it
      InstallCommand
    | UninstallCommand Uninstall
    | -- | name the files of a release, for CI to check the zip
      ListFiles
    deriving (Eq, Show)

parseCommand :: [String] -> Maybe Command
parseCommand = \case
    ["--install"] -> Just InstallCommand
    ["--uninstall"] -> Just (UninstallCommand AskFirst)
    ["--uninstall", "--quiet"] -> Just (UninstallCommand Quietly)
    ["--list-files"] -> Just ListFiles
    args -> go ByHand Nothing Nothing args
  where
    go s w a = \case
        [] -> Just (Run s (fromMaybe (defaultWindow s) w) a)
        "--by-game" : rest -> go ByGame w a rest
        "--tray" : rest -> go AtLogin w a rest
        "--window" : rest -> go s (Just WindowShown) a rest
        "--after" : pid : rest | Just n <- parseCount pid -> go s w (Just n) rest
        _ -> Nothing

usage :: String
usage =
    unlines
        [ "usage: trackkat [--by-game | --tray] [--window] [--after PID]"
        , "       trackkat --install | --uninstall [--quiet] | --list-files"
        , ""
        , "  --by-game    started by a game: in the tray, quitting once games are done"
        , "  --tray       started at login: in the tray"
        , "  --window     open the window even so"
        , "  --after PID  take over from that trackkat once it has quit"
        , "  --install    install this copy for this user, and start it"
        , "  --uninstall  uninstall trackkat; --quiet keeps settings without asking"
        , "  --list-files name the files of a release"
        ]

defaultWindow :: Start -> WindowAtStart
defaultWindow ByHand = WindowShown
defaultWindow _ = WindowHidden

-- | The arguments that start trackkat as it was started.
startArgs :: Start -> WindowAtStart -> [String]
startArgs s w = how ++ window
  where
    how = case s of
        ByHand -> []
        ByGame -> ["--by-game"]
        AtLogin -> ["--tray"]
    window = case (s, w) of
        (ByHand, _) -> []
        (_, WindowShown) -> ["--window"]
        (_, WindowHidden) -> []

-- | The arguments that have trackkat take over from the process given, if
-- any, once it has quit.
afterArgs :: Maybe Int -> [String]
afterArgs = maybe [] (\pid -> ["--after", renderNatural (toInteger pid)])
