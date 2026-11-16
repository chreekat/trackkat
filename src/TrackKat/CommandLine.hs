{-# LANGUAGE LambdaCase #-}

-- | trackkat's command line: what it asks for, and the arguments that
-- start another trackkat the way this one was started.
module TrackKat.CommandLine (
    Command (..),
    Start (..),
    WindowAtStart (..),
    Recorded (..),
    continued,
    Uninstall (..),
    parseCommand,
    usage,
    startArgs,
    afterArgs,
) where

import Data.List (isPrefixOf)
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

-- | Whether what the camera sees is recorded, and to which file.
data Recorded
    = Unrecorded
    | -- | (@--record@) to a file named for when it began, kept with the
      -- settings
      RecordedByDate
    | -- | (@--record FILE@) to this file, which doesn't begin with two
      -- dashes
      RecordedTo FilePath
    deriving (Eq, Show)

-- | How a trackkat that takes over from one that has been recording goes
-- on: recording still, but never to the file the first one named, which
-- it would replace.
continued :: Recorded -> Recorded
continued Unrecorded = Unrecorded
continued _ = RecordedByDate

-- | How to uninstall: asking whether to keep settings, or quietly keeping
-- them.
data Uninstall = AskFirst | Quietly
    deriving (Eq, Show)

-- | What the command line asks for.
data Command
    = -- | track, after the process given, if any, has quit
      Run Start WindowAtStart Recorded (Maybe Int)
    | -- | install this copy, and start it
      InstallCommand
    | UninstallCommand Uninstall
    | -- | name the files of a release, for CI to check the zip
      ListFiles
    deriving (Eq, Show)

-- | What the arguments ask for; nothing if they aren't trackkat's.
parseCommand :: [String] -> Maybe Command
parseCommand = \case
    ["--install"] -> Just InstallCommand
    ["--uninstall"] -> Just (UninstallCommand AskFirst)
    ["--uninstall", "--quiet"] -> Just (UninstallCommand Quietly)
    ["--list-files"] -> Just ListFiles
    args -> go ByHand Nothing Unrecorded Nothing args
  where
    go s w r a = \case
        [] -> Just (Run s (fromMaybe (defaultWindow s) w) r a)
        "--by-game" : rest -> go ByGame w r a rest
        "--tray" : rest -> go AtLogin w r a rest
        "--window" : rest -> go s (Just WindowShown) r a rest
        "--record" : file : rest | not (null file), not ("--" `isPrefixOf` file) -> go s w (RecordedTo file) a rest
        "--record" : rest -> go s w RecordedByDate a rest
        "--after" : pid : rest | Just n <- parseCount pid -> go s w r (Just n) rest
        _ -> Nothing

-- | What is printed for arguments that aren't trackkat's.
usage :: String
usage =
    unlines
        [ "usage: trackkat [--by-game | --tray] [--window] [--record [FILE]]"
        , "                [--after PID]"
        , "       trackkat --install | --uninstall [--quiet] | --list-files"
        , ""
        , "  --by-game    started by a game: in the tray, quitting once games are done"
        , "  --tray       started at login: in the tray"
        , "  --window     open the window even so"
        , "  --record     record every frame the camera sends, with the settings"
        , "               and what was asked of tracking, to FILE, or to a file"
        , "               named for the time, kept with the settings, to replay"
        , "               with tirprobe --replay; about 6 MB a minute"
        , "  --after PID  take over from that trackkat once it has quit"
        , "  --install    install this copy for this user, and start it"
        , "  --uninstall  uninstall trackkat; --quiet keeps settings without asking"
        , "  --list-files name the files of a release"
        ]

defaultWindow :: Start -> WindowAtStart
defaultWindow ByHand = WindowShown
defaultWindow _ = WindowHidden

-- | The arguments that start trackkat as it was started.
startArgs :: Start -> WindowAtStart -> Recorded -> [String]
startArgs s w r = how ++ window ++ recording
  where
    recording = case r of
        Unrecorded -> []
        RecordedByDate -> ["--record"]
        RecordedTo file -> ["--record", file]
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
