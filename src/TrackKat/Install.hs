{-# LANGUAGE LambdaCase #-}

-- | Installing trackkat for the user, keeping it up to date and
-- uninstalling it, on Windows, as TrackKat.Release decides: see
-- cbits/install.c.
module TrackKat.Install (
    Paths (..),
    findPaths,
    configFile,
    versionExe,
    installedVersions,
    moveSettings,
    installCopy,
    register,
    setStartup,
    launch,
    handOver,
    waitForExit,
    cleanUp,
    checkForUpdate,
    Uninstall (..),
    uninstall,
) where

import Control.Exception (IOException, try)
import Control.Monad (filterM, forM, forM_, unless, when)
import Data.Char (toLower)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Word (Word32)
import Foreign.C.String (CString, CWString, peekCString, peekCWString, withCWString)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (allocaArray)
import System.Directory
import System.Environment (getExecutablePath, lookupEnv)
import System.FilePath (addTrailingPathSeparator, normalise, takeDirectory, (</>))
import System.IO (readFile')
import Text.Printf (printf)

import TrackKat.Config (Startup (..))
import TrackKat.Gui (quitRunning)
import TrackKat.Release

foreign import ccall unsafe "hts_known_folder" c_known_folder :: CInt -> CWString -> CInt -> IO CInt
foreign import ccall safe "hts_make_shortcut" c_make_shortcut :: CWString -> CWString -> CWString -> IO CInt
foreign import ccall unsafe "hts_set_uninstall_entry" c_set_uninstall_entry :: CWString -> CWString -> CWString -> CInt -> IO CInt
foreign import ccall unsafe "hts_remove_uninstall_entry" c_remove_uninstall_entry :: IO CInt
foreign import ccall unsafe "hts_set_run" c_set_run :: CWString -> CWString -> CInt -> IO CInt
foreign import ccall unsafe "hts_remove_run" c_remove_run :: CWString -> IO CInt
foreign import ccall unsafe "hts_get_npclient_path" c_get_npclient_path :: CWString -> CInt -> IO CInt
foreign import ccall unsafe "hts_remove_npclient_path" c_remove_npclient_path :: IO CInt
foreign import ccall unsafe "hts_start" c_start :: CWString -> CWString -> CWString -> CInt -> IO CInt
foreign import ccall safe "hts_wait_exit" c_wait_exit :: CInt -> CInt -> IO CInt
foreign import ccall unsafe "hts_own_pid" c_own_pid :: IO CInt
foreign import ccall safe "hts_ask" c_ask :: CWString -> CInt -> IO CInt
foreign import ccall safe "hts_download" c_download :: CWString -> CWString -> IO CInt
foreign import ccall safe "hts_sha256" c_sha256 :: CWString -> CString -> IO CInt

-- | Where trackkat is, and where it installs and keeps things.
data Paths = Paths
    { exeDir :: FilePath
    -- ^ the folder of the trackkat.exe running
    , installRoot :: FilePath
    -- ^ @%LOCALAPPDATA%\\Programs\\trackkat@, a folder per version
    , settingsDir :: FilePath
    -- ^ @%APPDATA%\\trackkat@, for settings, calibration and NPClient's log
    , shortcut :: FilePath
    -- ^ the Start menu's shortcut
    }

findPaths :: IO Paths
findPaths = do
    exe <- getExecutablePath
    local <- knownFolder 0
    roaming <- knownFolder 1
    programs <- knownFolder 2
    pure
        Paths
            { exeDir = takeDirectory exe
            , installRoot = local </> "Programs" </> "trackkat"
            , settingsDir = roaming </> "trackkat"
            , shortcut = programs </> "trackkat.lnk"
            }

-- | trackkat.conf, where trackkat reads its settings from, whether it is
-- installed or not.
configFile :: Paths -> FilePath
configFile p = settingsDir p </> "trackkat.conf"

knownFolder :: CInt -> IO FilePath
knownFolder which = allocaArray 1024 $ \buf -> do
    rc <- c_known_folder which buf 1024
    if rc == 0 then peekCWString buf else ioError (userError ("can't find a known folder (error " ++ hex rc ++ ")"))

-- | An installed version's trackkat.exe.
versionExe :: Paths -> Version -> FilePath
versionExe p v = installRoot p </> versionFolder v </> "trackkat.exe"

-- | The versions installed completely.
installedVersions :: Paths -> IO [Version]
installedVersions p = do
    names <- either (\e -> const [] (e :: IOException)) id <$> try (listDirectory (installRoot p))
    filterM (doesFileExist . versionExe p) (mapMaybe folderVersion names)

-- | Move settings files that older versions kept beside trackkat.exe to
-- 'settingsDir', once.
moveSettings :: (String -> IO ()) -> Paths -> IO ()
moveSettings say p = do
    createDirectoryIfMissing True (settingsDir p)
    forM_ settingsFiles $ \f -> do
        let beside = exeDir p </> f
            there = settingsDir p </> f
        found <- leftover <$> doesFileExist beside <*> doesFileExist there
        case found of
            Nothing -> pure ()
            Just IgnoreBeside -> say (beside ++ " is no longer used; trackkat uses " ++ there ++ ".")
            Just MoveToSettings ->
                try (copyFileWithMetadata beside there >> removeFile beside) >>= \case
                    Right () -> say ("Moved " ++ beside ++ " to " ++ there ++ ".")
                    Left e -> say ("Can't move " ++ beside ++ " to " ++ there ++ ": " ++ show (e :: IOException))

-- | Copy this copy of trackkat, of the given version, into the install
-- root, unless that version is installed already. Returns its folder.
installCopy :: Paths -> Version -> IO (Either String FilePath)
installCopy p v = do
    let dest = installRoot p </> versionFolder v
        partial = dest ++ partialSuffix
    done <- doesFileExist (dest </> "trackkat.exe")
    if done
        then pure (Right dest)
        else
            try
                ( do
                    createDirectoryIfMissing True (installRoot p)
                    removePathForcibly partial
                    createDirectory partial
                    copied <- forM releaseFiles $ \f -> do
                        there <- doesFileExist (exeDir p </> f)
                        when there $ copyFileWithMetadata (exeDir p </> f) (partial </> f)
                        pure (f, there)
                    case [f | (f, False) <- copied, f `elem` ["trackkat.exe", "NPClient.dll", "NPClient64.dll", "libusb-1.0.dll"]] of
                        [] -> renameDirectory partial dest >> pure (Right dest)
                        missing -> do
                            removePathForcibly partial
                            pure (Left ("this folder has no " ++ unwords missing))
                )
                >>= \case
                    Left e -> pure (Left (show (e :: IOException)))
                    Right r -> pure r

-- | Make the Start menu's shortcut and Settings' uninstall entry point at
-- the installed trackkat running, of the given version.
register :: (String -> IO ()) -> Paths -> Version -> IO ()
register say p v = do
    let exe = exeDir p </> "trackkat.exe"
    rc <- withCWString (shortcut p) $ \l -> withCWString exe $ \t ->
        withCWString "Track your head with the TrackIR 5 for games" (c_make_shortcut l t)
    unless (rc == 0) $ say ("Can't make the Start menu's shortcut (error " ++ hex rc ++ ").")
    sizes <- mapM (getFileSize . (exeDir p </>)) =<< listDirectory (exeDir p)
    rc' <- withCWString exe $ \e -> withCWString (installRoot p) $ \l -> withCWString (renderVersion v) $ \ver ->
        c_set_uninstall_entry e l ver (fromIntegral (sum sizes `div` 1024))
    unless (rc' == 0) $ say ("Can't list trackkat in Settings' apps (error " ++ show rc' ++ ").")

-- | Have Windows start this copy of trackkat in the tray at login, or not.
-- trackkat-tray.exe starts it without a console window.
setStartup :: (String -> IO ()) -> Paths -> Startup -> IO ()
setStartup say p = \case
    StartAtLogin -> do
        let tray = exeDir p </> "trackkat-tray.exe"
        there <- doesFileExist tray
        if not there
            then say ("Can't start with Windows: there is no " ++ tray ++ ".")
            else do
                rc <- withCWString runName $ \n -> withCWString (quote tray) $ \c -> c_set_run n c 0
                unless (rc == 0) $ say ("Can't start with Windows (registry error " ++ show rc ++ ").")
    NoStartAtLogin -> do
        rc <- withCWString runName c_remove_run
        unless (rc == 0) $ say ("Can't stop starting with Windows (registry error " ++ show rc ++ ").")

runName :: String
runName = "trackkat"

quote :: String -> String
quote s = "\"" ++ s ++ "\""

-- | Start another trackkat.exe with these arguments, telling it to take
-- over once this process has quit. Left is a Windows error code.
handOver :: FilePath -> [String] -> IO (Either Int ())
handOver exe args = do
    pid <- c_own_pid
    launch exe (["--after", show pid] ++ args)

-- | Start a trackkat.exe with these arguments. Left is a Windows error code.
launch :: FilePath -> [String] -> IO (Either Int ())
launch exe args = launchWith exe args False

-- | Start a program, with or without a console window. Left is a Windows
-- error code.
launchWith :: FilePath -> [String] -> Bool -> IO (Either Int ())
launchWith exe args noWindow =
    withCWString exe $ \e -> withCWString (unwords (quote exe : args)) $ \c -> withCWString (takeDirectory exe) $ \d -> do
        rc <- c_start e c d (if noWindow then 1 else 0)
        pure (if rc < 0 then Left (fromIntegral (negate rc)) else Right ())

-- | Wait up to the given number of seconds for a process to quit; True
-- once it has.
waitForExit :: Int -> Int -> IO Bool
waitForExit pid seconds = (/= 0) <$> c_wait_exit (fromIntegral pid) (fromIntegral (seconds * 1000))

-- | Delete older versions, as 'staleFolders' picks them, except files
-- games still have loaded, which are left for next time. Returns whether
-- any were left.
cleanUp :: (String -> IO ()) -> Paths -> Placement -> IO Bool
cleanUp say p place = do
    names <- either (\e -> const [] (e :: IOException)) id <$> try (listDirectory (installRoot p))
    left <- forM (staleFolders place names) $ \name -> do
        gone <- deleteFolder (installRoot p </> name)
        when (gone && not (partialSuffix `isSuffixOf` name)) $ say ("Removed trackkat " ++ name ++ ", which is no longer used.")
        pure (not gone)
    pure (or left)

-- | Delete a folder of files in 'deletionOrder', stopping at the first
-- that can't be, as one a game has loaded. True if it is gone.
deleteFolder :: FilePath -> IO Bool
deleteFolder dir =
    try (listDirectory dir) >>= \case
        Left e -> const (pure False) (e :: IOException)
        Right files -> go (deletionOrder files)
  where
    go [] = either (\e -> const False (e :: IOException)) (const True) <$> try (removeDirectory dir)
    go (f : fs) =
        try (removePathForcibly (dir </> f)) >>= \case
            Left e -> const (pure False) (e :: IOException)
            Right () -> go fs

-- | The metadata of the latest release, unless TRACKKAT_UPDATE_URL names
-- other metadata: for testing, perhaps a file, whose zip is then a file
-- too.
defaultMetadata :: String
defaultMetadata = "https://github.com/chreekat/trackkat/releases/download/dev/trackkat-windows.json"

-- | Look for a release newer than this version and any installed, and if
-- there is one, download, check and unpack it into the install root.
-- Returns the version unpacked, if any.
checkForUpdate :: Paths -> Version -> IO (Either LookFailure (Maybe Version))
checkForUpdate p self = do
    metaUrl <- fromMaybe defaultMetadata <$> lookupEnv "TRACKKAT_UPDATE_URL"
    temp <- getTemporaryDirectory
    let metaFile = temp </> "trackkat-windows.json"
    either' (failed (parseLocation metaUrl)) $ \loc ->
        fetch loc metaFile `andThen` \() ->
            failing (readText metaFile) `andThen` \text ->
                either' (failed (parseRelease text)) $ \r -> do
                    installed <- installedVersions p
                    let v = releaseVersion r
                    if v <= self || any (>= v) installed
                        then pure (Right Nothing)
                        else either' (failed (releaseLocation loc (releaseUrl r))) $ \zipLoc -> do
                            let zipFile = temp </> ("trackkat-" ++ versionFolder v ++ ".zip")
                                dest = installRoot p </> versionFolder v
                                partial = dest ++ partialSuffix
                            result <-
                                fetch zipLoc zipFile `andThen` \() ->
                                    failing $
                                        sha256 zipFile `andThen` \digest ->
                                            if not (sameDigest digest (releaseSha256 r))
                                                then pure (Left ("the download's SHA-256 is " ++ digest ++ ", not " ++ releaseSha256 r))
                                                else
                                                    io (createDirectoryIfMissing True (installRoot p) >> removePathForcibly partial) `andThen` \() ->
                                                        extractRelease zipFile partial `andThen` \() ->
                                                            io (renameDirectory partial dest) `andThen` \() -> pure (Right (Just v))
                            _ <- try (removeFile zipFile) :: IO (Either IOException ())
                            pure result
  where
    either' e k = either (pure . Left) k e
    readText f = io (readFile' f)
    failed = either (Left . LookFailed) Right
    failing = fmap failed

andThen :: IO (Either e a) -> (a -> IO (Either e b)) -> IO (Either e b)
andThen m k = m >>= either (pure . Left) k

-- | Run an action, catching IO errors.
io :: IO a -> IO (Either String a)
io m = either (\e -> Left (show (e :: IOException))) Right <$> try m

-- | Copy what a location holds to a file.
fetch :: Location -> FilePath -> IO (Either LookFailure ())
fetch (LocalFile f) to = either (Left . LookFailed) Right <$> io (copyFile f to)
fetch (Web url) to = withCWString url $ \u -> withCWString to $ \t ->
    c_download u t >>= \case
        0 -> pure (Right ())
        rc
            | rc < 0 -> pure (Left (httpFailure url (fromIntegral (negate rc))))
            | otherwise -> pure (Left (LookFailed (url ++ ": Windows error " ++ show rc)))

sha256 :: FilePath -> IO (Either String String)
sha256 f = withCWString f $ \path -> allocaBytes 65 $ \out ->
    c_sha256 path out >>= \case
        0 -> Right <$> peekCString out
        rc -> pure (Left ("can't hash " ++ f ++ " (error " ++ hex rc ++ ")"))

-- | An HRESULT or NTSTATUS, as Windows writes them.
hex :: CInt -> String
hex rc = printf "0x%08x" (fromIntegral rc :: Word32)

-- | How to uninstall: asking whether to keep settings, or quietly keeping
-- them.
data Uninstall = AskFirst | Quietly

-- | Remove trackkat: quit it if it is running, and remove its shortcut,
-- uninstall entry, start at login, games' pointer to its NPClient, and
-- program folders. Files in use, such as this trackkat.exe and DLLs games
-- have loaded, are deleted once this process has quit, or at next login.
-- Settings are kept unless the user says otherwise.
uninstall :: Uninstall -> IO ()
uninstall how = do
    p <- findPaths
    keep <- case how of
        Quietly -> pure (Just True)
        AskFirst ->
            ask
                ( "Uninstall trackkat?\n\nYes: uninstall it, keeping your settings and calibration, in "
                    ++ settingsDir p
                    ++ ", for another time.\nNo: uninstall it and delete them too.\nCancel: keep trackkat."
                )
                1
                >>= \case
                    6 -> pure (Just True)
                    7 -> pure (Just False)
                    _ -> pure Nothing
    forM_ keep $ \keepSettings -> do
        quitRunning >>= mapM_ (\pid -> waitForExit pid 10)
        _ <- try (removeFile (shortcut p)) :: IO (Either IOException ())
        _ <- c_remove_uninstall_entry
        _ <- withCWString runName c_remove_run
        games <- allocaArray 1024 $ \buf -> c_get_npclient_path buf 1024 >>= \found -> if found /= 0 then Just <$> peekCWString buf else pure Nothing
        when (maybe False (\g -> inside (installRoot p) g || inside (exeDir p) g) games) $ () <$ c_remove_npclient_path
        names <- either (\e -> const [] (e :: IOException)) id <$> try (listDirectory (installRoot p))
        mapM_ (\n -> deleteAll (installRoot p </> n)) names
        _ <- try (removeDirectory (installRoot p)) :: IO (Either IOException ())
        rest <- doesDirectoryExist (installRoot p)
        comspec <- fromMaybe "C:\\Windows\\System32\\cmd.exe" <$> lookupEnv "ComSpec"
        let removeRoot = comspec ++ " /d /c rd /s /q " ++ quote (installRoot p)
        lockedByGames <-
            if not rest
                then pure False
                else do
                    -- This trackkat.exe, and the DLLs it loaded, go once it has quit.
                    _ <- launchWith comspec ["/d", "/c", "ping -n 4 127.0.0.1 >nul & rd /s /q", quote (installRoot p)] True
                    others <- filter (not . inside (exeDir p)) <$> filesUnder (installRoot p)
                    unless (null others) $ () <$ withCWString "trackkat uninstall" (\n -> withCWString removeRoot (\c -> c_set_run n c 1))
                    pure (not (null others))
        unless keepSettings $ () <$ (try (removePathForcibly (settingsDir p)) :: IO (Either IOException ()))
        let done =
                "trackkat is uninstalled."
                    ++ (if lockedByGames then " Some of its files are in use, by a game perhaps; Windows removes them when you next log in." else "")
                    ++ (if keepSettings then " Your settings are kept in " ++ settingsDir p ++ "." else "")
        putStrLn done
        case how of
            AskFirst -> () <$ ask done 0
            Quietly -> pure ()
  where
    ask text kind = withCWString text $ \t -> c_ask t kind
    inside dir path = map toLower (addTrailingPathSeparator (normalise dir)) `isPrefixOf` map toLower (addTrailingPathSeparator (normalise path))
    deleteAll dir = do
        isDir <- doesDirectoryExist dir
        if not isDir
            then () <$ (try (removeFile dir) :: IO (Either IOException ()))
            else do
                files <- either (\e -> const [] (e :: IOException)) id <$> try (listDirectory dir)
                forM_ (deletionOrder files) $ \f -> try (removePathForcibly (dir </> f)) :: IO (Either IOException ())
                () <$ (try (removeDirectory dir) :: IO (Either IOException ()))
    filesUnder dir = do
        entries <- either (\e -> const [] (e :: IOException)) id <$> try (listDirectory dir)
        concat
            <$> forM
                entries
                ( \e -> do
                    isDir <- doesDirectoryExist (dir </> e)
                    if isDir then filesUnder (dir </> e) else pure [normalise (dir </> e)]
                )
