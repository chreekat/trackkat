{-# LANGUAGE LambdaCase #-}

-- | trackkat's releases: their versions, the metadata published with each,
-- and the decisions about installing, updating and settings that follow
-- from where trackkat runs. Everything here but 'extractRelease' is pure;
-- TrackKat.Install acts on it, on Windows.
--
-- An installed trackkat lives in @%LOCALAPPDATA%\\Programs\\trackkat@, the
-- install root, one folder per version, named by its numbers (@0.1.123@).
-- A folder so named is complete: releases are unpacked beside it, under the
-- name with 'partialSuffix', and renamed when done.
module TrackKat.Release (
    -- * Versions
    Version,
    versionNumbers,
    parseVersion,
    renderVersion,
    describeVersion,
    versionFolder,
    folderVersion,

    -- * Release metadata
    Release (..),
    parseRelease,
    Location (..),
    parseLocation,
    releaseLocation,
    sameDigest,

    -- * Files
    releaseFiles,
    deletionOrder,
    settingsFiles,
    partialSuffix,
    extractRelease,

    -- * Decisions
    Placement (..),
    placement,
    Offer (..),
    offer,
    newerInstalled,
    staleFolders,
    Leftover (..),
    leftover,
) where

import Control.Exception (IOException, try)
import Control.Monad (forM_, unless)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Char (chr, digitToInt, isAlphaNum, isDigit, isHexDigit, isSpace, toLower)
import Data.List (intercalate, isPrefixOf, isSuffixOf, sortOn)
import Data.Maybe (listToMaybe)
import Data.Ord (Down (..))
import System.Directory (createDirectory)
import System.FilePath (dropTrailingPathSeparator, equalFilePath, isAbsolute, splitFileName, takeDirectory, (</>))

import Codec.Archive.Zip (Entry (..), fromEntry, toArchiveOrFail, zEntries)

-- | A release's version: numbers compared in order, as 0.1.123, and what
-- built it, as a commit, which comparisons ignore.
data Version = Version [Int] (Maybe String)
    deriving (Show)

instance Eq Version where
    a == b = versionNumbers a == versionNumbers b

instance Ord Version where
    compare a b = compare (versionNumbers a) (versionNumbers b)

versionNumbers :: Version -> [Int]
versionNumbers (Version ns _) = ns

-- | Read a version such as @0.1.123@ or @0.1.123+abc1234@.
parseVersion :: String -> Maybe Version
parseVersion s = case break (== '+') s of
    (numbers, rest) -> Version <$> traverse number (splitOn '.' numbers) <*> build rest
  where
    number n
        | not (null n), all isDigit n, length n <= 9 = Just (read n)
        | otherwise = Nothing
    build "" = Just Nothing
    build ('+' : b)
        | not (null b), all (\c -> isAlphaNum c || c `elem` ".-") b = Just (Just b)
    build _ = Nothing

renderVersion :: Version -> String
renderVersion v@(Version _ b) = versionFolder v ++ maybe "" ('+' :) b

-- | A version as trackkat's window and log show it, as @0.1.123 (abc1234)@.
describeVersion :: Version -> String
describeVersion v@(Version _ b) = versionFolder v ++ maybe "" (\c -> " (" ++ c ++ ")") b

-- | The name of the folder a version is installed in: its numbers.
versionFolder :: Version -> String
versionFolder (Version ns _) = intercalate "." (map show ns)

-- | The version installed in a folder of the install root, by its name.
folderVersion :: String -> Maybe Version
folderVersion name = case parseVersion name of
    Just v@(Version _ Nothing) -> Just v
    _ -> Nothing

-- | What is published with each release, as @trackkat-windows.json@: its
-- version, where its zip is and the zip's SHA-256, in hex.
data Release = Release
    { releaseVersion :: Version
    , releaseUrl :: String
    , releaseSha256 :: String
    }
    deriving (Eq, Show)

-- | Read a release's metadata: a JSON object with the strings @version@,
-- @url@ and @sha256@. Anything else in it is ignored.
parseRelease :: String -> Either String Release
parseRelease text = do
    fields <- case parseJson text of
        Just (JObject fs) -> Right fs
        _ -> Left "the release metadata isn't a JSON object"
    let field name = case lookup name fields of
            Just (JString s) -> Right s
            _ -> Left ("the release metadata has no " ++ name)
    version <- field "version" >>= maybe (Left "the release metadata's version can't be read") Right . parseVersion
    url <- field "url"
    sha <- field "sha256"
    unless (length sha == 64 && all isHexDigit sha) $ Left "the release metadata's sha256 isn't 64 hex digits"
    Right (Release version url (map toLower sha))

-- | Whether two SHA-256 digests in hex are the same.
sameDigest :: String -> String -> Bool
sameDigest a b = map toLower a == map toLower b

-- | Where metadata or a zip is fetched from: the web, over HTTPS, or, for
-- testing, a file.
data Location = Web String | LocalFile FilePath
    deriving (Eq, Show)

-- | Where a URL or path given for the metadata points.
parseLocation :: String -> Either String Location
parseLocation s
    | "https://" `isPrefixOf` map toLower s = Right (Web s)
    | "http://" `isPrefixOf` map toLower s = Left ("not HTTPS: " ++ s)
    | isAbsolute s = Right (LocalFile s)
    | otherwise = Left ("neither an HTTPS URL nor a full path: " ++ s)

-- | Where a release's zip is, given where its metadata came from: metadata
-- from the web gives the zip's HTTPS URL, and metadata from a file, for
-- testing, a file, perhaps relative to the metadata's folder.
releaseLocation :: Location -> String -> Either String Location
releaseLocation (Web _) url = case parseLocation url of
    Right (Web u) -> Right (Web u)
    _ -> Left ("the release's zip isn't at an HTTPS URL: " ++ url)
releaseLocation (LocalFile meta) path
    | "https://" `isPrefixOf` map toLower path || "http://" `isPrefixOf` map toLower path =
        Left ("metadata from a file names a zip on the web: " ++ path)
    | otherwise = Right (LocalFile (takeDirectory meta </> path))

-- | The files of a release, as its zip has them and as installing copies
-- them from a folder it was unzipped to. CI checks the zip against this.
releaseFiles :: [FilePath]
releaseFiles =
    [ "trackkat.exe"
    , "trackkat-tray.exe"
    , "tirprobe.exe"
    , "NPClient.dll"
    , "NPClient64.dll"
    , "libusb-1.0.dll"
    , "npgame.exe"
    , "trackdemo.exe"
    , "README.md"
    ]

-- | The order to delete a version folder's files in: the NPClient DLLs
-- first, which games keep loaded, and trackkat.exe last, so that while a
-- game holds a DLL, the folder keeps the trackkat.exe that DLL starts,
-- which hands over to the newest.
deletionOrder :: [FilePath] -> [FilePath]
deletionOrder = sortOn rank
  where
    rank f = case map toLower f of
        "npclient64.dll" -> 0 :: Int
        "npclient.dll" -> 1
        "trackkat.exe" -> 3
        _ -> 2

-- | The files trackkat keeps settings and what it learns in, in
-- @%APPDATA%\\trackkat@.
settingsFiles :: [FilePath]
settingsFiles = ["trackkat.conf", "trackkat.calibration"]

-- | Added to a version's folder name while it is being unpacked.
partialSuffix :: String
partialSuffix = ".partial"

-- | Unpack a release's zip into a new folder. The zip holds files only, no
-- folders, and must hold trackkat.exe.
extractRelease :: FilePath -> FilePath -> IO (Either String ())
extractRelease zipFile dest =
    -- Read whole, as a file read lazily stays open on Windows until it is
    -- read to its end.
    try (BL.fromStrict <$> B.readFile zipFile) >>= \case
        Left e -> pure (Left (show (e :: IOException)))
        Right bytes -> case toArchiveOrFail bytes of
            Left err -> pure (Left ("not a zip: " ++ err))
            Right archive -> do
                let names = map eRelativePath (zEntries archive)
                case filter (not . plainName) names of
                    bad : _ -> pure (Left ("the zip holds " ++ show bad ++ ", which isn't a plain file name"))
                    []
                        | "trackkat.exe" `notElem` names -> pure (Left "the zip has no trackkat.exe")
                        | otherwise ->
                            try
                                ( do
                                    createDirectory dest
                                    forM_ (zEntries archive) $ \e -> BL.writeFile (dest </> eRelativePath e) (fromEntry e)
                                )
                                >>= \case
                                    Left e -> pure (Left (show (e :: IOException)))
                                    Right () -> pure (Right ())
  where
    plainName n = not (null n) && n `notElem` [".", ".."] && all (`notElem` "/\\:") n

-- | Where trackkat runs from: an installed version's folder, or anywhere
-- else.
data Placement = Installed Version | Portable
    deriving (Eq, Show)

-- | Where trackkat runs from, given the install root and its own folder.
placement :: FilePath -> FilePath -> Placement
placement root dir
    | equalFilePath (dropTrailingPathSeparator parent) (dropTrailingPathSeparator root)
    , Just v <- folderVersion name =
        Installed v
    | otherwise = Portable
  where
    (parent, name) = splitFileName (dropTrailingPathSeparator dir)

-- | What trackkat's window offers.
data Offer
    = NoOffer
    | -- | to install this copy, which is of this version
      OfferInstall Version
    | -- | to switch to the installed copy, of this version, no older
      -- than this one
      OfferInstalled Version
    | -- | to restart into the newer version, installed and ready
      OfferUpdate Version
    deriving (Eq, Show)

-- | What to offer, given this copy's version (none for a development
-- build), where it runs from and the versions installed.
offer :: Maybe Version -> Placement -> [Version] -> Offer
offer Nothing _ _ = NoOffer
offer (Just self) Portable installed = case newest installed of
    Just v | v >= self -> OfferInstalled v
    _ -> OfferInstall self
offer (Just _) p installed = maybe NoOffer OfferUpdate (newerInstalled p installed)

-- | An installed version newer than the one running from its folder: the
-- one to hand over to.
newerInstalled :: Placement -> [Version] -> Maybe Version
newerInstalled (Installed own) installed = case newest installed of
    Just v | v > own -> Just v
    _ -> Nothing
newerInstalled Portable _ = Nothing

newest :: [Version] -> Maybe Version
newest = listToMaybe . sortOn Down

-- | Which of the install root's entries to delete, given where trackkat
-- runs from: the folders of older versions, and unpacking left unfinished.
-- Only the newest installed version cleans up.
staleFolders :: Placement -> [String] -> [String]
staleFolders Portable _ = []
staleFolders (Installed own) names = filter stale names
  where
    stale n = partialSuffix `isSuffixOf` n || maybe False (< own) (folderVersion n)

-- | What to do with a settings file found beside trackkat.exe, as older
-- versions kept them.
data Leftover
    = -- | move it to @%APPDATA%\\trackkat@
      MoveToSettings
    | -- | leave it, unused, since @%APPDATA%\\trackkat@ has one
      IgnoreBeside
    deriving (Eq, Show)

-- | What to do with a settings file, given whether it is beside
-- trackkat.exe and whether it is in @%APPDATA%\\trackkat@.
leftover :: Bool -> Bool -> Maybe Leftover
leftover False _ = Nothing
leftover True False = Just MoveToSettings
leftover True True = Just IgnoreBeside

-- * A small JSON reader, for the release metadata

data Json = JObject [(String, Json)] | JArray [Json] | JString String | JNumber String | JBool Bool | JNull

parseJson :: String -> Maybe Json
parseJson s = case value (dropWhile isSpace s) of
    Just (j, rest) | all isSpace rest -> Just j
    _ -> Nothing
  where
    value = \case
        '{' : rest -> members [] (skip rest)
        '[' : rest -> elements [] (skip rest)
        '"' : rest -> (\(str, r) -> (JString str, r)) <$> string rest
        't' : 'r' : 'u' : 'e' : rest -> Just (JBool True, rest)
        'f' : 'a' : 'l' : 's' : 'e' : rest -> Just (JBool False, rest)
        'n' : 'u' : 'l' : 'l' : rest -> Just (JNull, rest)
        str@(c : _) | c == '-' || isDigit c -> let (n, rest) = span (`elem` "+-.eE0123456789") str in Just (JNumber n, rest)
        _ -> Nothing
    skip = dropWhile isSpace
    members acc = \case
        '}' : rest | null acc -> Just (JObject [], rest)
        '"' : rest -> do
            (k, r1) <- string rest
            r2 <- case skip r1 of
                ':' : r -> Just (skip r)
                _ -> Nothing
            (v, r3) <- value r2
            case skip r3 of
                ',' : r -> members ((k, v) : acc) (skip r)
                '}' : r -> Just (JObject (reverse ((k, v) : acc)), r)
                _ -> Nothing
        _ -> Nothing
    elements acc = \case
        ']' : rest | null acc -> Just (JArray [], rest)
        str -> do
            (v, r1) <- value str
            case skip r1 of
                ',' : r -> elements (v : acc) (skip r)
                ']' : r -> Just (JArray (reverse (v : acc)), r)
                _ -> Nothing
    string = go []
      where
        go acc = \case
            '"' : rest -> Just (reverse acc, rest)
            '\\' : c : rest -> case c of
                'u' | (h, rest') <- splitAt 4 rest, length h == 4, all isHexDigit h -> go (chr (foldl (\n d -> n * 16 + digitToInt d) 0 h) : acc) rest'
                _ -> (\e -> go (e : acc) rest) =<< lookup c escapes
            c : rest | c >= ' ' -> go (c : acc) rest
            _ -> Nothing
        escapes = [('"', '"'), ('\\', '\\'), ('/', '/'), ('b', '\b'), ('f', '\f'), ('n', '\n'), ('r', '\r'), ('t', '\t')]

splitOn :: Char -> String -> [String]
splitOn c str = case break (== c) str of
    (a, []) -> [a]
    (a, _ : rest) -> a : splitOn c rest
