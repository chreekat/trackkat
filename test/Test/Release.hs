-- | Releases: versions, the metadata published with each, where they come
-- from, what an installed or portable trackkat offers, and unpacking one.
module Test.Release (tests) where

import Control.Monad (forM_)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import Data.Either (isLeft)
import Data.Maybe (isNothing)
import System.Directory (createDirectory, doesFileExist, getTemporaryDirectory, removeDirectoryRecursive, removePathForcibly)
import System.FilePath (pathSeparator, (</>))
import System.IO (readFile')
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, testCase)

import Codec.Archive.Zip (addEntryToArchive, emptyArchive, fromArchive, toEntry)

import Test.Check
import TrackKat.Release

tests :: TestTree
tests = group "Release" $ do
    -- A full path, as a temporary folder has: only its name is used.
    let tmp = if pathSeparator == '\\' then "C:\\Users\\zoe\\Temp" else "/tmp/zoe"
    -- Versions: numbers compared in order, what built them ignored.
    let version = parseVersion
        v1 = version "0.1.9"
        v2 = version "0.1.10+abc1234"
    check "version read" $ fmap versionNumbers v2 == Just [0, 1, 10]
    check "versions compare by number" $ ((<) <$> v1 <*> v2) == Just True
    check "the commit doesn't count" $ version "0.1.10" == v2
    check "version rendered" $ fmap renderVersion v2 == Just "0.1.10+abc1234"
    check "version described" $ fmap describeVersion v2 == Just "0.1.10 (abc1234)"
    check "version folder" $ fmap versionFolder v2 == Just "0.1.10"
    check "folder names have no commit" $ isNothing (folderVersion "0.1.10+abc") && folderVersion "0.1.10" == v2
    check "not versions" $ all (isNothing . version) ["", "dev", "0..1", "1.x", "0.1+", "0.1+a b", "-1.2", "1.2.3.partial"]

    -- The metadata published with each release.
    let sha = replicate 63 'a' ++ "B"
        meta = "{\"version\": \"0.1.10+abc1234\", \"url\": \"https://example.com/t\\u0072ackkat-windows.zip\",\n \"sha256\": \"" ++ sha ++ "\", \"size\": 123, \"notes\": [true, null]}"
    case parseRelease meta of
        Right r -> do
            check "release version" $ Just (releaseVersion r) == v2
            check "release url, unescaped" $ releaseUrl r == "https://example.com/trackkat-windows.zip"
            check "release digest, lower case" $ releaseSha256 r == replicate 63 'a' ++ "b"
        Left err -> checkShowing "release metadata" (err) False
    check "metadata missing a field" $ isLeft (parseRelease "{\"version\": \"0.1.2\", \"url\": \"https://x\"}")
    check "metadata with a bad digest" $ isLeft (parseRelease ("{\"version\": \"0.1.2\", \"url\": \"https://x\", \"sha256\": \"" ++ replicate 64 'g' ++ "\"}"))
    check "metadata that isn't JSON" $ all (isLeft . parseRelease) ["", "[1]", "{\"version\": }", "{\"a\": 1} x"]
    check "digests compare ignoring case" $ sameDigest "ABC" "abc" && not (sameDigest "abc" "abd")
    check "nothing published yet passes" $ httpFailure "u" 404 == Unavailable "u: HTTP status 404"
    check "server errors pass" $ all (\s -> httpFailure "u" s == Unavailable ("u: HTTP status " ++ show s)) [500, 502, 503, 599]
    check "other statuses don't" $ all (\s -> httpFailure "u" s == LookFailed ("u: HTTP status " ++ show s)) [400, 403, 410, 600]
    check "no network is offline" $ all (\c -> windowsFailure "u" c == Offline ("u: Windows error " ++ show c)) [12002, 12007, 12029, 12030, 12031]
    check "other Windows errors aren't" $ windowsFailure "u" 12175 == LookFailed "u: Windows error 12175"

    check "a full path" $ parseLocation (tmp </> "x.json") == Right (LocalFile (tmp </> "x.json"))
    check "https metadata" $ parseLocation "https://github.com/x.json" == Right (Web "https://github.com/x.json")
    check "plain http refused" $ isLeft (parseLocation "http://github.com/x.json")
    check "relative path refused" $ isLeft (parseLocation "x.json")
    check "metadata from the web names a zip on the web" $
        releaseLocation (Web "https://a/x.json") "https://b/x.zip" == Right (Web "https://b/x.zip")
            && isLeft (releaseLocation (Web "https://a/x.json") "/etc/passwd")
    check "metadata from a file names a file beside it" $
        releaseLocation (LocalFile (tmp </> "x.json")) "x.zip" == Right (LocalFile (tmp </> "x.zip"))
            && isLeft (releaseLocation (LocalFile (tmp </> "x.json")) "https://b/x.zip")

    -- Installed or not, and what the window offers.
    let root = tmp </> "Programs" </> "trackkat"
        installedAt v = placement root (root </> v)
        vs = mapM version ["0.1.9", "0.1.10", "0.1.8"]
    check "an installed version's folder" $ fmap Installed v1 == Just (installedAt "0.1.9")
    check "a trailing separator" $ fmap Installed v1 == Just (placement (root ++ [pathSeparator]) (root </> "0.1.9" ++ [pathSeparator]))
    check "a download is portable" $ placement root (tmp </> "Downloads" </> "trackkat-windows") == Portable
    check "the install root itself is portable" $ placement root root == Portable
    check "an unfinished folder is portable" $ installedAt "0.1.9.partial" == Portable
    check "a development build offers nothing" $ offer Nothing Portable [] == NoOffer
    check "portable, nothing installed: install" $ (offer v1 Portable <$> Just []) == fmap OfferInstall v1
    check "portable, older installed: install" $ (offer v2 Portable <$> mapM version ["0.1.9"]) == fmap OfferInstall v2
    check "portable, as new installed: switch" $ (offer v1 Portable <$> mapM version ["0.1.9"]) == fmap OfferInstalled v1
    check "portable, newer installed: switch" $ (offer v1 Portable <$> vs) == fmap OfferInstalled v2
    check "installed, newest: nothing" $ (offer v2 (installedAt "0.1.10") <$> vs) == Just NoOffer
    check "installed, newer ready: update" $ (offer v1 (installedAt "0.1.9") <$> vs) == fmap OfferUpdate v2
    check "a build in a newer version's folder doesn't update to it" $ (offer v1 (installedAt "0.1.10") <$> vs) == Just NoOffer
    check "an old version hands over to the newest" $ (newerInstalled (installedAt "0.1.8") <$> vs) == Just v2
    check "the newest doesn't hand over" $ (newerInstalled (installedAt "0.1.10") <$> vs) == Just Nothing
    check "a portable copy doesn't hand over" $ (newerInstalled Portable <$> vs) == Just Nothing

    -- Cleaning up older versions, games' DLLs first.
    let names = ["0.1.8", "0.1.9", "0.1.10", "0.1.11", "0.1.11.partial", "notes", "0.1.7+x"]
    check "stale folders" $ staleFolders (installedAt "0.1.10") names == ["0.1.8", "0.1.9", "0.1.11.partial"]
    check "portable copies clean nothing" $ null (staleFolders Portable names)
    check "deletion order" $
        deletionOrder ["README.md", "trackkat.exe", "NPClient.dll", "libusb-1.0.dll", "NPClient64.dll"]
            == ["NPClient64.dll", "NPClient.dll", "README.md", "libusb-1.0.dll", "trackkat.exe"]
    check "every release file has a place" $ length (deletionOrder releaseFiles) == length releaseFiles

    -- Settings files left beside trackkat.exe by older versions.
    check "settings moved" $ leftover True False == Just MoveToSettings
    check "settings left when both" $ leftover True True == Just IgnoreBeside
    check "no settings beside" $ isNothing (leftover False True) && isNothing (leftover False False)
    tree unpacking

-- | Unpacking a release's zip: a good one unpacks, and one that would write
-- outside its folder, has folders, lacks trackkat.exe or isn't a zip is
-- refused, nothing written outside.
unpacking :: TestTree
unpacking = testCase "unpacking a release's zip" $ do
    tmp <- getTemporaryDirectory
    let work = tmp </> "trackkat-test-unzip"
        zipOf files = fromArchive (foldr (addEntryToArchive . (\(n, b) -> toEntry n 0 (BL.fromStrict (C.pack b)))) emptyArchive files)
        unpack name files = do
            BL.writeFile (work </> (name ++ ".zip")) (zipOf files)
            extractRelease (work </> (name ++ ".zip")) (work </> name)
    removePathForcibly work
    createDirectory work
    good <- unpack "good" [("trackkat.exe", "exe"), ("README.md", replicate 5000 'x')]
    exe <- readFile' (work </> "good" </> "trackkat.exe")
    readme <- readFile' (work </> "good" </> "README.md")
    assertBool ("release unpacked " ++ show good) $ good == Right () && exe == "exe" && readme == replicate 5000 'x'
    refused <-
        mapM
            (\(n, fs) -> (,) n <$> unpack n fs)
            [ ("escape", [("trackkat.exe", "x"), ("../evil.exe", "x")])
            , ("folders", [("trackkat.exe", "x"), ("sub/a.dll", "x")])
            , ("backslash", [("trackkat.exe", "x"), ("..\\evil.exe", "x")])
            , ("no-exe", [("README.md", "x")])
            ]
    forM_ refused $ \(n, r) -> assertBool ("zip refused: " ++ n) (isLeft r)
    escaped <- doesFileExist (tmp </> "evil.exe")
    assertBool "nothing written outside" (not escaped)
    B.writeFile (work </> "junk.zip") (C.pack "not a zip")
    junk <- extractRelease (work </> "junk.zip") (work </> "junk")
    assertBool "a file that isn't a zip" (isLeft junk)
    removeDirectoryRecursive work
