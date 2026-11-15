-- | Releases: versions, the metadata published with each, where they come
-- from, what an installed or portable trackkat offers, and unpacking one.
module Test.Release (tests) where

import Control.Monad (forM_)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import Data.Char (toLower, toUpper)
import Data.Either (isLeft)
import Data.List (intercalate, isPrefixOf, isSuffixOf, sort)
import Data.Maybe (isNothing)
import System.Directory (createDirectory, doesFileExist, getTemporaryDirectory, removeDirectoryRecursive, removePathForcibly)
import System.FilePath (pathSeparator, (</>))
import System.IO (readFile')
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import Test.Tasty.QuickCheck

import Codec.Archive.Zip (addEntryToArchive, emptyArchive, fromArchive, toEntry)

import Test.Check
import Test.Gen (earlier, forAnyOrder, shrinkFrom, shrinkParts, shrinkTo)
import TrackKat.Number (renderNatural)
import TrackKat.Release

tests :: TestTree
tests = group "Release" $ do
    -- A full path, as a temporary folder has: only its name is used.
    let tmp = if pathSeparator == '\\' then "C:\\Users\\zoe\\Temp" else "/tmp/zoe"
    -- Versions: numbers compared in order, what built them ignored.
    let v1 = parseVersion "0.1.9"
        v2 = parseVersion "0.1.10+abc1234"
    check "version read" $ fmap versionNumbers v2 == Just [0, 1, 10]
    check "versions compare by number" $ ((<) <$> v1 <*> v2) == Just True
    check "the commit doesn't count" $ parseVersion "0.1.10" == v2
    check "version rendered" $ fmap renderVersion v2 == Just "0.1.10+abc1234"
    check "version described" $ fmap describeVersion v2 == Just "0.1.10 (abc1234)"
    check "version folder" $ fmap versionFolder v2 == Just "0.1.10"
    check "folder names have no commit" $ isNothing (folderVersion "0.1.10+abc") && folderVersion "0.1.10" == v2
    check "not versions" $ all (isNothing . parseVersion) ["", "dev", "0..1", "1.x", "0.1+", "0.1+a b", "-1.2", "1.2.3.partial"]

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
        vs = mapM parseVersion ["0.1.9", "0.1.10", "0.1.8"]
    check "an installed version's folder" $ fmap Installed v1 == Just (installedAt "0.1.9")
    check "a trailing separator" $ fmap Installed v1 == Just (placement (root ++ [pathSeparator]) (root </> "0.1.9" ++ [pathSeparator]))
    check "a download is portable" $ placement root (tmp </> "Downloads" </> "trackkat-windows") == Portable
    check "the install root itself is portable" $ placement root root == Portable
    check "an unfinished folder is portable" $ installedAt "0.1.9.partial" == Portable
    check "a development build offers nothing" $ offer Nothing Portable [] == NoOffer
    check "portable, nothing installed: install" $ (offer v1 Portable <$> Just []) == fmap OfferInstall v1
    check "portable, older installed: install" $ (offer v2 Portable <$> mapM parseVersion ["0.1.9"]) == fmap OfferInstall v2
    check "portable, as new installed: switch" $ (offer v1 Portable <$> mapM parseVersion ["0.1.9"]) == fmap OfferInstalled v1
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
    tree properties

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

properties :: TestTree
properties =
    testGroup
        "properties"
        [ testProperty "a version is read back as written, numbers and build" $
            \spec@(VersionSpec numbers _) ->
                let text = versionText spec
                 in fmap versionNumbers (parseVersion text) === Just numbers .&&. fmap renderVersion (parseVersion text) === Just text
        , testProperty "versions are in the order of their numbers, whatever built them" $
            \a b ->
                let (va, vb) = (version a, version b)
                 in compare va vb === compare (specNumbers a) (specNumbers b) .&&. (va == vb) === (specNumbers a == specNumbers b)
        , testProperty "a version's folder is named for its numbers alone, and names it" $
            \v ->
                versionFolder (version v) === versionText v{specBuild = Nothing}
                    .&&. folderVersion (versionFolder (version v)) === Just (version v)
                    .&&. (folderVersion (versionText v) == Nothing) === (specBuild v /= Nothing)
        , testProperty "a status from the web means try again later, or that the look failed, never that there is no network" $ \status ->
            let unavailable = status == 404 || (status >= 500 && status <= 599)
             in case httpFailure "https://example.com/x" status of
                    Unavailable said -> property unavailable .&&. property ("https://example.com/x" `isPrefixOf` said)
                    LookFailed said -> property (not unavailable) .&&. property ("https://example.com/x" `isPrefixOf` said)
                    Offline _ -> property False
        , testProperty "an error from Windows means there is no network, or that the look failed, never try again later" $ \code ->
            case windowsFailure "https://example.com/x" code of
                Unavailable _ -> property False
                Offline said -> property ("https://example.com/x" `isPrefixOf` said)
                LookFailed said -> property ("https://example.com/x" `isPrefixOf` said)
        , testProperty "a version installed in the root is installed, and anything else there is portable" $
            \v (Suffix more) ->
                placement root (root </> versionFolder (version v)) === Installed (version v)
                    .&&. (placement root (root </> (versionText v ++ more)) == Portable) === (specBuild v /= Nothing || more /= "")
        , testProperty "a portable copy offers the newest installed if that is no older, and else itself" $
            \self installed ->
                let vs = map version installed
                 in offer (Just (version self)) Portable vs
                        === (if any (>= version self) vs then OfferInstalled (maximum vs) else OfferInstall (version self))
        , testProperty "an installed copy offers the newest installed if that is newer than its folder's, whatever it is itself" $
            \self own installed ->
                let vs = map version installed
                    newer = if any (> version own) vs then Just (maximum vs) else Nothing
                 in offer (Just (version self)) (Installed (version own)) vs === maybe NoOffer OfferUpdate newer
                        .&&. newerInstalled (Installed (version own)) vs === newer
                        .&&. newerInstalled Portable vs === Nothing
                        .&&. offer Nothing (Installed (version own)) vs === NoOffer
        , testProperty "stale folders are the unfinished ones and older versions', never the running one's or a newer's" $
            \own others ->
                forAnyOrder (versionFolder (version own) : "notes" : concat [[versionFolder (version v), versionFolder (version v) ++ partialSuffix] | v <- others]) $ \names ->
                    let stale = staleFolders (Installed (version own)) names
                        older n = maybe False (< version own) (folderVersion n)
                     in stale === filter (\n -> partialSuffix `isSuffixOf` n || older n) names
                            .&&. property (versionFolder (version own) `notElem` stale)
                            .&&. staleFolders Portable names === []
        , testProperty "files are deleted each once, games' DLLs first and trackkat.exe last, the rest in their order" $
            \(Files files) ->
                let ordered = deletionOrder files
                    lower = map (map toLower)
                    dlls = ["npclient64.dll", "npclient.dll"]
                    (first, rest) = span (`elem` dlls) (lower ordered)
                    (middle, final) = break (== "trackkat.exe") rest
                 in conjoin
                        [ sort ordered === sort files
                        , property (not (any (`elem` dlls) rest))
                        , property (all (== "trackkat.exe") final)
                        , first === filter (== "npclient64.dll") first ++ filter (== "npclient.dll") first
                        , filter (`elem` middle) (lower files) === middle
                        ]
        , testProperty "digests are the same whatever their letters' case" $
            \(Digest digest) (LowerDigest other) ->
                property (sameDigest digest (map toUpper digest) && sameDigest (map toLower digest) digest)
                    .&&. sameDigest digest other === (map toLower digest == other)
        ]
  where
    root = if pathSeparator == '\\' then "C:\\Users\\zoe\\Programs\\trackkat" else "/home/zoe/Programs/trackkat"

-- | A version as what to write it from: its numbers, and what built it.
data VersionSpec = VersionSpec {specNumbers :: [Int], specBuild :: Maybe String}
    deriving (Show)

versionText :: VersionSpec -> String
versionText (VersionSpec numbers build) = intercalate "." (map (renderNatural . toInteger) numbers) ++ maybe "" ('+' :) build

-- | The version a spec writes.
version :: VersionSpec -> Version
version spec = maybe (error ("not a version: " ++ versionText spec)) id (parseVersion (versionText spec))

-- | One to four numbers, small or up to the nine digits a version's number
-- has at most, and perhaps a commit. Simplest the one number nought,
-- built by nothing; and simpler of fewer and smaller numbers.
instance Arbitrary VersionSpec where
    arbitrary = VersionSpec <$> numbers <*> oneof [pure Nothing, Just <$> listOf1 (elements "0123456789abcdef.-")]
      where
        numbers = chooseInt (1, 4) >>= \n -> vectorOf n (oneof [chooseInt (0, 3), chooseInt (0, 999999999)])
    shrink =
        shrinkParts
            [ \(VersionSpec numbers build) -> [VersionSpec numbers Nothing | build /= Nothing]
            , \(VersionSpec numbers build) -> [VersionSpec numbers' build | numbers' <- shrinkTo [0] (filter (not . null) . shrinkList (shrinkFrom 0)) numbers]
            ]

-- | What might follow a version in a folder's name.
newtype Suffix = Suffix String
    deriving (Show)

instance Arbitrary Suffix where
    arbitrary = Suffix <$> elements suffixes
    shrink (Suffix more) = Suffix <$> earlier suffixes more

suffixes :: [String]
suffixes = ["", ".partial", "x", " "]

-- | Some of a release's files and a few others, in any order; simpler of
-- fewer.
newtype Files = Files [FilePath]
    deriving (Show)

instance Arbitrary Files where
    arbitrary = Files <$> (sublistOf (releaseFiles ++ ["notes.txt", "TRACKKAT.EXE", "npclient.DLL"]) >>= shuffle)
    shrink (Files files) = Files <$> shrinkList (const []) files

-- | A digest's hexadecimal digits, of either case; simpler shorter.
newtype Digest = Digest String
    deriving (Show)

instance Arbitrary Digest where
    arbitrary = Digest <$> listOf (elements "0123456789abcdefABCDEF")
    shrink (Digest digits) = Digest <$> shrinkList (const []) digits

-- | A digest's hexadecimal digits in lower case; simpler shorter.
newtype LowerDigest = LowerDigest String
    deriving (Show)

instance Arbitrary LowerDigest where
    arbitrary = LowerDigest <$> listOf (elements "0123456789abcdef")
    shrink (LowerDigest digits) = LowerDigest <$> shrinkList (const []) digits
