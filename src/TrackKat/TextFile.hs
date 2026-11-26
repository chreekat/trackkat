-- | The text files trackkat keeps and reads, as its settings: UTF-8,
-- whatever Windows' code page, with lines ended as the system ends them.
module TrackKat.TextFile (
    readText,
    writeText,
) where

import System.IO (IOMode (..), hGetContents', hPutStr, hSetEncoding, utf8, utf8_bom, withFile)

-- | Read a file whole, skipping a byte-order mark at its start, as some
-- editors write.
readText :: FilePath -> IO String
readText path = withFile path ReadMode $ \h -> hSetEncoding h utf8_bom >> hGetContents' h

writeText :: FilePath -> String -> IO ()
writeText path text = withFile path WriteMode $ \h -> hSetEncoding h utf8 >> hPutStr h text
