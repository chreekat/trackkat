-- | The icons in the repository.
module Test.Icons (tests) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

tests :: TestTree
tests =
    testGroup
        "Icons"
        [ -- The tray's icons are made by tools/make-icon.sh.
          testCase "the tray's cat is the program's" $ do
            tray <- B.readFile "icons/tray.svg"
            program <- B.readFile "icons/trackkat.svg"
            assertBool "" (not (B.null (catPath tray)) && catPath tray == catPath program)
        ]
  where
    catPath svg = B.takeWhile (/= fromIntegral (fromEnum '"')) (B.drop 4 (snd (B.breakSubstring (C.pack " d=\"") svg)))
