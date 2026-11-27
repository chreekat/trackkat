-- | The log as the window keeps it.
module Test.UI.Log (tests) where

import Test.Tasty (TestTree)

import Test.Check (check, group)
import TrackKat.UI.Log

tests :: TestTree
tests = group "Log" $ do
    let book = foldl (flip record) emptyLog
        (first, mark) = since start (book ["Tracking.", "Recentered."])
    check "a reader is given each line once, in order" $
        first == ["Tracking.", "Recentered."]
            && fst (since mark (record "Paused." (book ["Tracking.", "Recentered."]))) == ["Paused."]
            && null (fst (since mark (book ["Tracking.", "Recentered."])))
    check "a line too long to keep is cut to a quarter of what is kept" $
        map length (fst (since start (record (replicate logMax 'x') emptyLog))) == [logMax `div` 4]
    check "the oldest lines go first, keeping no more than is kept" $
        let lines' = [show i ++ replicate 10000 '.' | i <- [1 .. 40 :: Int]]
            (kept, _) = since start (book lines')
         in not (null kept) && kept == drop (length lines' - length kept) lines' && sum (map length kept) <= logMax
