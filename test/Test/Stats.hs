-- | The performance window: means, maxima and the summary line.
module Test.Stats (tests) where

import Test.Tasty (TestTree)

import Test.Check
import TrackKat.Stats

tests :: TestTree
tests = group "Stats" $ do
    -- Performance window: means, maxima and the summary line.
    let w = sawGCs 2 0.00015 (sawCorrupt (sawFrame 0.0002 0.009 12 (sawFrame 0.0004 0.008 40 emptyWindow)))
    checkShowing "stats summary" (summary w) $
        summary w == "latency 0.30 ms (max 0.40)  gap max 9 ms  stripes max 40  GC 2 (max 0.15 ms)  corrupt 1"
    check "empty stats" $ summary emptyWindow == "latency 0.00 ms (max 0.00)  gap max 0 ms  stripes max 0  GC 0 (max 0.00 ms)"
