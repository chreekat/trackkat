-- | Matching a frame's blobs to the clip's reflectors.
module Test.Match (tests) where

import Test.Tasty (TestTree)

import Test.Check
import Test.Simulation (bentWorn, clipBent, headSeen)

import TrackKat.Match (Match (..), matchScore, matches, noSize)
import TrackKat.Pose (defaultIntrinsics)
import TrackKat.Synth
import TrackKat.Term (Finding (..), Name (..), Score (..))
import qualified TrackKat.Term as Term
import TrackKat.Tracker (findBlobs)

tests :: TestTree
tests = group "Match" $ do
    -- A match's cost is what its terms come to.
    let face = headSeen bentWorn 0 0 0
        blobsOf = findBlobs . renderDiscs
        found = matches defaultIntrinsics clipBent face (pure noSize) (blobsOf (((340, 288), 3) : pointDiscs defaultIntrinsics clipBent allLit (headSeen bentWorn 3 1 0)))
    checkShowing "a match's terms are its cost" (show (map (Term.renderFindings . matchJudged) (take 2 found))) $
        not (null found)
            && and [abs (sum (map (nats . findingScore) (matchJudged m)) - nats (matchScore m)) < 1e-9 && nats (matchScore m) == negate (matchCost m) / 2 | m <- found]
            && map (map (brief . findingName) . matchJudged) (take 1 found) == [["places", "sizes", "unseen"]]
