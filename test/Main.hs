module Main (main) where

import System.IO (hSetEncoding, stdout, utf8)
import Test.Tasty (defaultMain, testGroup)

import qualified Test.Branch
import qualified Test.Calibration
import qualified Test.Clips
import qualified Test.Config
import qualified Test.Following
import qualified Test.Frame
import qualified Test.Icons
import qualified Test.Level
import qualified Test.Lights
import qualified Test.Match
import qualified Test.Memory
import qualified Test.Mount
import qualified Test.Pose
import qualified Test.Protocol
import qualified Test.Recording
import qualified Test.Release
import qualified Test.Response
import qualified Test.ServedGame
import qualified Test.Shape
import qualified Test.Stats
import qualified Test.Term
import qualified Test.Tracker
import qualified Test.Visibility

-- | The tests in two groups: "unit", quick ones of one piece at a time, and
-- "simulation", of heads and clips in front of the camera, tracked for
-- seconds or minutes, which take most of the time. Either runs alone with
-- tasty's pattern, as @TASTY_PATTERN='$2==\"unit\"'@. What tests say is
-- written as UTF-8, whatever the terminal's encoding, as some of it is
-- trackkat's own words for the user.
main :: IO ()
main = do
    hSetEncoding stdout utf8
    defaultMain $
        testGroup
            "trackkat"
            [ testGroup
                "unit"
                [ Test.Protocol.tests
                , Test.Frame.tests
                , Test.Pose.tests
                , Test.Term.tests
                , Test.Match.tests
                , Test.Tracker.tests
                , Test.Calibration.tests
                , Test.Visibility.tests
                , Test.Response.tests
                , Test.Config.tests
                , Test.Stats.tests
                , Test.Release.tests
                , Test.ServedGame.tests
                , Test.Icons.tests
                ]
            , testGroup
                "simulation"
                [ Test.Tracker.simulations
                , Test.Following.simulations
                , Test.Branch.simulations
                , Test.Lights.simulations
                , Test.Calibration.simulations
                , Test.Level.simulations
                , Test.Shape.simulations
                , Test.Clips.simulations
                , Test.Mount.simulations
                , Test.Visibility.simulations
                , Test.Recording.simulations
                , Test.Memory.simulations
                ]
            ]
