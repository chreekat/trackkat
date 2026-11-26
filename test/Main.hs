module Main (main) where

import System.IO (hSetEncoding, stdout, utf8)
import Test.Tasty (defaultMain, testGroup)

import qualified Test.Belief
import qualified Test.Branch
import qualified Test.Calibration
import qualified Test.Clips
import qualified Test.CommandLine
import qualified Test.Config
import qualified Test.Controls
import qualified Test.Following
import qualified Test.Frame
import qualified Test.Gesture
import qualified Test.Icons
import qualified Test.Idle
import qualified Test.Level
import qualified Test.Lights
import qualified Test.Linear
import qualified Test.Match
import qualified Test.Memory
import qualified Test.Mount
import qualified Test.Number
import qualified Test.Old.Branch
import qualified Test.Old.Clips
import qualified Test.Old.Following
import qualified Test.Old.Gen
import qualified Test.Old.Lights
import qualified Test.Old.Match
import qualified Test.Old.Memory
import qualified Test.Old.Mount
import qualified Test.Old.Shape
import qualified Test.Old.Tracker
import qualified Test.Old.Visibility
import qualified Test.Pose
import qualified Test.Protocol
import qualified Test.Recording
import qualified Test.Release
import qualified Test.Response
import qualified Test.Scorecard
import qualified Test.ServedGame
import qualified Test.Session
import qualified Test.Shape
import qualified Test.Shrink
import qualified Test.Stats
import qualified Test.Term
import qualified Test.Tracker
import qualified Test.Trackers
import qualified Test.UI.Curve
import qualified Test.UI.Masthead
import qualified Test.UI.Tray
import qualified Test.Updater
import qualified Test.Visibility
import qualified Test.World

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
                [ Test.Shrink.tests
                , Test.Protocol.tests
                , Test.Frame.tests
                , Test.World.tests
                , Test.Linear.tests
                , Test.Pose.tests
                , Test.Term.tests
                , Test.Match.tests
                , Test.Belief.tests
                , Test.Lights.tests
                , Test.Tracker.tests
                , Test.Trackers.tests
                , Test.UI.Curve.tests
                , Test.UI.Masthead.tests
                , Test.UI.Tray.tests
                , Test.Calibration.tests
                , Test.Visibility.tests
                , Test.Response.tests
                , Test.Number.tests
                , Test.Config.tests
                , Test.Recording.properties
                , Test.Stats.tests
                , Test.Release.tests
                , Test.ServedGame.tests
                , Test.CommandLine.tests
                , Test.Gesture.tests
                , Test.Controls.tests
                , Test.Session.tests
                , Test.Updater.tests
                , Test.Idle.tests
                , Test.Icons.tests
                , testGroup
                    "the old tracker"
                    [ Test.Old.Gen.shrinks
                    , Test.Old.Match.tests
                    , Test.Old.Tracker.tests
                    ]
                ]
            , testGroup
                "simulation"
                [ Test.Tracker.simulations
                , Test.Trackers.simulations
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
                , Test.Scorecard.simulations
                , testGroup
                    "the old tracker"
                    [ Test.Old.Tracker.simulations
                    , Test.Old.Following.simulations
                    , Test.Old.Branch.simulations
                    , Test.Old.Lights.simulations
                    , Test.Old.Shape.simulations
                    , Test.Old.Clips.simulations
                    , Test.Old.Mount.simulations
                    , Test.Old.Visibility.simulations
                    , Test.Old.Memory.simulations
                    ]
                ]
            ]
