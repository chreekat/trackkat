-- | The hotkeys' bindings, and what joysticks do.
module Test.Controls (tests) where

import Data.List (isInfixOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Test.Gen
import Test.Instances ()
import TrackKat.Config
import TrackKat.Controls
import TrackKat.Log
import TrackKat.World (clockAt)

tests :: TestTree
tests =
    testGroup
        "Controls"
        [ testProperty "each hotkey has the bindings the settings give it, and the log's line names every one" $
            \(Sayable c) ->
                concatMap (hotkeyBindings c) [minBound .. maxBound] === recenterBindings c ++ pauseBindings c ++ trimBindings c
                    .&&. conjoin
                        [ counterexample (describeBindings c) (renderBinding b `isInfixOf` describeBindings c)
                        | k <- [minBound .. maxBound]
                        , b <- hotkeyBindings c k
                        ]
        , testProperty "a joystick coming or going is logged for the user, and nothing else" $
            \(Button button) (Naming (asked, now)) ->
                let (naming, _) = nameButtons (clockAt asked)
                 in conjoin
                        [ case joystickEffects naming (clockAt now) event of
                            [JoystickLog (ForUser line)] -> counterexample line (renderJoystick j `isInfixOf` line)
                            other -> counterexample (show other) False
                        | JoystickButton j _ <- [button]
                        , event <- [Connected j, Disconnected j]
                        ]
        , testProperty "a button pressed is always offered to the window, and named in the log just for a minute from when naming began" $
            \(Button button) (Naming (asked, now)) ->
                let (naming, _) = nameButtons (clockAt asked)
                    named = [line | JoystickLog (ForUser line) <- effects]
                    effects = concat [joystickEffects naming (clockAt now) (Pressed j n) | JoystickButton j n <- [button]]
                 in [b | OfferButton b <- effects] === [button]
                        .&&. length effects === length named + 1
                        .&&. (if now < asked + 60 then map (renderBinding button `isInfixOf`) named === [True] else named === [])
        ]

-- | A joystick's button.
newtype Button = Button Binding
    deriving (Show)

instance Arbitrary Button where
    arbitrary = Button <$> arbitrary `suchThat` isButton
    shrink (Button button) = Button <$> filter isButton (shrink button)

isButton :: Binding -> Bool
isButton JoystickButton{} = True
isButton Key{} = False

-- | When buttons began to be named, and a time from two minutes before
-- that to two after, in seconds. Simplest named at the first second, and
-- simpler named from it.
newtype Naming = Naming (Double, Double)
    deriving (Show)

instance Arbitrary Naming where
    arbitrary = do
        asked <- choose (0, 1e6)
        since <- oneof [choose (-120, 120), elements [59.999, 60, 60.001]]
        pure (Naming (asked, asked + since))
    shrink (Naming times) = Naming <$> shrinkTo (0, 0) (\(asked, now) -> [(0, now - asked)]) times
