-- | The hotkeys, which the settings bind to keys and buttons, and what
-- joysticks do.
module TrackKat.Controls (
    Hotkey (..),
    hotkeyBindings,
    describeBindings,
    JoystickEvent (..),
    ButtonNaming,
    nameButtons,
    JoystickEffect (..),
    joystickEffects,
) where

import Text.Printf (printf)

import TrackKat.Config
import TrackKat.Log (Line (..))

-- | An action bound to keys and joystick buttons.
data Hotkey = RecenterKey | PauseKey | TrimKey
    deriving (Eq, Show, Enum, Bounded)

-- | The keys and buttons bound to each hotkey.
hotkeyBindings :: Config -> Hotkey -> [Binding]
hotkeyBindings c RecenterKey = recenterBindings c
hotkeyBindings c PauseKey = pauseBindings c
hotkeyBindings c TrimKey = trimBindings c

-- | The log's line for what each hotkey is bound to.
describeBindings :: Config -> String
describeBindings config =
    printf
        "Recenter: %s   Pause: %s   Trim: %s"
        (keysAndButtons (recenterBindings config))
        (keysAndButtons (pauseBindings config))
        (keysAndButtons (trimBindings config))

-- | Bindings as the console lists them.
keysAndButtons :: [Binding] -> String
keysAndButtons [] = "none"
keysAndButtons bs = renderBindings bs

-- | Something that happened to a joystick: it was attached (or found at
-- start), detached, or one of its buttons was pressed.
data JoystickEvent = Connected Joystick | Disconnected Joystick | Pressed Joystick Int
    deriving (Eq, Show)

-- | Until when joystick buttons pressed are named in the log, in seconds
-- on the clock 'nameButtons' is given the time by.
newtype ButtonNaming = NamedUntil Double
    deriving (Eq, Show)

-- | From the time given, name the joystick buttons pressed for a minute,
-- so they can be copied into the settings file; and the line that says so.
nameButtons :: Double -> (ButtonNaming, Line)
nameButtons now =
    ( NamedUntil (now + 60)
    , ForUser "For the next minute, joystick buttons you press are named here, ready to bind in trackkat.conf."
    )

-- | What a joystick event asks for.
data JoystickEffect
    = JoystickLog Line
    | -- | add the button to the binding list in trackkat's window that
      -- waits for one, if one does
      OfferButton Binding
    deriving (Eq, Show)

-- | What a joystick event at the time given asks for: joysticks coming and
-- going are logged, and a button pressed is offered to the window, and
-- named in the log while buttons are.
joystickEffects :: ButtonNaming -> Double -> JoystickEvent -> [JoystickEffect]
joystickEffects (NamedUntil shownUntil) now event = case event of
    Connected j -> [JoystickLog (ForUser ("Joystick connected: " ++ renderJoystick j))]
    Disconnected j -> [JoystickLog (ForUser ("Joystick disconnected: " ++ renderJoystick j))]
    Pressed j n ->
        let b = JoystickButton j n
         in [JoystickLog (ForUser ("Joystick button pressed: " ++ renderBinding b)) | now < shownUntil]
                ++ [OfferButton b]
