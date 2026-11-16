-- | The hotkeys, which the settings bind to keys and buttons, and what
-- joysticks do.
module TrackKat.Controls (
    Hotkey (..),
    hotkeyBindings,
    boundTo,
    watchedKeys,
    describeBindings,
    KeyEdge (..),
    JoystickEvent (..),
    ButtonNaming,
    nameButtons,
    JoystickEffect (..),
    joystickEffects,
) where

import Text.Printf (printf)

import Data.List (nub)

import TrackKat.Config
import TrackKat.Gesture (Edge)
import TrackKat.Log (Line (..))
import TrackKat.World (Instant, seconds, (.+^))

-- | An action bound to keys and joystick buttons.
data Hotkey = RecenterKey | PauseKey | TrimKey | AimKey | MarkKey
    deriving (Eq, Show, Enum, Bounded)

-- | The keys and buttons bound to each hotkey.
hotkeyBindings :: Config -> Hotkey -> [Binding]
hotkeyBindings c RecenterKey = recenterBindings c
hotkeyBindings c PauseKey = pauseBindings c
hotkeyBindings c TrimKey = trimBindings c
hotkeyBindings c AimKey = aimBindings c
hotkeyBindings c MarkKey = markBindings c

-- | The hotkeys each way of pressing a key or button is bound to.
boundTo :: Config -> KeyOrButton -> [(Gesture, Hotkey)]
boundTo c i = [(boundGesture b, k) | k <- [minBound .. maxBound], b <- hotkeyBindings c k, boundKey b == i]

-- | The keys and buttons with anything bound to them, each once: those
-- whose presses and releases are watched for.
watchedKeys :: Config -> [KeyOrButton]
watchedKeys c = nub [boundKey b | k <- [minBound .. maxBound], b <- hotkeyBindings c k]

-- | The log's line for what each hotkey is bound to.
describeBindings :: Config -> String
describeBindings config =
    printf
        "Recenter: %s   Pause: %s   Trim: %s   Aim: %s   Mark: %s"
        (keysAndButtons (recenterBindings config))
        (keysAndButtons (pauseBindings config))
        (keysAndButtons (trimBindings config))
        (keysAndButtons (aimBindings config))
        (keysAndButtons (markBindings config))

-- | A watched key or button going down or coming up, and when. A key bound
-- with modifiers goes down with just those held, and comes up when the key
-- itself does, whatever is held by then.
data KeyEdge = KeyEdge
    { edgeAt :: Instant
    , edgeWay :: Edge
    , edgeKey :: KeyOrButton
    }
    deriving (Eq, Show)

-- | Bindings as the console lists them.
keysAndButtons :: [Binding] -> String
keysAndButtons [] = "none"
keysAndButtons bs = renderBindings bs

-- | Something that happened to a joystick: it was attached (or found at
-- start), detached, or one of its buttons was pressed.
data JoystickEvent = Connected Joystick | Disconnected Joystick | Pressed Joystick Int
    deriving (Eq, Show)

-- | Until when joystick buttons pressed are named in the log.
newtype ButtonNaming = NamedUntil Instant
    deriving (Eq, Show)

-- | From the time given, name the joystick buttons pressed for a minute,
-- so they can be copied into the settings file; and the line that says so.
nameButtons :: Instant -> (ButtonNaming, Line)
nameButtons now =
    ( NamedUntil (now .+^ seconds 60)
    , ForUser "For the next minute, joystick buttons you press are named here, ready to bind in trackkat.conf."
    )

-- | What a joystick event asks for.
data JoystickEffect
    = JoystickLog Line
    | -- | add the button to the binding list in trackkat's window that
      -- waits for one, if one does
      OfferButton KeyOrButton
    deriving (Eq, Show)

-- | What a joystick event at the time given asks for: joysticks coming and
-- going are logged, and a button pressed is offered to the window, and
-- named in the log while buttons are.
joystickEffects :: ButtonNaming -> Instant -> JoystickEvent -> [JoystickEffect]
joystickEffects (NamedUntil shownUntil) now event = case event of
    Connected j -> [JoystickLog (ForUser ("Joystick connected: " ++ renderJoystick j))]
    Disconnected j -> [JoystickLog (ForUser ("Joystick disconnected: " ++ renderJoystick j))]
    Pressed j n ->
        let b = JoystickButton j n
         in [JoystickLog (ForUser ("Joystick button pressed: " ++ renderKeyOrButton b)) | now < shownUntil]
                ++ [OfferButton b]
