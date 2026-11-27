-- | What the system tray icon shows: its look, tooltip and menu. The icon
-- itself is "TrackKat.Gui.Tray"'s.
module TrackKat.UI.Tray (
    ClipSight (..),
    Holding (..),
    TrayLook (..),
    trayLook,
    trayTip,
    TrayChoice (..),
    Tick (..),
    MenuEntry (..),
    trayMenu,
) where

import Data.List (intercalate)

import TrackKat.Display (CameraState (..), Notice (..), Output (..), Shown (..), Sighting (..))

-- | Whether the last frame showed the clip.
data ClipSight = ClipInSight | ClipOutOfSight
    deriving (Eq, Show)

-- | Whether games see the head move, or a view held by pausing.
data Holding = ViewLive | ViewHeld
    deriving (Eq, Show)

-- | The cat of icons/tray.svg, its eyes glowing in a colour by state: green,
-- amber, grey and red, in this order.
data TrayLook = LookTracking | LookClipLost | LookSearching | LookDown
    deriving (Eq, Show, Enum, Bounded)

trayLook :: Shown -> TrayLook
trayLook s = case shownCamera s of
    Tracking -> case shownSighting s of
        InSight _ _ -> LookTracking
        OutOfSight _ -> LookClipLost
    Searching -> LookSearching
    CameraDown _ -> LookDown

trayTip :: Shown -> String
trayTip s =
    intercalate "\n" $
        ["trackkat", what]
            ++ ["Game connected" | shownGames s > 0]
            ++ ["Paused" | Paused _ <- [shownOutput s]]
            ++ ["Update ready: restart to update" | Just _ <- [offerItem s]]
  where
    what = case trayLook s of
        LookTracking -> "Tracking"
        LookClipLost -> "Clip out of sight"
        LookSearching -> "Looking for the camera"
        LookDown -> "No camera"

-- | Something to do from the tray's menu.
data TrayChoice = OpenChoice | RecenterChoice | PauseChoice | TrimChoice | OfferChoice | QuitChoice
    deriving (Eq, Show, Enum, Bounded)

-- | Whether a menu item has a tick beside it.
data Tick = Ticked | Unticked
    deriving (Eq, Show)

data MenuEntry = MenuItem TrayChoice String Tick | MenuSeparator
    deriving (Eq, Show)

trayMenu :: Shown -> [MenuEntry]
trayMenu s =
    [ MenuItem OpenChoice "&Open trackkat" Unticked
    , MenuItem RecenterChoice "&Recenter" Unticked
    , MenuItem PauseChoice "&Pause" (case shownOutput s of Paused _ -> Ticked; Live -> Unticked)
    , MenuItem TrimChoice "&Trim: make this view forward" Unticked
    , MenuSeparator
    ]
        ++ concat [[MenuItem OfferChoice item Unticked, MenuSeparator] | Just item <- [offerItem s]]
        ++ [MenuItem QuitChoice "&Quit" Unticked]

-- | The menu's item for what the window's masthead offers, if it has one.
offerItem :: Shown -> Maybe String
offerItem s = shownOffer s >>= noticeTray
