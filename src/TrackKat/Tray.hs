-- | What the system tray icon shows: its look, tooltip and menu. The icon
-- itself is "TrackKat.Gui.Tray"'s.
module TrackKat.Tray (
    TrayState (..),
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

import TrackKat.Display (CameraState (..))

-- | What the tray tells of trackkat.
data TrayState = TrayState
    { trayCamera :: CameraState
    , trayClip :: ClipSight
    , trayGames :: Int
    -- ^ games tracking
    , trayHolding :: Holding
    , trayOffer :: Maybe String
    -- ^ the menu's item for what the window's strip offers, if it has one
    }
    deriving (Eq, Show)

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

trayLook :: TrayState -> TrayLook
trayLook s = case trayCamera s of
    Tracking -> case trayClip s of
        ClipInSight -> LookTracking
        ClipOutOfSight -> LookClipLost
    Searching -> LookSearching
    CameraDown _ -> LookDown

trayTip :: TrayState -> String
trayTip s =
    intercalate "\n" $
        ["trackkat", what]
            ++ ["Game connected" | trayGames s > 0]
            ++ ["Paused" | trayHolding s == ViewHeld]
            ++ ["Update ready: restart to update" | Just _ <- [trayOffer s]]
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

trayMenu :: TrayState -> [MenuEntry]
trayMenu s =
    [ MenuItem OpenChoice "&Open trackkat" Unticked
    , MenuItem RecenterChoice "&Recenter" Unticked
    , MenuItem PauseChoice "&Pause" (if trayHolding s == ViewHeld then Ticked else Unticked)
    , MenuItem TrimChoice "&Trim: make this view forward" Unticked
    , MenuSeparator
    ]
        ++ concat [[MenuItem OfferChoice item Unticked, MenuSeparator] | Just item <- [trayOffer s]]
        ++ [MenuItem QuitChoice "&Quit" Unticked]
