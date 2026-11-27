-- | The settings as the window keeps them: what the settings file says,
-- to show, and what the user changed in the window, to save.
--
-- Note [The window's changes]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The user can change a setting in the window or in the settings file, and
-- neither undoes the other's changes. Saving hands trackkat just the
-- settings the user changed in the window since they were last saved, and
-- trackkat writes just those into the file, which keeps what it says of the
-- rest. A setting the file pushes to one changed and not yet saved is
-- dropped: the user's change is the newer, and once saved, the file holds it
-- and pushes it back. Every other push is shown, so the window and the file
-- come to agree once neither is changing.
module TrackKat.UI.Settings (
    Desk,
    emptyDesk,
    push,
    takePushes,
    saveValues,
    takeSaved,
    Check (..),
    Toggle (..),
    toggleChecked,
    toggleValue,
) where

-- | Settings by name, each the latest of its name, oldest first.
data Desk = Desk
    { pushes :: [(String, String)]
    -- ^ those the file pushed, not yet shown
    , saves :: [(String, String)]
    -- ^ those the window saved, not yet taken
    }

emptyDesk :: Desk
emptyDesk = Desk [] []

push :: String -> String -> Desk -> Desk
push name value d = d{pushes = pushes d `with` [(name, value)]}

-- | The pushes to show, leaving out those to settings the window changed
-- and hasn't saved. See Note [The window's changes].
takePushes :: [String] -> Desk -> ([(String, String)], Desk)
takePushes changed d = (filter ((`notElem` changed) . fst) (pushes d), d{pushes = []})

saveValues :: [(String, String)] -> Desk -> Desk
saveValues values d = d{saves = saves d `with` values}

-- | The saved settings of some names.
takeSaved :: [String] -> Desk -> ([(String, String)], Desk)
takeSaved names d = (filter ((`elem` names) . fst) (saves d), d{saves = filter ((`notElem` names) . fst) (saves d)})

-- | Settings with some replaced or added at the end.
with :: [(String, String)] -> [(String, String)] -> [(String, String)]
with old new = filter ((`notElem` map fst new) . fst) old ++ new

-- | Whether a check box is ticked.
data Check = Checked | Unchecked
    deriving (Eq, Show)

-- | A setting a check box shows.
data Toggle = Toggle
    { tickedValue :: String
    , untickedValue :: String
    , otherValues :: Check
    -- ^ how any other value shows
    }

toggleChecked :: Toggle -> String -> Check
toggleChecked (Toggle on off other) value
    | value == on = Checked
    | value == off = Unchecked
    | otherwise = other

toggleValue :: Toggle -> Check -> String
toggleValue (Toggle on _ _) Checked = on
toggleValue (Toggle _ off _) Unchecked = off
