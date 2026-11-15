-- | When an installed trackkat looks for a newer version, and what it says
-- of what it finds. "TrackKat.Install" does the looking, on Windows.
--
-- See Note [What looking for updates promises].
module TrackKat.Updater (
    Updater,
    newUpdater,
    Looking (..),
    updateEvery,
    dueLook,
    Effect (..),
    begun,
    answered,
    offerNotice,
) where

import TrackKat.Config (Updates (..))
import TrackKat.Display (Notice (..), UpdateStatus (..))
import TrackKat.Log (Line (..))
import TrackKat.Release

-- Note [What looking for updates promises]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- * Unasked, trackkat looks only while updates are on, and not within

--   'updateEvery' of a look that reached GitHub, or of one the user asked
--   for, whatever that one found.
--

-- * A look that finds no network is tried again the next time one could be

--   due, and unasked tells the user nothing: no line in the window's log,
--   and nothing in its strip.
--

-- * A look the user asked for always answers, in the log and in the strip.

--

-- * A failure that lasts is logged for the user once, by the first unasked

--   look to meet it, until a look works; the terminal has it every time.
--
-- "Test.Updater" states these as properties.

-- | What trackkat remembers of its looks for a newer version.
data Updater = Updater
    { uLooked :: Maybe Double
    -- ^ when it last looked, in seconds, counting only looks that put off
    -- the next: see 'answered'
    , uFailure :: FailureLogged
    }
    deriving (Eq, Show)

-- | Whether the log has said a look for a newer version failed, since one
-- last worked.
data FailureLogged = FailureLogged | NoFailureLogged
    deriving (Eq, Show)

-- | Having never looked.
newUpdater :: Updater
newUpdater = Updater Nothing NoFailureLogged

-- | Why trackkat looks for a newer version.
data Looking
    = -- | 'updateEvery' has passed, and updates are on
      Due
    | -- | the user clicked Check for updates
      Asked
    deriving (Eq, Show)

-- | How often an installed trackkat looks for a newer version, in seconds.
updateEvery :: Double
updateEvery = 4 * 3600

-- | The look to make unasked at the time given, in seconds on a clock that
-- never goes back, if one is due.
dueLook :: Updates -> Double -> Updater -> Maybe Looking
dueLook UpdatesOff _ _ = Nothing
dueLook UpdatesOn now u = case uLooked u of
    Just looked | now - looked < updateEvery -> Nothing
    _ -> Just Due

-- | Something for trackkat to do about a look.
data Effect
    = Log Line
    | -- | say so in the strip across the top of the window, briefly; the
      -- log has the whole story
      ShowStatus UpdateStatus
    deriving (Eq, Show)

-- | What to do as a look begins.
begun :: Looking -> [Effect]
begun Asked = [Log (ForUser "Looking for a newer trackkat.")]
begun Due = []

-- | Take in what a look found: what to remember, and what to say. Given
-- are this copy's version, why it looked, the time the wait for the next
-- look runs from, what the look found, and what the window offers now
-- that it has looked.
--
-- A look puts off the next unless it was unasked and found no network, as
-- just after waking from sleep.
answered :: Version -> Looking -> Double -> Either LookFailure (Maybe Version) -> Offer -> Updater -> (Updater, [Effect])
answered self why now found waiting (Updater _ logged) = case (found, waiting) of
    -- Offline, or behind a proxy that won't let it through: the log
    -- says so when asked, and otherwise once until a look works, so
    -- a lasting failure shows; the terminal, if any, every time.
    -- Offline, as just after waking from sleep, is no news unless
    -- asked: the next round tries again.
    (Left (Offline err), _) ->
        let line = "Can't reach GitHub to look for a newer trackkat; is the network up? (" ++ err ++ ")"
         in case why of
                Due -> (Updater Nothing logged, [Log (ForDeveloper line)])
                Asked -> (Updater (Just now) logged, [Log (ForUser line), ShowStatus (UpdateNote "No network; try later.")])
    (Left (Unavailable err), _) ->
        failed
            ("No update information on GitHub just now (a new build may be being published); try again in a few minutes. (" ++ err ++ ")")
            "Nothing on GitHub now; try later."
    (Left (LookFailed err), _) -> failed ("Can't look for a newer trackkat: " ++ err) "Can't check now: see the log."
    (Right (Just v), _) ->
        (worked, [Log (ForUser (ready v)), ShowStatus (UpdateNote (readyShort v))])
    -- Fetched by an earlier look, and waiting for a restart.
    (Right Nothing, OfferUpdate v) ->
        (worked, [Log (ForUser (ready v)) | why == Asked] ++ [ShowStatus (UpdateNote (readyShort v))])
    (Right Nothing, _) ->
        ( worked
        , [Log (ForUser ("trackkat is up to date (" ++ versionFolder self ++ ").")) | why == Asked]
            ++ [ShowStatus (UpToDate ("Up to date (" ++ versionFolder self ++ ")."))]
        )
  where
    worked = Updater (Just now) NoFailureLogged
    failed line short =
        ( Updater (Just now) FailureLogged
        ,
            [ Log $ case (why, logged) of
                (Due, FailureLogged) -> ForDeveloper line
                _ -> ForUser line
            , ShowStatus (UpdateNote short)
            ]
        )
    ready v = "trackkat " ++ describeVersion v ++ " is ready: restart to update, or it starts next time."
    readyShort v = versionFolder v ++ " is ready."

-- | What the strip across the top of the window says of an offer, given
-- the folder this copy runs from.
offerNotice :: FilePath -> Offer -> Maybe Notice
offerNotice dir o = case o of
    NoOffer -> Nothing
    OfferInstall v ->
        Just
            Notice
                { noticeText = "trackkat " ++ versionFolder v ++ " is running from " ++ dir ++ ". Install it to start it from the Start menu and keep it up to date."
                , noticeButton = "Install trackkat"
                , noticeTray = Nothing
                }
    OfferInstalled v ->
        Just
            Notice
                { noticeText = "trackkat " ++ versionFolder v ++ " is installed; this copy, in " ++ dir ++ ", isn't."
                , noticeButton = "Use installed trackkat"
                , noticeTray = Nothing
                }
    OfferUpdate v ->
        Just
            Notice
                { noticeText = versionFolder v ++ " is ready. It starts next time trackkat does."
                , noticeButton = "Restart to update"
                , noticeTray = Just ("Restart to update to " ++ versionFolder v)
                }
