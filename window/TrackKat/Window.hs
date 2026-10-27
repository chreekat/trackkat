{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | trackkat's window: the head and status, live; buttons for what the
-- hotkeys do; the calibration; the settings, saved to trackkat.conf; and
-- the log. Dear ImGui draws it with OpenGL 3 in a GLFW window.
--
-- 'runWindow' runs it on a thread of its own. Other threads hand it the
-- status, log lines and settings, which never waits on the window, and
-- take what the user asks for with 'takeActions'.
module TrackKat.Window (
    Window,
    newWindow,
    Visibility (..),
    WindowFailure (..),
    runWindow,
    openWindow,
    windowOpen,
    updateStatus,
    currentStatus,
    logLine,
    showSettings,
    showSettingsMessage,
    Action (..),
    takeActions,
    capturing,
    addBinding,
    stopCapturing,
) where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (bracket_)
import Control.Monad (forM_, forever, unless, when)
import Data.Bits ((.|.))
import Data.Foldable (toList)
import Data.IORef
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.StateVar (StateVar, makeStateVar)
import Data.Text (Text)
import qualified Data.Text as T
import Foreign.Ptr (nullPtr)
import Text.Printf (printf)

import DearImGui hiding (end, flags, size, spacing, w, x, y, z)
import DearImGui.GLFW (glfwNewFrame)
import DearImGui.GLFW.OpenGL (glfwInitForOpenGL)
import DearImGui.Internal.Text (withCString)
import DearImGui.OpenGL3 (openGL3Init, openGL3NewFrame, openGL3RenderDrawData)
import qualified DearImGui.Raw as Raw
import qualified DearImGui.Raw.IO as Raw.IO
import qualified Graphics.UI.GLFW as GLFW

import TrackKat.Config (Binding, Hotkey (..), hotkeySetting, renderBinding)
import TrackKat.Number (parseNumber)
import TrackKat.Pose (HeadPose)
import TrackKat.Response (Trim (..))
import TrackKat.Status
import TrackKat.Tracker (seenNotes)
import TrackKat.Window.Curves
import TrackKat.Window.Draw
import TrackKat.Window.GL
import TrackKat.Window.Style

-- | What trackkat's window shares with the rest of trackkat.
data Window = Window
    { wStatus :: IORef Status
    , wLog :: IORef (Seq String)
    -- ^ the latest 'logLines' lines
    , wMessage :: IORef String
    -- ^ the line beside the Save button
    , wInbox :: TQueue Delivery
    , wActions :: TQueue Action
    , wCapture :: TVar (Maybe Hotkey)
    -- ^ the binding list waiting for a key or button to add
    , wVisibility :: IORef Visibility
    -- ^ what the user last asked of the window
    , wWaker :: MVar (Maybe (IO ()))
    -- ^ how to wake the window's thread, while it runs
    }

-- | Whether the window is on the screen (perhaps minimised), or hidden
-- with only the tray icon left.
data Visibility = OnScreen | InTray
    deriving (Eq, Show)

-- | Why the window couldn't be shown.
newtype WindowFailure = WindowFailure String

-- | Something handed to the window's thread.
data Delivery
    = -- | settings' values, by their names in the settings file
      SettingsShown [(String, String)]
    | BindingAdded Hotkey Binding
    | -- | the user asked to see the window
      Raise

-- | What the user asks for in the window.
data Action
    = RecenterAction
    | PauseAction
    | TrimAction
    | CalibrateAction
    | -- | save these settings' values, by their names in the settings file
      SaveAction [(String, String)]
    deriving (Eq, Show)

-- | How many of the latest log lines the window keeps.
logLines :: Int
logLines = 5000

newWindow :: IO Window
newWindow =
    Window
        <$> newIORef initialStatus
        <*> newIORef Seq.empty
        <*> newIORef "Separate several keys or buttons with commas."
        <*> newTQueueIO
        <*> newTQueueIO
        <*> newTVarIO Nothing
        <*> newIORef InTray
        <*> newMVar Nothing

-- | Change the status shown. Never waits on the window.
updateStatus :: Window -> (Status -> Status) -> IO ()
updateStatus w f = atomicModifyIORef' (wStatus w) (\s -> (f s, ()))

currentStatus :: Window -> IO Status
currentStatus = readIORef . wStatus

-- | Add a line to the log.
logLine :: Window -> String -> IO ()
logLine w l = atomicModifyIORef' (wLog w) (\ls -> (Seq.drop (Seq.length ls + 1 - logLines) (ls |> l), ()))

-- | Show settings' values, by their names in the settings file, replacing
-- any edits to them. Names the window doesn't show are ignored.
showSettings :: Window -> [(String, String)] -> IO ()
showSettings w s = atomically (writeTQueue (wInbox w) (SettingsShown s))

-- | Say whether the last save worked.
showSettingsMessage :: Window -> String -> IO ()
showSettingsMessage w = writeIORef (wMessage w)

-- | What the user asked for since the last call.
takeActions :: Window -> IO [Action]
takeActions w = atomically (flushTQueue (wActions w))

-- | The binding list waiting for a key or joystick button to add, if any.
capturing :: Window -> IO (Maybe Hotkey)
capturing = readTVarIO . wCapture

-- | Add a binding to a list in the window, and stop waiting for one.
addBinding :: Window -> Hotkey -> Binding -> IO ()
addBinding w k b = atomically $ do
    writeTQueue (wInbox w) (BindingAdded k b)
    modifyTVar' (wCapture w) (\c -> if c == Just k then Nothing else c)

stopCapturing :: Window -> IO ()
stopCapturing w = atomically (writeTVar (wCapture w) Nothing)

-- | Bring the window to the screen, if it is running.
openWindow :: Window -> IO ()
openWindow w = do
    writeIORef (wVisibility w) OnScreen
    atomically (writeTQueue (wInbox w) Raise)
    withMVar (wWaker w) sequence_

-- | Whether the window is running and on the screen; hidden in the tray
-- doesn't count.
windowOpen :: Window -> IO Bool
windowOpen w = do
    running <- isJust <$> readMVar (wWaker w)
    visible <- readIORef (wVisibility w)
    pure (running && visible == OnScreen)

-- | Run the window, on the screen or in the tray to start with, until the
-- process ends. Call it on a bound thread: GLFW and OpenGL keep their state
-- per OS thread. Returns only if the window can't be made.
runWindow :: Window -> Visibility -> IO WindowFailure
runWindow w start = do
    writeIORef (wVisibility w) start
    ok <- GLFW.init
    if not ok
        then WindowFailure . ("can't start GLFW: " ++) <$> glfwError
        else do
            GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
            GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 0)
            GLFW.windowHint (GLFW.WindowHint'ScaleToMonitor True)
            GLFW.windowHint (GLFW.WindowHint'Visible False)
            (width, height) <- startingSize
            -- On Windows, GLFW gives the window the icon linked into the
            -- program as GLFW_ICON: see icons/trackkat.rc.
            GLFW.createWindow width height "trackkat" Nothing Nothing >>= \case
                Nothing -> do
                    why <- glfwError
                    GLFW.terminate
                    pure (WindowFailure ("can't open a window with OpenGL 3: " ++ why))
                Just win -> do
                    GLFW.makeContextCurrent (Just win)
                    GLFW.swapInterval 0
                    _ <- createContext
                    -- ImGui would otherwise save its own window positions.
                    Raw.IO.setIniFilename nullPtr
                    fonts <- loadFonts
                    _ <- glfwInitForOpenGL win True
                    _ <- openGL3Init
                    editor <- newEditor
                    modifyMVar_ (wWaker w) (const (pure (Just GLFW.postEmptyEvent)))
                    when (start == OnScreen) $ GLFW.showWindow win
                    forever (cycleOnce w win fonts editor)

-- | The latest GLFW error, described.
glfwError :: IO String
glfwError = maybe "unknown error" (\(e, s) -> s ++ " (" ++ show e ++ ")") <$> GLFW.getError

-- | The window's size before scaling to the screen: tall enough for
-- everything but the log's history, but no taller than the screen.
startingSize :: IO (Int, Int)
startingSize = do
    fits <-
        GLFW.getPrimaryMonitor >>= \case
            Nothing -> pure 960
            Just m -> do
                (_, _, _, h) <- GLFW.getMonitorWorkarea m
                (_, scale) <- GLFW.getMonitorContentScale m
                pure (round (fromIntegral h / max 1 scale) - 40)
    pure (780, min 1120 fits)

-- | How often the window is drawn while on the screen.
framesPerSecond :: Double
framesPerSecond = 30

-- | Handle what was handed over and the window's events, then draw a frame
-- if it is time, or else wait for the next.
cycleOnce :: Window -> GLFW.Window -> Fonts -> Editor -> IO ()
cycleOnce w win fonts editor = do
    deliveries <- atomically (flushTQueue (wInbox w))
    forM_ deliveries $ \case
        SettingsShown s -> deliver editor s
        BindingAdded k b -> addToList editor k b
        Raise -> do
            GLFW.showWindow win
            GLFW.restoreWindow win
            GLFW.focusWindow win
    closing <- GLFW.windowShouldClose win
    when closing $ do
        GLFW.setWindowShouldClose win False
        writeIORef (wVisibility w) InTray
    wanted <- readIORef (wVisibility w)
    visible <- GLFW.getWindowVisible win
    case wanted of
        InTray | visible -> GLFW.hideWindow win >> stopCapturing w
        _ -> pure ()
    focused <- GLFW.getWindowFocused win
    unless focused $ stopCapturing w
    st <- readIORef (wStatus w)
    iconified <- GLFW.getWindowIconified win
    if wanted == InTray || iconified
        then GLFW.waitEvents
        else do
            now <- fromMaybe 0 <$> GLFW.getTime
            due <- readIORef (edNextFrame editor)
            if now < due
                then GLFW.waitEventsTimeout (due - now)
                else do
                    writeIORef (edNextFrame editor) (max now (due + 1 / framesPerSecond))
                    drawFrame w win fonts editor st

drawFrame :: Window -> GLFW.Window -> Fonts -> Editor -> Status -> IO ()
drawFrame w win fonts editor st = do
    openGL3NewFrame
    glfwNewFrame
    newFrame
    (scale, _) <- GLFW.getWindowContentScale win
    withStyle scale fonts $ do
        Raw.setNextWindowFullscreen
        withCString "trackkat" $ \name ->
            bracket_ (Raw.begin name Nothing (Just mainFlags)) Raw.end $
                windowContents w win (Frame scale fonts st editor)
    render
    (fw, fh) <- GLFW.getFramebufferSize win
    clearFrame fw fh (rgbF background)
    openGL3RenderDrawData =<< getDrawData
    GLFW.swapBuffers win
  where
    mainFlags = ImGuiWindowFlags_NoTitleBar .|. ImGuiWindowFlags_NoResize .|. ImGuiWindowFlags_NoMove .|. ImGuiWindowFlags_NoCollapse .|. ImGuiWindowFlags_NoSavedSettings .|. ImGuiWindowFlags_NoBringToFrontOnFocus
    rgbF (Colour r g b _) = (fromIntegral r / 255, fromIntegral g / 255, fromIntegral b / 255)

-- | The window's own state, kept on its thread.
data Editor = Editor
    { edDraft :: IORef (M.Map String String)
    -- ^ the settings the window shows, as edited, by their names in the
    -- settings file
    , edShown :: IORef (M.Map String String)
    -- ^ the settings as last shown from the file
    , edCurves :: IORef CurveView
    , edLogScroll :: IORef LogScroll
    , edLogLength :: IORef Int
    -- ^ the log's length when last drawn
    , edNextFrame :: IORef Double
    -- ^ when to draw the next frame, in GLFW's seconds
    }

-- | How the curves are edited: as multipliers with precisions, or as text.
data CurveView = CurveControls | CurveText
    deriving (Eq)

-- | Whether the log follows new lines, or the user has scrolled back.
data LogScroll = FollowingLog | ScrolledBack

newEditor :: IO Editor
newEditor =
    Editor
        <$> newIORef M.empty
        <*> newIORef M.empty
        <*> newIORef CurveControls
        <*> newIORef FollowingLog
        <*> newIORef 0
        <*> newIORef 0

-- | The settings the window shows, by their names in the settings file.
windowSettings :: [String]
windowSettings =
    map hotkeySetting [minBound .. maxBound]
        ++ ["smoothing", "steadiness", "responsiveness"]
        ++ map axisSetting [minBound .. maxBound]
        ++ ["lean"]

deliver :: Editor -> [(String, String)] -> IO ()
deliver ed s = do
    let shown = M.fromList [(n, v) | (n, v) <- s, n `elem` windowSettings]
    modifyIORef' (edDraft ed) (M.union shown)
    modifyIORef' (edShown ed) (M.union shown)

-- | Add a binding to a list's text, unless it is there already.
addToList :: Editor -> Hotkey -> Binding -> IO ()
addToList ed k b = modifyIORef' (edDraft ed) (M.alter (Just . add . fromMaybe "") (hotkeySetting k))
  where
    new = renderBinding b
    add old
        | T.toCaseFold (T.pack new) `elem` map (T.toCaseFold . T.strip) (T.splitOn "," (T.pack old)) = old
        | all (`elem` (" ," :: String)) old = new
        | otherwise = old ++ ", " ++ new

-- | What a frame is drawn from.
data Frame = Frame
    { frScale :: Float
    -- ^ 1 at 100% scaling
    , frFonts :: Fonts
    , frStatus :: Status
    , frEditor :: Editor
    }

-- | A length at 100% scaling, scaled.
px :: Frame -> Float -> Float
px fr n = n * frScale fr

windowContents :: Window -> GLFW.Window -> Frame -> IO ()
windowContents w win fr = do
    livePanel fr
    buttons w fr
    calibration fr
    settings w fr
    logPane w win fr
    -- Esc hides the window, as closing it does, unless a field is being
    -- edited, where it undoes the edit.
    escape <- Raw.shortcut (let ImGuiKey k = ImGuiKey_Escape in fromIntegral k) ImGuiInputFlags_None
    when escape $ writeIORef (wVisibility w) InTray

str :: String -> IO ()
str = text . T.pack

coloured :: Colour -> String -> IO ()
coloured c = textColored (pure (vec4 c) :: IO ImVec4) . T.pack

-- | Help for the item just drawn, shown when the mouse rests on it.
tip :: Frame -> String -> IO ()
tip fr s = withItemTooltip $ withTextWrapPos (px fr 380) (str s)

small :: Frame -> IO a -> IO a
small fr = withFontSize (realToFrac (px fr smallFontSize))

boldText :: Frame -> String -> IO ()
boldText fr = withFont (bold (frFonts fr)) . str

-- | Put the next item on the same line, at a distance from the left of the
-- window.
besideAt :: Float -> IO ()
besideAt x = Raw.sameLine >> setCursorPosX x

textWidth :: String -> IO Float
textWidth s = (\(ImVec2 wd _) -> wd) <$> calcTextSize (T.pack s) False (-1)

-- | A button at least a width wide, its label centred.
button' :: Frame -> String -> Float -> IO Bool
button' fr l wd = do
    tw <- textWidth (takeWhile (/= '#') l)
    let padding = ImVec2 (max (px fr 8) ((wd - tw) / 2)) (px fr 4)
    withStyleVar ImGuiStyleVar_FramePadding (pure padding :: IO ImVec2) $
        withCString (T.pack l) Raw.button

-- | Whether games are following the head.
live :: Status -> Bool
live st = case (stCamera st, stSighting st) of
    (Tracking, InSight _ _) -> True
    _ -> False

-- | The head as last seen, or else as games see it.
headPose :: Status -> HeadPose
headPose st = case stSighting st of
    InSight hp _ -> hp
    OutOfSight _ -> stView st

-- | The dark panel at the top: the head as games see it, each axis, the
-- status lights and the performance figures.
livePanel :: Frame -> IO ()
livePanel fr = do
    let st = frStatus fr
        height = px fr 282
        pad = px fr 10
        flags = ImGuiWindowFlags_NoScrollbar .|. ImGuiWindowFlags_NoScrollWithMouse
    withChild "live" (ImVec2 0 height) False flags $ \_ -> do
        ImVec2 width _ <- Raw.getWindowSize
        ImVec2 l t <- Raw.getWindowPos
        list <- Raw.getWindowDrawList
        fill (Canvas list l t width height) panel (px fr 6)
        setCursorPos (pure (ImVec2 pad pad) :: IO ImVec2)
        let size = height - 2 * pad
        headCanvas <- canvas size size
        drawHead headCanvas (frScale fr) (stView st) (live st)
        tip fr "Your head as games see it, turned and moved as the game's view will be. The blue line shows where it looks."
        let x0 = pad + size + px fr 18
            right = width - pad
        setCursorPos (pure (ImVec2 x0 pad) :: IO ImVec2)
        withStyleVar ImGuiStyleVar_ItemSpacing (pure (ImVec2 (px fr 8) (px fr 4)) :: IO ImVec2) $ do
            axes fr x0 (right - x0)
            setCursorPosY (pad + px fr 152)
            lights fr x0 right
        setCursorPos (pure (ImVec2 x0 (height - pad - px fr 17)) :: IO ImVec2)
        case stCamera st of
            Tracking | not (null (stStats st)) -> small fr $ do
                coloured dim (stStats st)
                tip fr "Over the last second: how long trackkat took from a camera frame to games having the pose (average and worst), the longest wait between frames, the most stripes in a frame and garbage collections. For bug reports."
            _ -> pure ()
        overlay fr headCanvas st

-- | Across the foot of the head drawing, why it isn't moving, if it isn't.
overlay :: Frame -> Canvas -> Status -> IO ()
overlay fr cv st = case why of
    Nothing -> pure ()
    Just s -> do
        let size = px fr baseFontSize
        wd <- withFontWithSize (bold (frFonts fr)) (realToFrac size) (textWidth s)
        label cv (bold (frFonts fr)) size amber (cvLeft cv + (cvWidth cv - wd) / 2, cvTop cv + cvHeight cv - px fr 28) s
  where
    why = case (stCamera st, stSighting st, stOutput st) of
        (Tracking, OutOfSight _, _) -> Just "Clip out of sight"
        (Tracking, InSight _ _, Paused) -> Just "PAUSED"
        (Tracking, InSight _ _, Live) -> Nothing
        _ -> Just "No tracking"

-- | Each axis: the head, what games see, and a bar, in columns from x0
-- across a width.
axes :: Frame -> Float -> Float -> IO ()
axes fr x0 width = do
    let st = frStatus fr
        headRight = x0 + px fr 150
        viewRight = x0 + px fr 250
        barLeft = x0 + px fr 266
        help = "Hover over anything for help"
        rightAligned edge s = do
            wd <- textWidth s
            besideAt (edge - wd)
    small fr $ do
        setCursorPosX x0
        str ""
        rightAligned headRight "head"
        coloured dim "head"
        rightAligned viewRight "games see"
        coloured dim "games see"
        rightAligned (x0 + width) help
        coloured dim help
    forM_ [minBound .. maxBound] $ \a -> do
        let h = axisValue a (headPose st)
            v = axisValue a (stView st)
            held = case stOutput st of
                Paused -> True
                Live -> live st
        setCursorPosX x0
        str (axisName a)
        withFont (mono (frFonts fr)) $ do
            rightAligned headRight (formatAxis a h)
            coloured (if live st then textColour else dim) (formatAxis a h)
            tip fr "Your head as the camera measures it, from the centre you set by recentering: degrees turned, mm moved."
            rightAligned viewRight (formatAxis a v)
            coloured (if held then accent else dim) (formatAxis a v)
            tip fr "What games get: your head after smoothing, the curves and any trim. While paused, or with the clip out of sight, it holds still."
        besideAt barLeft
        lineHeight <- realToFrac <$> Raw.getTextLineHeight
        bar <- canvas (x0 + width - barLeft) lineHeight
        drawBar bar (frScale fr) a h v held
        let (minus, plus) = axisSides a
            sz = px fr 11
            middle = cvTop bar + (lineHeight - sz) / 2
        label bar (regular (frFonts fr)) sz dim (cvLeft bar + px fr 5, middle) minus
        plusWidth <- withFontSize (realToFrac sz) (textWidth plus)
        label bar (regular (frFonts fr)) sz dim (cvLeft bar + cvWidth bar - plusWidth - px fr 5, middle) plus
        tip fr "Blue: where games see each axis, out to the end of its range (behind you, straight up, 300 mm). Yellow tick: your head."

-- | The status lights, from x0, their values wrapping at the right.
lights :: Frame -> Float -> Float -> IO ()
lights fr x0 right = do
    let st = frStatus fr
        light c name value help = do
            setCursorPosX x0
            withGroup $ do
                lineHeight <- realToFrac <$> Raw.getTextLineHeight
                d <- canvas (px fr 8) lineHeight
                dot d{cvTop = cvTop d + (lineHeight - px fr 8) / 2, cvHeight = px fr 8} c
                besideAt (x0 + px fr 16)
                small fr (coloured dim name)
                besideAt (x0 + px fr 66)
                withTextWrapPos right (str value)
            tip fr help
        games = stGames st
    case stCamera st of
        Tracking -> light green "Camera" "Connected" cameraHelp
        Searching -> light grey "Camera" "Looking for it…" cameraHelp
        CameraDown why -> light red "Camera" (if null why then "Not found" else why) cameraHelp
    case (stCamera st, stSighting st) of
        (Tracking, InSight _ seen) -> light green "Clip" (intercalate ", " ("In sight" : seenNotes seen)) clipHelp
        (Tracking, OutOfSight n) -> light amber "Clip" (printf "Out of sight (%d of 3 reflectors)" n) clipHelp
        _ -> light grey "Clip" "—" clipHelp
    light (if games > 0 then accent else grey) "Games" (if games == 0 then "None" else show games ++ " connected") gamesHelp
    case stTrim st of
        NoTrim -> light grey "Trim" "None" trimHelp
        Trim y p -> light accent "Trim" (printf "yaw %+.0f° pitch %+.0f°" y p) trimHelp
    case stOutput st of
        Live -> light green "Output" "Live" outputHelp
        Paused -> light amber "Output" "Paused: games see your head held still" outputHelp
  where
    cameraHelp = "Green when trackkat has the camera; grey while it looks for it; red, with the reason, when it can't use it. trackkat picks it up again after unplugging or sleep."
    gamesHelp = "Games reading your head through trackkat's NPClient. The camera's second green light shows this too."
    clipHelp = "Whether the camera sees all three reflectors on the TrackClip, and anything unusual: a reflector out of sight or merged with another, or stray light. Out of sight, games see your head held where it was until it comes back."
    trimHelp = "Where Trim made the new forward, in degrees as games see it (pitch positive down). Recenter clears it."
    outputHelp = "Live: games follow your head. Paused: games see your head held still until you press Pause again."

-- | A button's label naming the first of an action's bindings, as last
-- shown from the settings file; the part after ### identifies the button
-- whatever it says.
actionLabel :: Frame -> String -> Hotkey -> IO String
actionLabel fr action k = do
    shown <- readIORef (edShown (frEditor fr))
    let bindings = filter (not . null) (map (T.unpack . T.strip) (T.splitOn "," (T.pack (M.findWithDefault "" (hotkeySetting k) shown))))
    pure $
        action ++ case bindings of
            [] -> "###" ++ hotkeySetting k
            [b] -> " (" ++ b ++ ")###" ++ hotkeySetting k
            b : _ -> " (" ++ b ++ ", …)###" ++ hotkeySetting k

request :: Window -> Action -> IO ()
request w a = atomically (writeTQueue (wActions w) a)

buttons :: Window -> Frame -> IO ()
buttons w fr = do
    let st = frStatus fr
        push l a help = do
            clicked <- button' fr l (px fr 150)
            when clicked $ request w a
            tip fr help
            Raw.sameLine
    Raw.spacing
    recenterLabel <- actionLabel fr "Recenter" RecenterKey
    pauseLabel <- actionLabel fr (case stOutput st of Paused -> "Resume"; Live -> "Pause") PauseKey
    trimLabel <- actionLabel fr "Trim" TrimKey
    push recenterLabel RecenterAction "Make the way you face now straight ahead, and clear any trim. Its keys, or a game's own recenter binding, do the same from anywhere."
    push pauseLabel PauseAction "Freeze the view: games see your head held still until you press it again. Handy for stretching, or looking away from the screen."
    push trimLabel TrimAction "Make where you're looking the new forward, handy for staring at an MFD; you can still glance around from there. Recenter clears it."
    push "Calibrate level" CalibrateAction "Learn which way is up now: click, then turn your head left and right for five seconds. trackkat learns it as you play anyway, so this is for after moving the camera."
    small fr $ do
        Raw.alignTextToFramePadding
        withTextWrapPos 0 (coloured dim "Closing this window keeps trackkat running in the tray.")

calibration :: Frame -> IO ()
calibration fr = do
    Raw.spacing
    separatorText "Calibration"
    withGroup $ withTextWrapPos 0 $ str (stCalibration (frStatus fr))
    tip fr "What trackkat has learnt from how you turn your head: how the camera is tipped, and where your neck pivots. It keeps learning as you play, and keeps it in trackkat.calibration; delete that to start afresh."

draftValue :: Frame -> String -> IO String
draftValue fr name = M.findWithDefault "" name <$> readIORef (edDraft (frEditor fr))

setDraft :: Frame -> String -> String -> IO ()
setDraft fr name v = modifyIORef' (edDraft (frEditor fr)) (M.insert name v)

-- | A setting's text, to edit.
setting :: Frame -> String -> StateVar Text
setting fr name = makeStateVar (T.pack <$> draftValue fr name) (setDraft fr name . T.unpack)

settings :: Window -> Frame -> IO ()
settings w fr = do
    Raw.spacing
    separatorText "Settings"
    bindingLists w fr
    smoothing fr
    curves fr
    leanBox fr
    saveRow w fr

bindingLists :: Window -> Frame -> IO ()
bindingLists w fr = do
    waiting <- readTVarIO (wCapture w)
    forM_ [minBound .. maxBound] $ \k -> do
        let (name, help) = case k of
                RecenterKey -> ("Recenter", "Keys and buttons that recenter, separated by commas, such as F12, Ctrl+Mouse4 or Throttle - HOTAS Warthog button 23. They work from anywhere, even inside a game.")
                PauseKey -> ("Pause", "Keys and buttons that pause, separated by commas, such as F9, Mouse5 or Joystick - HOTAS Warthog button 5. They work from anywhere, even inside a game.")
                TrimKey -> ("Trim", "Keys and buttons that trim, separated by commas, such as Ctrl+F12 or Throttle - HOTAS Warthog button 24. Empty, as it starts, trim has no key.")
            addWidth = px fr 170
        Raw.alignTextToFramePadding
        str name
        tip fr help
        besideAt (px fr 100)
        ImVec2 avail _ <- Raw.getContentRegionAvail
        setNextItemWidth (avail - addWidth - px fr 8)
        _ <- inputText (T.pack ("##" ++ hotkeySetting k)) (setting fr (hotkeySetting k)) 256
        tip fr help
        Raw.sameLine
        let addLabel = if waiting == Just k then "Press it now…" else "Add key or button…"
        clicked <- button' fr (addLabel ++ "###add" ++ hotkeySetting k) addWidth
        when clicked $ atomically $ writeTVar (wCapture w) (if waiting == Just k then Nothing else Just k)
        tip fr "Click, then press the key, joystick button, or middle or side mouse button to add. Click again to stop waiting."

smoothing :: Frame -> IO ()
smoothing fr = do
    on <- (/= "off") <$> draftValue fr "smoothing"
    Raw.spacing
    _ <- checkbox "Smoothing" (makeStateVar (pure on) (\b -> setDraft fr "smoothing" (if b then "on" else "off")))
    tip fr "Calm the view while your head is still, at the cost of a little lag when you move. Off, games get every tremor."
    withDisabled (pure (not on) :: IO Bool) $ do
        knob (px fr 140) "Steadiness" "steadiness" "How still the view stays while your head is still. Raise it if the view jitters; lower it if it feels floaty or drifts into place."
        knob (px fr 420) "Responsiveness" "responsiveness" "How closely the view keeps up when you move. Raise it if the view lags behind your head; lower it if it jitters as you move slowly."
  where
    knob x name key help = do
        besideAt x
        str name
        tip fr help
        Raw.sameLine
        setNextItemWidth (px fr 150)
        value <- maybe 0 round . parseNumber <$> draftValue fr key
        _ <- sliderInt (T.pack ("##" ++ key)) (makeStateVar (pure value) (setDraft fr key . show)) 0 100
        tip fr help

curves :: Frame -> IO ()
curves fr = do
    view' <- readIORef (edCurves (frEditor fr))
    Raw.spacing
    withTableOpen defTableOptions{tableFlags = ImGuiTableFlags_SizingStretchSame} "curves" 3 $
        forM_ [[Yaw, Pitch, Roll], [X, Y, Z]] $ \row -> do
            tableNextRow
            forM_ row $ \a -> do
                tableNextColumn $ withID (fromEnum a) (curveCell fr view' a)

curveCell :: Frame -> CurveView -> PoseAxis -> IO ()
curveCell fr view' a = do
    written <- draftValue fr (axisSetting a)
    let st = frStatus fr
        form = readCurveForm a written
        precisionNow = case form of
            MultiplierForm _ p -> p
            _ -> 0
        factorNow = fromMaybe 1 (formFactor form)
        setCurve f p = setDraft fr (axisSetting a) (multiplierText f p)
        typed t = case parseNumber (T.unpack (T.strip t)) of
            Just f | f > 0, f <= 1000 -> setCurve f precisionNow
            _ -> pure ()
    plot <- canvas (px fr 76) (px fr 76)
    drawCurve plot (frScale fr) a form (axisValue a (headPose st)) (live st)
    tip fr "The curve: across, your head; up, the game's view. The dot is your head now; with a precision, the dashed line is the multiplier alone."
    Raw.sameLine
    withGroup $ do
        Raw.alignTextToFramePadding
        boldText fr (axisName a)
        tip fr multiplierHelp
        case view' of
            CurveControls -> do
                Raw.sameLine
                str "×"
                tip fr multiplierHelp
                Raw.sameLine
                setNextItemWidth (px fr 44)
                _ <- inputText "##multiplier" (makeStateVar (pure (T.pack (maybe "" formatMultiplier (formFactor form)))) typed) 16
                tip fr multiplierHelp
                step "-" LT factorNow precisionNow setCurve
                step "+" GT factorNow precisionNow setCurve
                small fr $ do
                    Raw.alignTextToFramePadding
                    coloured dim "Precision"
                    tip fr precisionHelp
                    Raw.sameLine
                    ImVec2 avail _ <- Raw.getContentRegionAvail
                    setNextItemWidth (max (px fr 60) (avail - px fr 6))
                    _ <- sliderInt "##precision" (makeStateVar (pure (round precisionNow)) (setCurve factorNow . fromIntegral)) 0 100
                    tip fr precisionHelp
            CurveText -> do
                ImVec2 avail _ <- Raw.getContentRegionAvail
                setNextItemWidth (avail - px fr 6)
                _ <- inputText "##text" (setting fr (axisSetting a)) 256
                tip fr "The curve as trackkat.conf writes it: a multiplier such as 1:6, perhaps with a precision as in 1:6, precision 40, or input:output points such as 10:10 45:120, joined by straight lines from 0:0."
        small fr $ do
            case form of
                MultiplierForm f _ -> coloured dim (describeReach a f)
                PointsForm _ -> coloured dim "Custom curve, of points"
                Unreadable why -> coloured amber $ case view' of
                    CurveText -> why
                    CurveControls -> "Can't read this curve"
            tip fr "How far your head moves for the view to reach the end of its range. A custom curve, of points, is left as it is unless you change its multiplier or precision."
  where
    multiplierHelp = "How many times further the view turns or moves than your head; the line below says what that comes to. - and + step it by halves."
    precisionHelp = "Above 0, small movements are gentler, for aiming and reading gauges, and large ones steeper, so the same head movement still reaches the end of the view."
    step l direction f p setCurve = do
        Raw.sameLine
        clicked <- button' fr l 0
        when clicked $ setCurve (stepMultiplier direction f) p
        tip fr multiplierHelp

leanBox :: Frame -> IO ()
leanBox fr = do
    withView <- (/= "screen") <$> draftValue fr "lean"
    Raw.spacing
    _ <- checkbox "Lean relative to where the game's view looks (lean = view)" (makeStateVar (pure withView) (\b -> setDraft fr "lean" (if b then "view" else "screen")))
    tip fr "On, leaning follows the view: looking behind you, leaning left leans out of the cockpit's right side. Off, leaning moves you the same way whichever way you look."

-- | Put the next item on the same line, flush with the right edge.
rightOf :: Float -> IO ()
rightOf wd = do
    ImVec2 avail _ <- Raw.getContentRegionAvail
    x <- getCursorPosX
    Raw.sameLine
    setCursorPosX (x + avail - wd)

saveRow :: Window -> Frame -> IO ()
saveRow w fr = do
    Raw.spacing
    clicked <- button' fr "Save" (px fr 100)
    when clicked $ do
        draft <- readIORef (edDraft (frEditor fr))
        request w (SaveAction [(n, v) | n <- windowSettings, Just v <- [M.lookup n draft]])
    tip fr "Write these settings to trackkat.conf, beside trackkat.exe; they apply at once. Only what you changed is rewritten, so your own edits and comments there stay."
    Raw.sameLine
    message <- readIORef (wMessage w)
    Raw.alignTextToFramePadding
    coloured dim message
    tip fr "Whether the last Save worked, and if not, why."
    let asText = "Curves as text"
    wd <- textWidth asText
    frameHeight <- getFrameHeight
    view' <- readIORef (edCurves (frEditor fr))
    rightOf (wd + frameHeight + px fr 6)
    _ <- checkbox (T.pack asText) (makeStateVar (pure (view' == CurveText)) (\b -> writeIORef (edCurves (frEditor fr)) (if b then CurveText else CurveControls)))
    tip fr "Edit the curves as text, as trackkat.conf has them, to give any shape by points. trackkat.conf explains how."

logPane :: Window -> GLFW.Window -> Frame -> IO ()
logPane w win fr = do
    Raw.spacing
    separatorText "Log"
    lines' <- readIORef (wLog w)
    small fr $ do
        Raw.alignTextToFramePadding
        coloured dim "What trackkat reports, but not the status shown above. Copy it into bug reports."
    let copy = "Copy log"
    wd <- textWidth copy
    rightOf (wd + px fr 16)
    clicked <- button' fr copy 0
    when clicked $ setClipboard win (concatMap (++ "\n") (toList lines'))
    tip fr "Copy the whole log, to paste into a bug report."
    ImVec2 _ availH <- Raw.getContentRegionAvail
    let ed = frEditor fr
    readIORef (edLogScroll ed) >>= \case
        FollowingLog -> setNextWindowScroll (ImVec2 (-1) 1e9)
        ScrolledBack -> pure ()
    withFontWithSize (mono (frFonts fr)) (realToFrac (px fr smallFontSize)) $ do
        lineStep <- getTextLineHeightWithSpacing
        withChild "log" (ImVec2 0 (max (px fr 160) availH)) True ImGuiWindowFlags_None $ \_ -> do
            withListClipper Nothing (toList lines') str
            end <- getCursorPosY
            height <- getWindowHeight
            before <- readIORef (edLogLength ed)
            -- Lines added since the last frame are below what ImGui scrolled
            -- to, as it scrolls by the last frame's height.
            let added = max 0 (Seq.length lines' - before)
                below = end - height - fromIntegral added * lineStep
            writeIORef (edLogLength ed) (Seq.length lines')
            writeIORef (edLogScroll ed) (if below <= lineStep then FollowingLog else ScrolledBack)
    tip fr "What trackkat reports: recentering, trim, calibration, joysticks and the buttons pressed on them, settings reloads, and errors."
