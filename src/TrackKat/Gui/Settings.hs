{-# LANGUAGE RecursiveDo #-}

-- | The settings the parts of the window made in Haskell show and save,
-- with window = haskell: what passes between them and the settings file,
-- from any thread, and the fields showing them, on the window's.
module TrackKat.Gui.Settings (
    pushSetting,
    takeSaved,
    Settings,
    newSettings,
    Field (..),
    addField,
    showPushes,
    changed,
    saveNow,
) where

import Control.Monad (forM_, unless, void, when)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (nub)
import Data.Tuple (swap)
import Foreign.Ptr (FunPtr, nullPtr)
import Graphics.Win32.GDI.Types (HWND)
import System.IO.Unsafe (unsafePerformIO)
import System.Win32.Types (BOOL, DWORD, UINT, WPARAM)

import TrackKat.Gui.Paint (orReport)
import TrackKat.UI.Settings (Desk, emptyDesk, push, saveValues, takePushes)
import qualified TrackKat.UI.Settings as Desk (takeSaved)

foreign import ccall unsafe "windows.h SetTimer" c_SetTimer :: HWND -> WPARAM -> UINT -> FunPtr TimerProc -> IO WPARAM
foreign import ccall unsafe "windows.h KillTimer" c_KillTimer :: HWND -> WPARAM -> IO BOOL

-- | What passes between the window and the settings file.
desk :: IORef Desk
desk = unsafePerformIO (newIORef emptyDesk)
{-# NOINLINE desk #-}

-- | Show a setting the file says, if a field here shows it.
pushSetting :: String -> String -> IO ()
pushSetting name value = atomicModifyIORef' desk (\d -> (push name value d, ()))

-- | The settings of some names the user changed here and saved, with their
-- values, once each.
takeSaved :: [String] -> IO [(String, String)]
takeSaved names = atomicModifyIORef' desk (swap . Desk.takeSaved names)

-- | A setting shown in the window.
data Field = Field
    { fieldName :: String
    , showValue :: String -> IO ()
    , valueShown :: IO String
    -- ^ as trackkat.conf writes it
    }

-- | The window's settings, on the window's thread.
data Settings = Settings
    { fields :: IORef [Field]
    , changedNames :: IORef [String]
    -- ^ the names of those the user changed and hasn't saved
    , announce :: IO ()
    -- ^ say there are saved changes to take
    , autosave :: IORef WPARAM
    -- ^ the thread's timer to save them, or 0
    , autosaveProc :: FunPtr TimerProc
    }

type TimerProc = HWND -> UINT -> WPARAM -> DWORD -> IO ()

foreign import ccall "wrapper" mkTimerProc :: TimerProc -> IO (FunPtr TimerProc)

-- | The window's settings, no fields yet, telling of saved changes with the
-- action given and reporting what fails in saving them with the first.
newSettings :: (String -> IO ()) -> IO () -> IO Settings
newSettings report say = mdo
    s <- Settings <$> newIORef [] <*> newIORef [] <*> pure say <*> newIORef 0 <*> pure proc
    -- For the life of the process.
    proc <- mkTimerProc $ \_ _ _ _ -> orReport report "Saving the window's settings" (saveNow s) (pure ())
    pure s

addField :: Settings -> Field -> IO ()
addField s f = modifyIORef' (fields s) (++ [f])

-- | Show the file's pushes in their fields. See Note [The window's changes]
-- in "TrackKat.UI.Settings".
showPushes :: Settings -> IO ()
showPushes s = do
    names <- readIORef (changedNames s)
    shown <- atomicModifyIORef' desk (swap . takePushes names)
    fs <- readIORef (fields s)
    forM_ shown $ \(name, value) -> forM_ [f | f <- fs, fieldName f == name] (`showValue` value)

-- | Note that the user changed a setting, to save once some milliseconds
-- pass with no other change.
changed :: Settings -> UINT -> String -> IO ()
changed s ms name = do
    modifyIORef' (changedNames s) (nub . (++ [name]))
    -- With a timer of its own, replaced rather than added to.
    timer <- readIORef (autosave s)
    c_SetTimer nullPtr timer ms (autosaveProc s) >>= writeIORef (autosave s)

-- | Save the settings the user changed here, as their fields show them,
-- now rather than when 'changed' would.
saveNow :: Settings -> IO ()
saveNow s = do
    timer <- readIORef (autosave s)
    when (timer /= 0) (void (c_KillTimer nullPtr timer) >> writeIORef (autosave s) 0)
    names <- readIORef (changedNames s)
    writeIORef (changedNames s) []
    unless (null names) $ do
        fs <- readIORef (fields s)
        values <- sequence [(,) name <$> valueShown f | name <- names, f <- fs, fieldName f == name]
        atomicModifyIORef' desk (\d -> (saveValues values d, ()))
        announce s
