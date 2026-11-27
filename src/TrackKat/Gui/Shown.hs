-- | What the window has been told to show, for the parts of it drawn in
-- Haskell: told from any thread, and read on the window's own from a
-- snapshot it takes each time it refreshes, so every part shows the same
-- moment.
module TrackKat.Gui.Shown (
    tell,
    takeSnapshot,
    snapshot,
) where

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)

import TrackKat.Display (Shown, nothingShown)

-- | Change what the window is told.
tell :: (Shown -> Shown) -> IO ()
tell f = atomicModifyIORef' told (\s -> (f s, ()))

-- | Take what the window has been told so far as what it shows; on the
-- window's thread.
takeSnapshot :: IO ()
takeSnapshot = readIORef told >>= writeIORef taken

-- | What the window shows, as of its last snapshot.
snapshot :: IO Shown
snapshot = readIORef taken

told, taken :: IORef Shown
told = unsafePerformIO (newIORef nothingShown)
{-# NOINLINE told #-}
taken = unsafePerformIO (newIORef nothingShown)
{-# NOINLINE taken #-}
