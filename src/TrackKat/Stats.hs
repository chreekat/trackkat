-- | Performance figures gathered over a short window, for the status line.
module TrackKat.Stats (
    Window,
    emptyWindow,
    sawFrame,
    sawCorrupt,
    sawGCs,
    summary,
) where

import Text.Printf (printf)

data Window = Window
    { wFrames :: !Int
    , wLatencyTotal :: !Double
    -- ^ seconds, summed over frames
    , wLatencyMax :: !Double
    -- ^ seconds
    , wGapMax :: !Double
    -- ^ seconds between frames arriving
    , wCorrupt :: !Int
    , wGCs :: !Int
    , wGCPauseMax :: !Double
    -- ^ seconds
    }
    deriving (Eq, Show)

emptyWindow :: Window
emptyWindow = Window 0 0 0 0 0 0 0

-- | A frame handled: seconds from its data arriving to its pose being
-- published, and seconds since the previous frame arrived.
sawFrame :: Double -> Double -> Window -> Window
sawFrame latency gap w =
    w
        { wFrames = wFrames w + 1
        , wLatencyTotal = wLatencyTotal w + latency
        , wLatencyMax = max (wLatencyMax w) latency
        , wGapMax = max (wGapMax w) gap
        }

sawCorrupt :: Window -> Window
sawCorrupt w = w{wCorrupt = wCorrupt w + 1}

-- | Garbage collections that ran, and the longest pause among them, in
-- seconds.
sawGCs :: Int -> Double -> Window -> Window
sawGCs n pause w = w{wGCs = wGCs w + n, wGCPauseMax = max (wGCPauseMax w) pause}

-- | For example @latency 0.31 ms (max 1.20)  gap max 9 ms  GC 2 (max 0.15 ms)@.
summary :: Window -> String
summary w =
    printf "latency %.2f ms (max %.2f)  gap max %.0f ms  GC %d (max %.2f ms)%s" mean (ms (wLatencyMax w)) (ms (wGapMax w)) (wGCs w) (ms (wGCPauseMax w)) corrupt
  where
    mean = if wFrames w == 0 then 0 else ms (wLatencyTotal w / fromIntegral (wFrames w))
    ms = (* 1000)
    corrupt = if wCorrupt w == 0 then "" else printf "  corrupt %d" (wCorrupt w) :: String
