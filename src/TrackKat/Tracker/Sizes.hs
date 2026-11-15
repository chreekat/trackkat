-- | What is remembered of how big each reflector's blob is (see 'Size'),
-- and what it says of how the reflectors show.
module TrackKat.Tracker.Sizes (
    -- * Remembering
    sizeMemory,
    resized,
    everSeen,
    biggestAt,

    -- * How the reflectors show
    Showing (..),
    showing,
    showingSeconds,
) where

import Data.Foldable (toList)

import TrackKat.Match
import TrackKat.Pose (Pose)
import TrackKat.Tracker.Judge (metres)

-- | Seconds over which a reflector's usual size is forgotten.
sizeMemory :: Double
sizeMemory = 10

-- | A reflector's size after a frame so many seconds after the last one
-- tracked, with the clip the given metres away, given how the reflector
-- showed: its latest size is what a blob of its own shows, and its usual
-- size no less than that, fading meanwhile. One sharing a blob with
-- another has its share guessed, not seen, so only fades, as one unseen
-- does.
resized :: Double -> Double -> Size -> Maybe Sight -> Size
resized since z size seen =
    let usual = sizeUsual size * exp (negate since / sizeMemory)
     in case seen of
            Just s | sightKind s /= Merged -> let atOneMetre = sightPixels s * z * z in Size atOneMetre (max atOneMetre usual)
            _ -> size{sizeUsual = usual}

-- | Whether any reflector has been seen, lately enough to say how big it
-- is.
everSeen :: Three Size -> Bool
everSeen = any ((> 0) . sizeUsual)

-- | The pixels the biggest reflector, of the given sizes, lights at a
-- pose, if there is one and any has been seen.
biggestAt :: Three Size -> Maybe Pose -> Maybe Double
biggestAt sizes at = case (foldr (max . sizeUsual) 0 sizes, at) of
    (biggest, Just pose) | biggest > 0 -> Just (biggest / metres pose ^ (2 :: Int))
    _ -> Nothing

-- | How a match shows the reflectors: all three; one perhaps behind
-- another, sharing a blob no bigger than that one alone (see
-- 'bigEnoughForTwo'), or perhaps out of sight; or one out of sight.
data Showing = AllShowing | OneBehind | OneOut
    deriving (Eq, Show)

-- | How the given sights show the reflectors, given their sizes and the
-- distance in metres.
showing :: Three Size -> Double -> Three (Maybe Sight) -> Showing
showing sizes z sights = case sequenceA sights of
    Nothing -> OneOut
    Just three -> case [(sightPixels s, sizeLatest size / (z * z)) | (size, s) <- toList ((,) <$> sizes <*> three), sightKind s == Merged] of
        [(a, ea), (b, eb)] | not (bigEnoughForTwo (a + b) ea eb) -> OneBehind
        _ -> AllShowing

-- | For how long, in seconds since all three last showed, the clip is
-- tracked showing less: two reflectors leave the pose free to turn about
-- the line through them, and one taken to be behind another, as when the
-- head rolls far, may be out of sight instead.
showingSeconds :: Showing -> Double
showingSeconds AllShowing = 1 / 0
showingSeconds OneBehind = 2.5
showingSeconds OneOut = 1
