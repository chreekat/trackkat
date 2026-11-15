-- | Still lights as the tracker keeps them (see "TrackKat.Lights"): which
-- of them a frame's blobs are masked by, and what each frame teaches of
-- them, the clip made out in it or not.
module TrackKat.Tracker.Still (
    Still (..),
    maskFor,

    -- * Learning from a frame
    Standing (..),
    confirmSeconds,
    standing,
    clipView,
    learnSeen,
    learnUnseen,
    inNewLight,
) where

import Data.Foldable (toList)

import TrackKat.Frame (Blob)
import TrackKat.Lights hiding (News (..))
import qualified TrackKat.Lights as Lights
import TrackKat.Match (Sight (..), Three, reflectors)
import TrackKat.Pose (Intrinsics, Model, Pose, seenAt)
import TrackKat.Tracker.Outcome (Event (..))
import TrackKat.World (Instant, Seconds, seconds, (.-.))

-- | Whether still lights are learnt and masked, and those learnt.
data Still = Still
    { stillMasking :: !Masking
    , stillLights :: !Lights
    }

-- | The areas masked in a frame: the lights learnt, but for any where a
-- reflector is predicted to be, at the places given.
maskFor :: Still -> [(Double, Double)] -> Mask
maskFor (Still masking lights) predicted = case masking of
    NoMasking -> noMask
    MaskLights -> activeMask predicted lights

-- | Whether the clip has been tracked for long enough that lights are
-- learnt clear of it.
data Standing = Confirmed | Unconfirmed
    deriving (Eq, Show)

-- | For how long the clip must have been tracked in every frame for still
-- lights to be learnt clear of it.
confirmSeconds :: Seconds
confirmSeconds = seconds 0.5

-- | The clip's standing at a time, tracked in every frame since another.
standing :: Instant -> Instant -> Standing
standing now since
    | now .-. since >= confirmSeconds = Confirmed
    | otherwise = Unconfirmed

-- | What a frame showed of the clip, tracked at a pose with each
-- reflector seen as given, for learning lights from it. Lights are learnt
-- clear of the clip only once it is confirmed: something tracked for a
-- moment can be lights that look like the clip. The number given is the
-- pixels the biggest reflector would light where the head was recentered.
clipView :: Intrinsics -> Model -> Standing -> Pose -> Three (Maybe Sight) -> Maybe Double -> View
clipView k m confirmed pose sights usual = case confirmed of
    Confirmed -> View (toList sights) discs usual
    Unconfirmed -> View (toList sights) [] Nothing
  where
    -- Where each reflector is, or would be, in the image, out to a margin
    -- that takes in the pieces of the biggest seen, such as a reflector
    -- broken in two.
    discs =
        let biggestSeen = maximum (0 : [sightPixels s | Just s <- toList sights])
         in [(seenAt k m pose which, 15 + 2 * sqrt (biggestSeen / pi)) | which <- toList reflectors]

-- | Learn still lights from a frame's blobs, seen at the given time,
-- given what it showed of the clip: the lights learnt, and the events
-- saying what changed. Nothing is learnt, or told, with masking off.
learnSeen :: Instant -> View -> [Blob] -> Still -> (Lights, [Event])
learnSeen now view blobs (Still masking lights) = case masking of
    NoMasking -> (lights, [])
    MaskLights -> map toEvent <$> learnLights now view blobs lights
  where
    toEvent (Lights.Learnt p why) = LightIgnored p why
    toEvent (Lights.Forgotten p) = LightForgotten p
    toEvent (Lights.Moved p how) = LightMoved p how

-- | Learn still lights from a frame where the clip wasn't made out, as
-- 'learnSeen' does. Lights just learnt that the clip would be found in
-- are the clip, held still: they are forgotten unannounced. What says so
-- is given: the places of the masked blobs the clip would be found in,
-- given what would be masked.
learnUnseen :: Instant -> [Blob] -> (Mask -> [(Double, Double)]) -> Still -> (Lights, [Event])
learnUnseen now blobs clipIn still = (lights, [e | e <- learntNews, not (isUnlearnt e)])
  where
    (learntLights, learntNews) = learnSeen now unseen blobs still
    newly = [p | LightIgnored p _ <- learntNews]
    (lights, unlearnt)
        | null newly = (learntLights, [])
        | otherwise = unlearn newly (clipIn (activeMask [] learntLights)) learntLights
    isUnlearnt (LightIgnored p _) = p `elem` unlearnt
    isUnlearnt _ = False

-- | Whether any of the given places is in a light that the events say
-- was just learnt, of the lights given: what was taken for the clip
-- there was the light.
inNewLight :: [Event] -> [(Double, Double)] -> Lights -> Bool
inNewLight news = holding [p | LightIgnored p _ <- news]
