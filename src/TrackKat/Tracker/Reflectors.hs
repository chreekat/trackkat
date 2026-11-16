-- | What a frame shows of the clip, given where its reflectors are taken
-- to be: the third part of what a hypothesis about a frame is scored by
-- (see Note [One account] in "TrackKat.Tracker.Account").
--
-- See Note [What a frame shows].
module TrackKat.Tracker.Reflectors (
    Expect (..),
    whatShows,
    atBest,
    suiting,
    atBestAnywhere,
    blobGain,
    sightWeight,
    presences,
    reflectorName,
    pick,
) where

import Data.Foldable (toList)
import Data.Maybe (isJust)

import TrackKat.Frame (frameHeight, frameWidth)
import TrackKat.Pose (Reflector (..))
import TrackKat.Sight
import TrackKat.Term
import TrackKat.Tracker.Presence
import TrackKat.Tracker.Sizes
import TrackKat.World (Seconds, World (..), inPixels, world)

-- Note [What a frame shows]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
-- A frame is some blobs, each a place and a number of pixels. A hypothesis
-- says which of them are the clip's reflectors and where each reflector
-- should be; the rest are strays. What the frame says for the hypothesis
-- is how much likelier its blobs are so than if every one were a stray:
-- the log of that ratio, in nats, a sum of terms ('whatShows'), one lot for
-- each reflector. A stray adds nothing, being as likely either way, so a
-- blob that no hypothesis uses changes no score.
--
-- A reflector seen as a blob of its own:
--
--  * its place. A stray is as likely anywhere in the picture, and the
--    world's 'strayBlobs' of them are expected in a frame; a reflector's
--    centre is within the world's 'centroidNoise' of where the pose puts
--    it. The ratio of the two densities where they are highest is
--    'blobGain', some 11 nats: what a blob just where a reflector should
--    be says for its being the reflector. It falls by half the square of
--    how many noises off the blob is, to nothing at about five pixels,
--    and against beyond. A reflector seen dimly, or sharing a blob, has
--    its centre known less well ('sightWeight'): it loses less for being
--    off, and gains less for being on.
--
--  * its size: what the blob's pixels say of its being this reflector's,
--    beside a stray's (see Note [A reflector's size] in
--    "TrackKat.Tracker.Sizes").
--
--  * its showing at all: the chance that it shows now, given whether it
--    showed when the clip was last made out and how long ago that was (see
--    Note [Coming and going] in "TrackKat.Tracker.Presence").
--
-- A reflector unseen has only the last: the chance it is out of sight
-- now. It has no blob to gain by, which is most of what being unseen
-- costs beside being seen.
--
-- Two reflectors sharing a blob gain by it once, half each: by its
-- place, which is between where the two should be, as their sizes share
-- it, and known half as well as a blob of one's own; and by its size,
-- which is anything from the bigger of them to both. A reflector seen as
-- two blobs close together is judged as one blob of both, and gains by
-- the second ('pieces'): it is rare that a reflector breaks up, the
-- world's 'inPieces', but a piece is then somewhere within the
-- reflector's own width, far likelier there than a stray is.
--
-- What the three sizes say together is added once ('alikeness').

-- | Where each reflector should be seen in a frame, and what is known of
-- how it shows.
data Expect = Expect
    { expectAt :: Three (Double, Double)
    -- ^ where in the image each should be
    , expectMetres :: Double
    -- ^ how far the clip is from the camera
    , expectSizes :: Three Size
    -- ^ what is learnt of each one's size, as of the frame
    , expectShowed :: Maybe (Three Presence, Seconds)
    -- ^ whether each showed when the clip was last made out, and how long
    -- before the frame that was; nothing if it never was
    }

-- | What a blob just where a reflector should be says for its being the
-- reflector and not a stray, in nats: the log of the picture's area over
-- the world's 'strayBlobs' times the area the world's 'centroidNoise'
-- spreads a centre over.
blobGain :: Double
blobGain = log (fromIntegral (frameWidth * frameHeight) / (strayBlobs world * 2 * pi * noise * noise))
  where
    noise = inPixels (centroidNoise world)

-- | How much a reflector's place counts, seen so, when it is expected to
-- light so many pixels: its centre is good to the world's 'centroidNoise'
-- over this. A dim reflector, smaller than expected, counts for less: part
-- of it may be too dim to see, moving its centroid. One sharing a blob
-- counts for half, its place being an estimate.
sightWeight :: Double -> Sight -> Double
sightWeight expected s = dim * sharing
  where
    dim = if expected > 0 then max 0.05 (min 1 (sightPixels s / expected)) else 1
    sharing = if sightKind s == Merged then 0.5 else 1

-- | A reflector's name, as in "the top reflector".
reflectorName :: Reflector -> String
reflectorName LeftReflector = "left"
reflectorName RightReflector = "right"
reflectorName TopReflector = "top"

-- | One reflector's of three.
pick :: Reflector -> Three a -> a
pick LeftReflector (Three a _ _) = a
pick RightReflector (Three _ a _) = a
pick TopReflector (Three _ _ a) = a

-- | What a frame says for the clip's reflectors being seen as given, and
-- not every blob a stray: the log of the ratio of the two likelihoods (see
-- Note [What a frame shows]).
whatShows :: Expect -> Term (Three (Maybe Sight))
whatShows expect = foldMap (reflector expect) reflectors <> together
  where
    z = expectMetres expect
    together =
        likelihood (Name "alike" "the lights in view aren't alike in size as the clip's reflectors are") $ \sights ->
            alikeness (shared (expectSizes expect)) [reading z size (sightPixels s) | (size, Just s) <- toList ((,) <$> expectSizes expect <*> sights), sightKind s /= Merged]

-- | How much the place of a blob two reflectors share counts, beside a
-- blob of one's own: where its centre is between theirs depends on how
-- they overlap.
sharedWeight :: Double
sharedWeight = 0.5

-- | Whether each reflector shows, seen as given.
presences :: Three (Maybe Sight) -> Three Presence
presences = fmap (\sight -> if isJust sight then Present else Gone)

-- | The most a frame could say for the reflectors seen as given, were the
-- clip so far away: what it says with every reflector just where it was
-- seen.
atBest :: Expect -> Three (Maybe Sight) -> Score
atBest expect sights = score (whatShows expect{expectAt = (\q -> maybe q sightAt) <$> expectAt expect <*> sights}) sights

-- | The distances, in metres, that suit how big the reflectors are seen:
-- that expected; each at which a reflector seen as its own blob is just
-- the size it is expected to be; midway between each two of those; and a
-- few between the least and most of them all. What the sizes say is best
-- at one of them, or near enough.
suiting :: Expect -> Three (Maybe Sight) -> [Double]
suiting expect sights = expectMetres expect : own ++ [sqrt (a * b) | (i, a) <- zip [0 :: Int ..] own, (j, b) <- zip [0 ..] own, i < j] ++ [least * (most / least) ** (fromIntegral i / 4) | i <- [1 .. 3 :: Int], most > least]
  where
    own = [sqrt (exp (sizeMean size) / max 1 (sightPixels s)) | (size, Just s) <- toList ((,) <$> expectSizes expect <*> sights), sightKind s /= Merged]
    least = minimum (expectMetres expect : own)
    most = maximum (expectMetres expect : own)

-- | The most a frame could say for the reflectors seen as given, however
-- far away the clip: 'atBest' at the distance that suits it best.
atBestAnywhere :: Expect -> Three (Maybe Sight) -> Score
atBestAnywhere expect sights = maximum [atBest expect{expectMetres = z} sights | z <- suiting expect sights]

-- | What a frame says of one reflector, of how the three are seen.
reflector :: Expect -> Reflector -> Term (Three (Maybe Sight))
reflector expect which =
    likelihood (named "shows" "isn't in sight as it was") showing
        <> aboutAny placed place
        <> aboutAny (pick which) pieces
        <> aboutAny sized size
  where
    named what wrong = Name (reflectorName which ++ " " ++ what) ("the " ++ reflectorName which ++ " reflector " ++ wrong)
    z = expectMetres expect
    known = pick which (expectSizes expect)
    (u, v) = pick which (expectAt expect)
    noise = inPixels (centroidNoise world)
    showing sights = case pick which sights of
        Just _ -> inSight
        Nothing -> outOfSight
    (inSight, outOfSight) = case expectShowed expect of
        Just (was, since) -> (passing reflectorComings since (pick which was) Present, passing reflectorComings since (pick which was) Gone)
        Nothing -> (usually reflectorComings Present, usually reflectorComings Gone)
    -- The reflector's sight, with the sight, place and size of any
    -- reflector it shares a blob with.
    sharing sights = case pick which sights of
        Nothing -> Nothing
        Just s
            | sightKind s /= Merged -> Just (s, Nothing)
            | otherwise -> case [(o, q, other) | (r, q, other, Just o) <- toList ((,,,) <$> reflectors <*> expectAt expect <*> expectSizes expect <*> sights), r /= which, sightKind o == Merged] of
                partner : _ -> Just (s, Just partner)
                [] -> Just (s, Nothing)
    -- How far off its place the reflector's blob is, and how much its
    -- place counts: of a blob it shares, how far the blob's centre is
    -- from where the two's should be, each by its share of the blob.
    placed sights =
        ( \(s, partner) -> case partner of
            Nothing -> (off (sightAt s) (u, v), sightWeight (sizeAt z known) s, 1)
            Just (o, (x, y), _) ->
                let f = sightPixels s / max 1 (sightPixels s + sightPixels o)
                    (su, sv) = sightAt s
                    (ou, ov) = sightAt o
                 in (off (f * su + (1 - f) * ou, f * sv + (1 - f) * ov) (f * u + (1 - f) * x, f * v + (1 - f) * y), sharedWeight, 0.5)
        )
            <$> sharing sights
    off (a, b) (c, d) = sqrt ((a - c) ^ (2 :: Int) + (b - d) ^ (2 :: Int))
    place =
        weighing
            (named "place" "isn't where the clip's shape puts it")
            "px"
            Spread
            (\(d, w, part) -> part * (blobGain + 2 * log w - (w * d / noise) ^ (2 :: Int) / 2))
            (\(d, w, _) -> (d, noise / w))
    pieces =
        aboutAny (\s -> if sightKind s == Rejoined then Just s else Nothing) $
            likelihood (named "pieces" "is in pieces") $ \s ->
                log (inPieces world) + log (fromIntegral (frameWidth * frameHeight) / (strayBlobs world * 4 * max 1 (sightPixels s)))
    sized sights = (\(s, partner) -> (s, (\(o, _, other) -> (other, sightPixels o)) <$> partner)) <$> sharing sights
    size =
        weighing
            (named "size" "isn't the size it has been")
            "px"
            Expected
            (\sizing -> let (said, _, _) = sizeSays sizing in said)
            (\sizing -> let (_, pixels, expected) = sizeSays sizing in (pixels, expected))
    -- What a size says, with the pixels judged and those expected. Two
    -- sharing a blob say half each of what the blob says of both.
    sizeSays (s, Nothing) = (readSays (reading z known (sightPixels s)), sightPixels s, sizeAt z known)
    sizeSays (s, Just (other, pixels)) =
        (readSays (sharedReading z known other (sightPixels s + pixels)) / 2, sightPixels s + pixels, sizeAt z known + sizeAt z other)
