-- | What is learnt of the reflectors' sizes, and what a blob's size says
-- (see "TrackKat.Tracker.Sizes").
module Test.Tracker.Sizes (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Test.Gen
import Test.Instances ()
import TrackKat.Linear (V3 (..), identity)
import TrackKat.Match (Three (..))
import TrackKat.Pose (Pose (..))
import TrackKat.Tracker.Sizes
import TrackKat.World (Seconds, World (..), inSeconds, seconds, world)

-- | One distribution says how big a reflector is, wherever a size is
-- judged: a blob is likeliest the size expected and no likelier the
-- further off, what is learnt from a blob is never then contradicted by
-- it, and what is learnt fades at the rate stated.
tests :: TestTree
tests =
    testGroup
        "Sizes"
        [ testProperty "a reflector not seen is the size reflectors are, about, and not known" $
            once $
                conjoin
                    [ close 1e-9 (sizeAt 1 unlearnt) (reflectorPixels world)
                    , close 1e-12 (sizeVariance unlearnt) (log (reflectorPixelsFactor world) ^ (2 :: Int))
                    , sizeKnown unlearnt === False
                    ]
        , testProperty "a blob lights fewer pixels the further off, by the square of the distance" $
            \(ASize size) (Metres z) -> close (1e-9 * sizeAt 1 size) (sizeAt z size * z * z) (sizeAt 1 size)
        , testProperty "a blob is likeliest its reflector's when just the size expected, and no likelier the further off, bigger or smaller" $
            \(ASize size) (Metres z) (Times nearer) (Times further) ->
                let says k = readSays (reading z size (k * sizeAt z size))
                 in conjoin
                        [ property (says nearer <= says 1 && says (1 / nearer) <= says 1)
                        , property (says (nearer * further) <= says nearer + 1e-9)
                        , property (says (1 / (nearer * further)) <= says (1 / nearer) + 1e-9)
                        ]
        , testProperty "a blob half again smaller than expected or more, which may be part of its reflector, is no less likely than one as many times bigger" $
            \(ASize size) (Metres z) (Times k) ->
                let says times = readSays (reading z size (times * sizeAt z size))
                 in k >= 1.5 ==> property (says (1 / k) >= says k - 1e-9)
        , testProperty "the chances of a reflector's blob being each size, from a pixel up, come to one" $
            \(ASize size) ->
                let steps = 4000 :: Int
                    least = negate (sizeMean size)
                    width = (10 - least) / fromIntegral steps
                    density i = strayDensity * exp (readSays (reading 1 size (exp (sizeMean size + least + width * (fromIntegral i + 0.5)))))
                 in sizeMean size > log 100 ==> close 0.02 (width * sum (map density [0 .. steps - 1])) 1
        , testProperty "what is learnt from a blob is nearer it and surer, and the blob no less likely for it: never contradicted by its own latest" $
            \(ASize size) (Metres z) (Pixels pixels) ->
                let after = learntFrom z pixels size
                    off s = abs (readOff (reading z s pixels))
                 in conjoin
                        [ counterexample "further" (off after <= off size + 1e-9)
                        , counterexample "less sure" (sizeVariance after <= sizeVariance size + 1e-12)
                        , counterexample "less likely" (readSays (reading z after pixels) >= readSays (reading z size pixels) - 1e-9)
                        ]
        , testProperty "a blob teaches the same of its reflector at any distance, for the pixels it would light at 1 m" $
            \(Pixels atOneMetre) (Metres z) ->
                close 1e-9 (sizeEvidence (learntFrom z (atOneMetre / (z * z)) unlearnt)) (sizeEvidence (learntFrom 1 atOneMetre unlearnt))
        , testProperty "a blob much smaller than its reflector, likely part of it, teaches less than one as big as expected" $
            \(ASize size) (Metres z) ->
                let weighs pixels = sizeWeight (learntFrom z pixels size) - sizeWeight size
                 in sizeKnown size ==> property (weighs (sizeAt z size / 8) <= weighs (sizeAt z size))
        , testProperty "what is learnt fades as much over two times as over their sum, weighing less by e in the seconds sizes are remembered" $
            \(ASize size) (Lasting first) (Lasting second) ->
                conjoin
                    [ close (1e-9 * sizeWeight size) (sizeWeight (faded second (faded first size))) (sizeWeight (faded (seconds (inSeconds first + inSeconds second)) size))
                    , close (1e-9 * sizeWeight size) (sizeWeight (faded (sizeMemory world) size)) (sizeWeight size / exp 1)
                    , sizeEvidence (faded first size) === sizeEvidence size
                    ]
        , testProperty "fading leaves a size no surer, and long enough leaves it as it was before anything was learnt" $
            \(ASize size) (Lasting since) ->
                property (sizeVariance (faded since size) >= sizeVariance size - 1e-12)
                    .&&. close 1e-6 (sizeMean (faded (seconds (40 * inSeconds (sizeMemory world))) size)) (sizeMean unlearnt)
        , testProperty "a reflector seen for a few frames is known, and its size is the biggest's at a pose if it is; with no pose, or none known, there is none" $
            \(Pixels pixels) (Metres z) ->
                let seen = iterate (learntFrom 1 pixels) unlearnt !! 20
                    pose = Pose identity (V3 0 0 (1000 * z))
                 in conjoin
                        [ sizeKnown seen === True
                        , biggestAt (pure unlearnt) (Just pose) === Nothing
                        , biggestAt (Three unlearnt seen unlearnt) Nothing === Nothing
                        , maybe (counterexample "none" False) (\lit -> close (1e-6 * lit) lit (sizeAt z seen)) (biggestAt (Three unlearnt seen unlearnt) (Just pose))
                        ]
        , testProperty "sizes learnt well have nothing in common, and unlearnt sizes all but what one reflector differs from another by" $
            \(Pixels pixels) ->
                let seen = iterate (learntFrom 1 pixels) unlearnt !! 20
                 in shared (pure seen) === 0
                        .&&. close 1e-12 (shared (pure unlearnt)) (sizeVariance unlearnt - log (sameSizeFactor world) ^ (2 :: Int))
                        .&&. shared (Three unlearnt unlearnt seen) === 0
        , testProperty "with nothing in common, or one blob, being alike adds nothing; unlearnt, three all half again as big or more say more than two so and one as much smaller" $
            \(Metres z) (Times k) ->
                let off times = reading z unlearnt (times * sizeAt z unlearnt)
                    common = shared (pure unlearnt)
                 in conjoin
                        [ alikeness 0 [off k, off k, off k] === 0
                        , close 1e-9 (alikeness common [off k]) 0
                        , property (k < 1.5 || alikeness common [off k, off k, off k] >= alikeness common [off k, off k, off (1 / k)] - 1e-9)
                        ]
        , testProperty "a blob two reflectors share is as likely anything from the bigger of them to both, by its pixels, and less likely beyond" $
            \(ASize a) (ASize b) (Metres z) (Fraction between) (Times beyond) ->
                let both = sizeAt z a + sizeAt z b
                    bigger = max (sizeAt z a) (sizeAt z b)
                    says pixels = readSays (sharedReading z a b pixels)
                    part = bigger + between * (both - bigger)
                 in conjoin
                        [ close 1e-9 (says part - says both) (log (part / both))
                        , property (says (beyond * both) <= says both + 1e-9)
                        , property (says (bigger / beyond) <= says bigger + 1e-9)
                        ]
        ]

-- | What is learnt of a reflector's size: nothing, or the evidence of
-- blobs of up to 3000 pixels at 1 m. Simplest nothing.
newtype ASize = ASize Size
    deriving (Show)

instance Arbitrary ASize where
    arbitrary = (\(Three size _ _) -> ASize size) <$> genSizes 3000
    shrink (ASize size) = [ASize unlearnt | size /= unlearnt]

-- | How far off the clip is, in metres, within the camera's range.
newtype Metres = Metres Double
    deriving (Show)

instance Arbitrary Metres where
    arbitrary = Metres <$> choose (0.25, 2.5)
    shrink (Metres z) = Metres <$> shrinkWithin (0.25, 2.5) 1 z

-- | A blob's pixels, from the smallest kept to a lamp's.
newtype Pixels = Pixels Double
    deriving (Show)

instance Arbitrary Pixels where
    arbitrary = Pixels <$> choose (6, 5000)
    shrink (Pixels pixels) = Pixels <$> shrinkWithin (6, 5000) 6 pixels

-- | How many times as big, from as big to eight times.
newtype Times = Times Double
    deriving (Show)

instance Arbitrary Times where
    arbitrary = Times <$> choose (1, 8)
    shrink (Times k) = Times <$> shrinkWithin (1, 8) 1 k

-- | A length of time of up to two minutes.
newtype Lasting = Lasting Seconds
    deriving (Show)

instance Arbitrary Lasting where
    arbitrary = Lasting . seconds <$> choose (0, 120)
    shrink (Lasting d) = Lasting . seconds <$> shrinkWithin (0, 120) 0 (inSeconds d)
