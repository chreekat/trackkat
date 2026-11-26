-- | What is learnt of the camera's level and the neck's pivot: read and
-- written, described, drawn, and learnt from a head's movement.
module Test.Calibration (tests, simulations) where

import Control.Monad (forM_)
import Data.Either (isLeft)
import Data.List (isPrefixOf)
import Data.Maybe (isNothing)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Test.Check
import Test.Gen (genTurn, genWritten, shrinkParts, written)
import qualified Test.Gen as Gen
import Test.Simulation
import TrackKat.Calibration
import TrackKat.Linear
import TrackKat.Number (renderNumber)
import TrackKat.Pose
import TrackKat.Synth

tests :: TestTree
tests = group "Calibration" $ do
    let facing = Pose identity (V3 0 0 700)
        closeV a b = norm (a ^-^ b) < 1e-3
    -- The clip on the head, from the neck pivot, once both are learnt: a
    -- clip squarely facing the camera at the centre, the pivot below and
    -- behind it, is above and in front of the pivot, its left reflector
    -- (as the camera sees it) on the wearer's right; a clip turned 90
    -- degrees, facing the camera from the side of the head, is on that side.
    let calibrated up = either error id (parseCalibration standardMounting ("up = " ++ up ++ "\nturning = 900\npivot = 0 100 100\npivot windows = 40\n"))
        middle (l, r, _) = 0.5 *^ (l ^+^ r)
        sides (l, r, t) = map norm [r ^-^ l, t ^-^ l, t ^-^ r]
        clipSides = sides (mLeft headset, mRight headset, mTop headset)
        closeTo a b = norm (a ^-^ b) < 1e-6
    check "no clip on the head before anything is learnt" $
        all isNothing [learntClip headset (Just facing) defaultCalibration, learntClip headset Nothing (calibrated "0 -1 0")]
    check "no clip on the head with only the level learnt" $
        isNothing (learntClip headset (Just facing) (either error id (parseCalibration standardMounting "up = 0 -1 0\nturning = 900\n")))
    case learntClip headset (Just facing) (calibrated "0 -1 0") of
        Nothing -> check "clip on the head once learnt" False
        Just placed@(l, r, _) -> do
            checkShowing "clip facing the camera is above and in front of the pivot" (show placed) $ closeTo (middle placed) (V3 0 100 100)
            checkShowing "its left reflector is on the wearer's right" (show placed) $ let V3 x _ _ = l; V3 x' _ _ = r in x > 0 && x' < 0
            check "the clip keeps its shape on the head" $ and (zipWith (\a b -> abs (a - b) < 1e-6) (sides placed) clipSides)
    case learntClip headset (Just (Pose (rodrigues (V3 0 (pi / 2) 0)) (V3 0 0 700))) (calibrated "0 -1 0") of
        Nothing -> check "turned clip on the head" False
        Just placed -> checkShowing "clip turned 90 degrees is on the wearer's right" (show placed) $ closeTo (middle placed) (V3 100 100 0)
    -- A pivot learnt 192 mm below the clip, 26 behind it and 1 to the
    -- left, as the user's was: the lower pair sits that far above and in
    -- front of it.
    let lowPivot = either error id (parseCalibration standardMounting "up = 0 -1 0\nturning = 900\npivot = 1 192 26\npivot windows = 40\n")
    checkShowing "a low pivot described" (describePivotBriefly (Just facing) lowPivot) $
        "Neck pivot 192±40 mm below the clip, 26±40 behind, 1±40 to your left;" `isPrefixOf` describePivotBriefly (Just facing) lowPivot
    case learntClip headset (Just facing) lowPivot of
        Nothing -> check "clip on the head with a low pivot" False
        Just placed -> checkShowing "the clip 192 mm above a low pivot and 26 in front" (show placed) $ closeTo (middle placed) (V3 1 192 26)
    -- With the camera tipped down, up on the head is the learnt up, not the
    -- camera's.
    let tipped = V3 0 (negate (cos (deg 20))) (negate (sin (deg 20)))
    case learntClip headset (Just facing) (calibrated "0 -0.9396926 -0.3420201") of
        Nothing -> check "clip on the head with the camera tipped" False
        Just placed ->
            checkShowing "clip on the head with the camera tipped" (show placed) $
                let V3 _ y _ = middle placed in abs (y - dot (V3 0 (-100) (-100)) tipped) < 1e-3

    -- How the clip sits on the head: the standard way, which turns
    -- nothing; mounted otherwise and back; and the prior pivot following it.
    check "the standard mounting turns nothing" $
        let m = mounted standardMounting headset in all (uncurry closeTo) [(mLeft m, mLeft headset), (mRight m, mRight headset), (mTop m, mTop headset)]
    let sideways = Mounting (rotYXZ 0 0 90) (V3 (-60) 100 60)
        remounted = mounted standardMounting (mounted sideways headset)
    check "a clip mounted otherwise and back is as it was" $
        all (uncurry closeTo) [(mLeft remounted, mLeft headset), (mRight remounted, mRight headset), (mTop remounted, mTop headset)]
    check "the pivot looked for where the mounting puts it" $
        closeTo (appliedPivot (newLearner (mounted sideways headset) defaultCalibration)) (V3 (-60) 100 60)
            && closeTo (appliedPivot (newLearner headset defaultCalibration)) (V3 0 100 100)

    -- A pivot learnt for a clip mounted otherwise loads as it was saved,
    -- start after start.
    let sidewaysClip = mounted sideways headset
        restarted c = parseCalibration (mMount sidewaysClip) (renderCalibration (learnerCalibration (newLearner sidewaysClip c)))
        savedSideways = parseCalibration (mMount sidewaysClip) "up = 0 -1 0\nturning = 900\npivot = -50 120 70\npivot windows = 40\npivot information = 4 0.5 0 2 0 3\n"
    checkShowing "a pivot of a clip mounted otherwise, loaded again and again" (show (estimatedPivot <$> (restarted =<< restarted =<< savedSideways))) $
        either (const False) (closeV (V3 (-50) 120 70) . estimatedPivot . learnerCalibration . newLearner sidewaysClip) (restarted =<< restarted =<< savedSideways)

    -- A calibration saved by earlier builds, whose information was the
    -- windows' normal matrices, loads with its pivot as it was, but is taken
    -- for no surer than a tenth of what is now remembered.
    let older = "up = 0 -0.9 0.4\nturning = 3000\npivot = 3 192 26\npivot windows = 160\npivot information = 400 10 5 30 2 380\n"
    case parseCalibration standardMounting older of
        Left err -> checkShowing "an older calibration loads" (err) False
        Right loaded -> do
            check "an older calibration's pivot" $ closeV (estimatedPivot loaded) (V3 3 192 26)
            checkShowing "an older calibration's pivot is taken as unsure" (show [pivotSpreadAlong loaded u | u <- [V3 1 0 0, V3 0 1 0, V3 0 0 1]]) $
                all (\u -> pivotSpreadAlong loaded u > 9) [V3 1 0 0, V3 0 1 0, V3 0 0 1]
    check "empty calibration file" $ fmap renderCalibration (parseCalibration standardMounting "") == Right (renderCalibration defaultCalibration)
    check "bad up" $ isLeft (parseCalibration standardMounting "up = 0 0 0\n")
    check "bad line" $ isLeft (parseCalibration standardMounting "# fine\nnonsense\n")
    check "unknown calibration setting" $ either id (const "") (parseCalibration standardMounting "tilt = 3\n") == "line 1: unknown setting \"tilt\""
    tree properties

properties :: TestTree
properties =
    testGroup
        "properties"
        [ testProperty "a calibration file is loaded as it says" $
            \file ->
                case parseCalibration standardMounting (savedText file) of
                    Left err -> counterexample err False
                    Right c ->
                        conjoin
                            [ case (savedUp file, savedTurning file) of
                                (Just up, Just turning) | turning > 0 -> counterexample (show (levelUp (estimatedLevel c))) (property (angleBetween (levelUp (estimatedLevel c)) up < 1e-9))
                                _ -> property True
                            , maybe (property True) (\turning -> levelTurning c === min 1800 turning) (savedTurning file)
                            , maybe (property True) (Gen.closeV 1e-6 (estimatedPivot c)) (savedPivot file)
                            , maybe (property True) (pivotTurns c ===) (savedWindows file)
                            ]
        , testProperty "a calibration saved, loaded and saved again is saved the same" $
            \file ->
                case parseCalibration standardMounting (savedText file) of
                    Left err -> counterexample err False
                    Right c ->
                        let settings = map (unwords . map unsigned . words) . filter ((/= "#") . take 1) . lines
                            -- A number that rounds to nothing from below is written so.
                            unsigned number = if number == "-0" then "0" else number
                         in fmap (settings . renderCalibration) (parseCalibration standardMounting (renderCalibration c)) === Right (settings (renderCalibration c))
        ]

-- | What a calibration file says: up, the turning it was learnt from, the
-- pivot, the windows it was learnt from, and the information about it.
data Saved = Saved
    { savedUp :: Maybe V3
    , savedTurning :: Maybe Double
    , savedPivot :: Maybe V3
    , savedWindows :: Maybe Double
    , savedInformation :: Maybe [Double]
    }
    deriving (Show)

savedText :: Saved -> String
savedText file =
    unlines . concat $
        [ ["up = " ++ numbers [x, y, z] | Just (V3 x y z) <- [savedUp file]]
        , ["turning = " ++ numbers [w] | Just w <- [savedTurning file]]
        , ["pivot = " ++ numbers [x, y, z] | Just (V3 x y z) <- [savedPivot file]]
        , ["pivot windows = " ++ numbers [w] | Just w <- [savedWindows file]]
        , ["pivot information = " ++ numbers info | Just info <- [savedInformation file]]
        ]
  where
    numbers = unwords . map renderNumber

-- | A file as trackkat writes one, or with some of its lines gone: up
-- within 40 degrees of the camera's, a pivot within 300 mm of the clip,
-- and information that tells the pivot in every direction. Simplest with
-- every line gone, and simpler with one gone.
instance Arbitrary Saved where
    arbitrary =
        Saved
            <$> perhaps ((\turn -> writtenV (mulMV (rodrigues turn) (V3 0 (-1) 0))) <$> genTurn 40)
            <*> perhaps (genWritten (0, 50000000))
            <*> perhaps (V3 <$> mm <*> mm <*> mm)
            <*> perhaps (written . (* 10000) <$> chooseInt (0, 500))
            <*> perhaps information
      where
        perhaps g = frequency [(1, pure Nothing), (4, Just <$> g)]
        mm = genWritten (-3000000, 3000000)
        writtenV (V3 x y z) = V3 (rounded x) (rounded y) (rounded z)
        rounded x = written (round (x * 10000))
        information = (\a b d f g j -> [a, b, d, f, g, j]) <$> diagonal <*> off <*> off <*> diagonal <*> off <*> diagonal
        diagonal = genWritten (10000, 5000000)
        off = genWritten (-3000, 3000)
    shrink =
        shrinkParts
            [ \file -> [file{savedUp = Nothing} | Just _ <- [savedUp file]]
            , \file -> [file{savedTurning = Nothing} | Just _ <- [savedTurning file]]
            , \file -> [file{savedPivot = Nothing} | Just _ <- [savedPivot file]]
            , \file -> [file{savedWindows = Nothing} | Just _ <- [savedWindows file]]
            , \file -> [file{savedInformation = Nothing} | Just _ <- [savedInformation file]]
            ]

-- | A camera tipped 13 degrees down and 4 to its right.
tilt :: M33
tilt = rodrigues (V3 0 0 (deg 4)) `mulMM` rodrigues (V3 (deg 13) 0 0)

-- | The pose that camera sees of a head turned and placed as given in level
-- axes.
seenFrom :: M33 -> V3 -> Pose
seenFrom rLevel tLevel = Pose (tilt `mulMM` rLevel) (mulMV tilt tLevel)

simulations :: TestTree
simulations = group "Calibration" $ do
    -- Learning from everyday movement: the camera tipped 13 degrees down and
    -- 4 to its right watches a head turn about a neck pivot, with a little
    -- noise in every pose. Ninety seconds of glancing left and right while
    -- slowly nodding, then twenty of nodding alone.
    let truePivot = V3 (-60) 90 70
        neckAt = V3 0 50 800
        -- The head turned as given, its pivot at the given place in level axes.
        headAt place y p r = seenFrom (rotYXZ y p r) (place ^-^ mulMV (rotYXZ y p r) truePivot)
        noisy i (Pose r t) =
            Pose
                (rodrigues (V3 (deg 0.2 * noise i 1) (deg 0.2 * noise i 2) (deg 0.2 * noise i 3)) `mulMM` r)
                (t ^+^ V3 (noise i 4) (noise i 5) (2 * noise i 6))
        glancing sway =
            [ (time, noisy i (headAt (neckAt ^+^ sway time) y p 0))
            | i <- [0 .. 120 * 110 :: Int]
            , let time = fromIntegral i / 120 :: Double
                  (y, p)
                    | time < 90 = (35 * sin (2 * pi * time / 2.5), 8 * sin (2 * pi * time / 7))
                    | otherwise = (0, 15 * sin (2 * pi * time / 2))
            ]
        runLearner = runLearnerWith headset
        (learnt, refinements) = runLearner (glancing (const (V3 0 0 0)))
        c = learnerCalibration learnt
        (tiltDown, tiltRight) = cameraTilt (estimatedLevel c)
        (appliedDown, appliedRight) = cameraTilt (appliedLevel learnt)
        straight = headAt neckAt 0 0 0
    checkShowing "learnt level" (show (tiltDown, tiltRight)) $ abs (tiltDown - 13) < 0.5 && abs (tiltRight - 4) < 0.5
    checkShowing "applied level" (show (appliedDown, appliedRight)) $ abs (appliedDown - 13) < 0.5 && abs (appliedRight - 4) < 0.5
    checkShowing "learnt pivot" (show (estimatedPivot c, pivotTurns c)) $ norm (estimatedPivot c ^-^ truePivot) < 5
    checkShowing "applied pivot" (show (appliedPivot learnt)) $ norm (appliedPivot learnt ^-^ truePivot) < 5
    checkShowing "refinements reported" (show (length refinements)) $
        LevelRefined `elem` refinements && PivotRefined `elem` refinements
    forM_ [(30, 0), (-40, 0), (0, 15), (25, -10)] $ \(y, p) -> do
        let hp = relativePose (appliedLevel learnt) (appliedPivot learnt) straight (headAt neckAt y p 0)
            moved = norm (V3 (tx hp) (ty hp) (tz hp))
        checkShowing "turning alone doesn't move" (show (y, p) ++ " got " ++ show hp) $
            moved < 3 && abs (yaw hp - y) < 0.5 && abs (pitch hp - p) < 0.5 && abs (roll hp) < 0.5
    let unlearnt = relativePose cameraLevel (appliedPivot (newLearner headset defaultCalibration)) straight (headAt neckAt 30 0 0)
    checkShowing "without learning, turning moves" (show unlearnt) $ norm (V3 (tx unlearnt) (ty unlearnt) (tz unlearnt)) > 20
    checkShowing "level described" (describeLevel c) $ take 14 (describeLevel c) == "Level learned:"
    checkShowing "pivot described" (describePivot (Just straight) c) $ take 19 (describePivot (Just straight) c) == "Neck pivot learned:"
    -- Nothing learnt, as after clearing the calibration, is said so.
    forM_ [Nothing, Just straight] $ \centre ->
        checkShowing "nothing learnt described" (show centre) $
            [describeLevel defaultCalibration, describePivot centre defaultCalibration]
                == ["Level: not learned yet; turn your head left and right.", "Neck pivot: not learned yet; turn your head left and right."]
    -- The window's shorter lines say the same, and fit its width.
    checkShowing "level described briefly" (describeLevelBriefly c) $
        take 14 (describeLevelBriefly c) == "Camera tipped " && length (describeLevelBriefly c) <= 90
    checkShowing "pivot described briefly" (describePivotBriefly (Just straight) c) $
        take 11 (describePivotBriefly (Just straight) c) == "Neck pivot " && '±' `elem` describePivotBriefly (Just straight) c && length (describePivotBriefly (Just straight) c) <= 95
    forM_ [Nothing, Just straight] $ \centre ->
        checkShowing "nothing learnt described briefly" (show centre) $
            [describeLevelBriefly defaultCalibration, describePivotBriefly centre defaultCalibration]
                == [describeLevel defaultCalibration, describePivot centre defaultCalibration]
    -- A clip worn rolled 15 degrees sees a pivot square below and behind the
    -- head off to one side in its own axes; described at the centre, it is
    -- square again.
    let worn = rotYXZ 0 0 15
        inClip = mulMV (transpose worn) (V3 0 150 60)
    checkShowing "pivot in clip axes is off to one side" (show inClip) $ let V3 x _ _ = inClip in abs x > 30
    check "pivot described square to the head" $ norm (inLevelAxes cameraLevel (Pose worn (V3 0 0 700)) inClip ^-^ V3 0 150 60) < 1e-9

    -- Swaying the body while glancing mustn't drag the pivot far.
    let (swaying, _) = runLearner (glancing (\time -> V3 (30 * sin (2 * pi * time / 3.7)) 0 (20 * sin (2 * pi * time / 5.3))))
    checkShowing "pivot while swaying" (show (estimatedPivot (learnerCalibration swaying))) $
        norm (estimatedPivot (learnerCalibration swaying) ^-^ truePivot) < 15

    -- How high the pivot is, along the axis of turning left and right, is
    -- shown only by nods and tilts (see Note [Learning the pivot]). Turning
    -- alone for twenty seconds shows where the axis is, but leaves the
    -- height where most heads have it, and says it is unsure of it.
    let headAbout pivot place y p r = seenFrom (rotYXZ y p r) (place ^-^ mulMV (rotYXZ y p r) pivot)
        turningOnly = learnerCalibration (fst (runLearner [(time, noisy i (headAt neckAt (35 * sin (2 * pi * time / 2.5)) 0 0)) | i <- [0 .. 120 * 20 :: Int], let time = fromIntegral i / 120]))
        V3 axisX axisHeight axisZ = estimatedPivot turningOnly
        V3 _ priorHeight _ = estimatedPivot defaultCalibration
        V3 trueX _ trueZ = truePivot
    checkShowing "turning alone finds the axis" (show (estimatedPivot turningOnly)) $ abs (axisX - trueX) < 5 && abs (axisZ - trueZ) < 5
    check "but not the height on it" $ abs (axisHeight - priorHeight) < 5
    checkShowing "and knows it" (describePivotBriefly (Just straight) turningOnly) $
        pivotSpreadAlong turningOnly (V3 0 1 0) > 30 && pivotSpreadAlong turningOnly (V3 1 0 0) < 10 && pivotSpreadAlong turningOnly (V3 0 0 1) < 10
    -- Later evidence moves what was learnt: ten minutes of glancing and
    -- nodding about one pivot, then three about one 50 mm lower, as when
    -- the clip has moved up on the headset, and the lower one is learnt.
    let lower = truePivot ^+^ V3 0 50 0
        movedOn =
            [ (time, noisy i (headAbout (if time < 600 then truePivot else lower) neckAt (30 * sin (2 * pi * time / 2.5)) (12 * sin (2 * pi * time / 1.7)) 0))
            | i <- [0 .. 60 * 780 :: Int]
            , let time = fromIntegral i / 60 :: Double
            ]
        moved = learnerCalibration (fst (runLearner movedOn))
    checkShowing "a pivot that moved is learnt again" (show (estimatedPivot moved)) $ norm (estimatedPivot moved ^-^ lower) < 8

    -- Saving and loading.
    let saved = renderCalibration c
        closeV a b = norm (a ^-^ b) < 1e-3
        reloaded = parseCalibration standardMounting saved
        loadedSo holds = either (const False) holds reloaded
    checkShowing "calibration loads" (either id (const "") reloaded) $ loadedSo (const True)
    check "calibration round-trips" $ loadedSo $ \loaded -> renderCalibration loaded == saved
    check "loaded level" $ loadedSo $ \loaded -> angleBetween (levelUp (estimatedLevel loaded)) (levelUp (estimatedLevel c)) < 1e-3
    check "loaded pivot" $ loadedSo $ \loaded -> closeV (estimatedPivot loaded) (estimatedPivot c)
    check "loaded evidence" $ loadedSo $ \loaded -> abs (levelTurning loaded - levelTurning c) < 1e-3 && abs (pivotTurns loaded - pivotTurns c) < 1e-3
    check "loaded calibration applies at once" $ loadedSo $ \loaded -> closeV (appliedPivot (newLearner headset loaded)) (estimatedPivot c)

    -- Learning from mirror-image poses can't put the pivot in front of the
    -- clip, where it seems to be on them.
    let mirroredPoses = [(t, mp) | (t, truth, _) <- slipping, Just mp <- [mirrorPose defaultIntrinsics clipBent truth]]
        mirrorLearnt = learnerCalibration (fst (runLearnerWith clipBent mirroredPoses))
    checkShowing "pivot not learnt from mirror images" (show (estimatedPivot mirrorLearnt)) $ plausibleCalibration clipBent mirrorLearnt

    -- A saved pivot in front of and above the clip was learnt from mirror
    -- images; it is forgotten on loading, and the level with it.
    let polluted = "up = 0 -0.9 0.4\nturning = 3000\npivot = 0 -60 -40\npivot windows = 120\npivot information = 500 0 0 500 0 500\n"
    check "a saved pivot in front of the clip is forgotten" $ case screenCalibration clipBent <$> parseCalibration standardMounting polluted of
        Right (Left _) -> True
        _ -> False
    check "a plausible saved pivot is kept" $ case screenCalibration headset <$> parseCalibration standardMounting saved of
        Right (Right kept) -> renderCalibration kept == saved
        _ -> False
