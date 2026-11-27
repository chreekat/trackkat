{-# LANGUAGE LambdaCase #-}

-- | The live view across the window, with window = haskell: the head view
-- in its corner ("TrackKat.UI.HeadView"), and beside it the axes, lights,
-- figures and switches ("TrackKat.UI.Live"). The switches and head view are
-- cbits/gui.c's buttons, drawn here.
module TrackKat.Gui.Live (registerLive) where

#include <windows.h>

import Data.Bits ((.&.))
import Foreign.C.String (CWString, peekCWString)
import Foreign.C.Types (CFloat (..), CInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Ptr (Ptr, nullPtr, wordPtrToPtr)
import Foreign.Storable (peek, peekByteOff)
import Graphics.Win32.GDI.Types (HDC, HWND)
import Graphics.Win32.Window (WindowClosure, allocaPAINTSTRUCT, beginPaint, defWindowProc, endPaint, getClientRect, getParent, sendMessage)
import System.Win32.Types (HANDLE, LPCTSTR, UINT)

import TrackKat.Config (HeadView (..))
import qualified TrackKat.Display as Display
import TrackKat.Gui.Paint
import TrackKat.Linear (V3 (..))
import TrackKat.Pose (HeadPose (..), Reflector (..))
import TrackKat.Response (Aiming (..))
import TrackKat.Trackers (BlobRole (..))
import TrackKat.UI.Draw (Shape (..), scaledAt)
import TrackKat.UI.HeadView
import TrackKat.UI.Live
import TrackKat.UI.Tray (ClipSight (..), Holding (..))

foreign import ccall unsafe "hts_gui_head_state"
    c_head_state
        :: Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> IO ()
foreign import ccall unsafe "hts_gui_live_state"
    c_live_state :: CWString -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CInt -> Ptr CInt -> CWString -> CWString -> IO ()
foreign import ccall unsafe "hts_gui_live_control" c_live_control :: HWND -> IO CInt
foreign import ccall unsafe "windows.h SetCursor" c_SetCursor :: HANDLE -> IO HANDLE
foreign import ccall unsafe "windows.h LoadCursorW" c_LoadCursor :: Ptr () -> LPCTSTR -> IO HANDLE

-- | Register the live view's window class, reporting what fails in it.
registerLive :: (String -> IO ()) -> IO (Either Int ())
registerLive report = registerPart report "The live view" "trackkat_live" liveProc

-- | One of the live view's controls drawn here.
data Control = SwitchControl Switch | HeadViewControl

control :: HWND -> IO (Maybe Control)
control w =
    c_live_control w >>= \i -> pure $ case i of
        4 -> Just HeadViewControl
        _ | i >= 0 && i < 4 -> Just (SwitchControl (toEnum (fromIntegral i)))
        _ -> Nothing

liveProc :: WindowClosure
liveProc w msg wp lp
    | msg == #{const WM_ERASEBKGND} = pure 1
    | msg == #{const WM_PAINT} = do
        allocaPAINTSTRUCT $ \ps -> do
            dc <- beginPaint w ps
            (_, _, right, bottom) <- getClientRect w
            dpi <- screenDpi
            (_, live) <- state
            buffered dc (0, 0, fromIntegral right, fromIntegral bottom) $ \mem ->
                mapM_ (draw mem) (livePane dpi (fromIntegral right, fromIntegral bottom) live)
            endPaint w ps
        pure 0
    | msg == #{const WM_DRAWITEM} = do
        let item = wordPtrToPtr (fromIntegral lp)
        #{peek DRAWITEMSTRUCT, hwndItem} item >>= control >>= mapM_ (drawItem item)
        pure 1
    -- A hand over the head view, which a click switches.
    | msg == #{const WM_SETCURSOR} =
        control (wordPtrToPtr (fromIntegral wp)) >>= \case
            Just HeadViewControl -> 1 <$ (c_SetCursor =<< c_LoadCursor nullPtr (wordPtrToPtr #{const IDC_HAND}))
            _ -> defWindowProc (Just w) msg wp lp
    -- The switches' and head view's clicks are the window's to act on.
    | msg == #{const WM_COMMAND} = getParent w >>= \parent -> sendMessage parent msg wp lp
    | otherwise = defWindowProc (Just w) msg wp lp

-- | A switch or the head view, with a focus rectangle while it has the
-- keyboard. The head view is drawn off screen first, so it doesn't flicker.
drawItem :: Ptr () -> Control -> IO ()
drawItem item which = do
    dc <- #{peek DRAWITEMSTRUCT, hDC} item :: IO HDC
    left <- #{peek DRAWITEMSTRUCT, rcItem.left} item :: IO CInt
    top <- #{peek DRAWITEMSTRUCT, rcItem.top} item
    right <- #{peek DRAWITEMSTRUCT, rcItem.right} item
    bottom <- #{peek DRAWITEMSTRUCT, rcItem.bottom} item
    itemState <- #{peek DRAWITEMSTRUCT, itemState} item :: IO UINT
    dpi <- screenDpi
    (scene, live) <- state
    let focused = itemState .&. #{const ODS_FOCUS} /= 0 && itemState .&. #{const ODS_NOFOCUSRECT} == 0
        s = scaledAt dpi
        (l, t, r, b) = (fromIntegral left, fromIntegral top, fromIntegral right, fromIntegral bottom)
    case which of
        HeadViewControl ->
            buffered dc (left, top, right - left, bottom - top) $ \mem ->
                mapM_ (draw mem) $
                    headScene dpi (min (r - l) (b - t)) scene
                        ++ [Focus (s 2, s 2, r - l - s 2, b - t - s 2) | focused]
        SwitchControl sw ->
            mapM_ (draw dc) $
                switchFace dpi (l, t, r, b) (if itemState .&. #{const ODS_SELECTED} /= 0 then Pressed else Released) live sw
                    ++ [Focus (l + s 4, t + s 4, r - s 4, b - s 4) | focused]

-- | What cbits/gui.c last saw for the live view.
state :: IO (HeadScene, LiveState)
state =
    alloca $ \view -> alloca $ \camera -> alloca $ \blobs -> alloca $ \seenBits -> alloca $ \paused ->
        allocaArray 6 $ \headP -> allocaArray 6 $ \poseP -> alloca $ \clipShown -> allocaArray 9 $ \clipP ->
            alloca $ \nSpots -> allocaArray (7 * blobsMax) $ \spotP -> alloca $ \nAreas -> allocaArray (3 * areasMax) $ \areaP ->
                allocaArray textMax $ \cameraText -> allocaArray 2 $ \heldP -> alloca $ \trimmed -> allocaArray 2 $ \trimP ->
                    alloca $ \trimMoves -> alloca $ \aiming -> alloca $ \games -> allocaArray 64 $ \gameName ->
                        allocaArray textMax $ \stats -> do
                            c_head_state view camera blobs seenBits paused headP poseP clipShown clipP nSpots spotP nAreas areaP
                            c_live_state cameraText heldP trimmed trimP trimMoves aiming games gameName stats
                            v <- peek view
                            cam <- peek camera
                            b <- peek blobs
                            bits <- peek seenBits
                            p <- peek paused
                            hd <- headPose <$> floats 6 headP
                            pose <- headPose <$> floats 6 poseP
                            shown <- peek clipShown
                            clip <- floats 9 clipP
                            spots <- peek nSpots >>= \n -> floats (7 * fromIntegral n) spotP
                            areas <- peek nAreas >>= \n -> floats (3 * fromIntegral n) areaP
                            why <- peekCWString cameraText
                            held <- floats 2 heldP
                            tr <- peek trimmed
                            trim <- floats 2 trimP
                            moves <- peek trimMoves
                            aim <- peek aiming
                            g <- peek games
                            name <- peekCWString gameName
                            figures <- peekCWString stats
                            let holding = if p /= 0 then ViewHeld else ViewLive
                                cameraOn = cam == cameraTracking
                                pair xs = case xs of [x, y] -> (x, y); _ -> (0, 0)
                            pure
                                ( HeadScene
                                    { sceneView = case v of 1 -> PhysicalView; 2 -> CameraView; _ -> VirtualView
                                    , sceneSight = if cameraOn then Tracking (if b < 0 then ClipInSight else ClipOutOfSight) else NoTracking
                                    , sceneHead = hd
                                    , scenePose = pose
                                    , sceneHolding = holding
                                    , sceneMissing = [which | (which, bit) <- reflectorBits, bits .&. bit == 0]
                                    , sceneSpots = spot <$> chunks 7 spots
                                    , sceneAreas = [Area x y r | [x, y, r] <- chunks 3 areas]
                                    , sceneClip = case clip of
                                        [lx, ly, lz, rx, ry, rz, tx', ty', tz'] | shown /= 0 -> Just (V3 lx ly lz, V3 rx ry rz, V3 tx' ty' tz')
                                        _ -> Nothing
                                    }
                                , LiveState
                                    { liveCamera = case cam of 1 -> Display.Tracking; 0 -> Display.Searching; _ -> Display.CameraDown why
                                    , liveClip = if b < 0 then ClipSeen else ClipLost (fromIntegral b)
                                    , liveHead = hd
                                    , liveView = pose
                                    , liveHolding = holding
                                    , liveHeld = pair held
                                    , liveTrim =
                                        if tr /= 0
                                            then let (y, pt) = pair trim in Trimmed y pt (if moves /= 0 then MovesToo else TurnsOnly)
                                            else NotTrimmed
                                    , liveAiming = if aim /= 0 then Aiming else NotAiming
                                    , liveGames = fromIntegral g
                                    , liveGameName = if null name then Nothing else Just name
                                    , liveStats = figures
                                    }
                                )
  where
    floats n ptr = map realToFrac <$> peekArray n (ptr :: Ptr CFloat) :: IO [Double]
    headPose [a, b, c, d, e, f] = HeadPose a b c d e f
    headPose _ = HeadPose 0 0 0 0 0 0
    spot [x, y, pixels, role, vx, vy, cxy] = Spot x y pixels (blobRole role) vx vy cxy
    spot _ = Spot 0 0 0 StrayBlob 0 0 0
    chunks n xs = case splitAt n xs of
        (c, []) -> [c | not (null c)]
        (c, rest) -> c : chunks n rest
    -- As SPOT_REFLECTOR and the rest in cbits/gui.c.
    blobRole :: Double -> BlobRole
    blobRole 0 = ReflectorBlob
    blobRole 1 = StrayBlob
    blobRole _ = MaskedBlob
    -- As REFLECTOR_LEFT and the rest, and CAMERA_TRACKING.
    reflectorBits = [(LeftReflector, 1), (RightReflector, 2), (TopReflector, 4 :: CInt)]
    cameraTracking = 1
    -- As BLOBS_MAX, AREAS_MAX and TEXT_MAX.
    blobsMax = 16
    areasMax = 8
    textMax = 512
