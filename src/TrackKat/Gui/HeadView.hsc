-- | The live view across the window, with window = haskell: the head view
-- in its corner is what "TrackKat.UI.HeadView" says it shows; the axes,
-- lights and switches beside it are still cbits/gui.c's.
module TrackKat.Gui.HeadView (registerLive) where

#include <windows.h>

import Data.Bits ((.&.))
import Foreign.C.Types (CFloat (..), CInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Ptr (Ptr, nullPtr, wordPtrToPtr)
import Foreign.Storable (peek, peekByteOff)
import Graphics.Win32.GDI.Types (HDC, HWND, LPRECT, allocaRECT, pokeRECT)
import Graphics.Win32.Window (WindowClosure, allocaPAINTSTRUCT, beginPaint, defWindowProc, endPaint, getClientRect, getParent, sendMessage)
import System.Win32.Types (BOOL, DWORD, HANDLE, LPCTSTR, UINT)

import TrackKat.Config (HeadView (..))
import TrackKat.Gui.Paint
import TrackKat.Linear (V3 (..))
import TrackKat.Pose (HeadPose (..), Reflector (..))
import TrackKat.Trackers (BlobRole (..))
import TrackKat.UI.HeadView
import TrackKat.UI.Tray (ClipSight (..), Holding (..))

foreign import ccall unsafe "hts_gui_head_state"
    c_head_state
        :: Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CFloat -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> Ptr CInt -> Ptr CFloat -> IO ()
foreign import ccall unsafe "hts_gui_is_head_view" c_is_head_view :: HWND -> IO CInt
foreign import ccall unsafe "hts_gui_paint_live" c_paint_live :: HDC -> CInt -> CInt -> IO ()
foreign import ccall unsafe "hts_gui_draw_switch" c_draw_switch :: Ptr () -> IO ()
foreign import ccall unsafe "windows.h SetCursor" c_SetCursor :: HANDLE -> IO HANDLE
foreign import ccall unsafe "windows.h LoadCursorW" c_LoadCursor :: Ptr () -> LPCTSTR -> IO HANDLE
foreign import ccall unsafe "windows.h SetTextColor" c_SetTextColor :: HDC -> DWORD -> IO DWORD
foreign import ccall unsafe "windows.h SetBkColor" c_SetBkColor :: HDC -> DWORD -> IO DWORD
foreign import ccall unsafe "windows.h DrawFocusRect" c_DrawFocusRect :: HDC -> LPRECT -> IO BOOL

-- | Register the live view's window class, reporting what fails in it.
registerLive :: (String -> IO ()) -> IO (Either Int ())
registerLive report = registerPart report "The live view" "trackkat_live" liveProc

liveProc :: WindowClosure
liveProc w msg wp lp
    | msg == #{const WM_ERASEBKGND} = pure 1
    | msg == #{const WM_PAINT} = do
        allocaPAINTSTRUCT $ \ps -> do
            dc <- beginPaint w ps
            (_, _, right, bottom) <- getClientRect w
            let (width, height) = (fromIntegral right, fromIntegral bottom)
            buffered dc (0, 0, width, height) (\mem -> c_paint_live mem width height)
            endPaint w ps
        pure 0
    | msg == #{const WM_DRAWITEM} = do
        let item = wordPtrToPtr (fromIntegral lp)
        control <- #{peek DRAWITEMSTRUCT, hwndItem} item
        isHead <- c_is_head_view control
        if isHead /= 0 then drawHeadView item else c_draw_switch item
        pure 1
    -- A hand over the head view, which a click switches.
    | msg == #{const WM_SETCURSOR} = do
        isHead <- c_is_head_view (wordPtrToPtr (fromIntegral wp))
        if isHead /= 0
            then 1 <$ (c_SetCursor =<< c_LoadCursor nullPtr (wordPtrToPtr #{const IDC_HAND}))
            else defWindowProc (Just w) msg wp lp
    -- The switches' and head view's clicks are the window's to act on.
    | msg == #{const WM_COMMAND} = getParent w >>= \parent -> sendMessage parent msg wp lp
    | otherwise = defWindowProc (Just w) msg wp lp

-- | The head view, with a focus rectangle while it has the keyboard.
drawHeadView :: Ptr () -> IO ()
drawHeadView item = do
    dc <- #{peek DRAWITEMSTRUCT, hDC} item
    left <- #{peek DRAWITEMSTRUCT, rcItem.left} item :: IO CInt
    top <- #{peek DRAWITEMSTRUCT, rcItem.top} item
    right <- #{peek DRAWITEMSTRUCT, rcItem.right} item
    bottom <- #{peek DRAWITEMSTRUCT, rcItem.bottom} item
    itemState <- #{peek DRAWITEMSTRUCT, itemState} item :: IO UINT
    let (w, h) = (right - left, bottom - top)
    dpi <- screenDpi
    scene <- headState
    s2 <- scaled 2
    buffered dc (left, top, w, h) $ \mem -> do
        mapM_ (draw mem) (headScene dpi (fromIntegral (min w h)) scene)
        let focused = itemState .&. #{const ODS_FOCUS} /= 0 && itemState .&. #{const ODS_NOFOCUSRECT} == 0
        if focused
            then allocaRECT $ \r -> do
                pokeRECT r (fromIntegral s2, fromIntegral s2, fromIntegral (w - s2), fromIntegral (h - s2))
                _ <- c_SetTextColor mem 0
                _ <- c_SetBkColor mem 0xFFFFFF
                () <$ c_DrawFocusRect mem r
            else pure ()

-- | What cbits/gui.c last saw for the head view.
headState :: IO HeadScene
headState =
    alloca $ \view -> alloca $ \camera -> alloca $ \blobs -> alloca $ \seenBits -> alloca $ \paused ->
        allocaArray 6 $ \headP -> allocaArray 6 $ \poseP -> alloca $ \clipShown -> allocaArray 9 $ \clipP ->
            alloca $ \nSpots -> allocaArray (7 * blobsMax) $ \spotP -> alloca $ \nAreas -> allocaArray (3 * areasMax) $ \areaP -> do
                c_head_state view camera blobs seenBits paused headP poseP clipShown clipP nSpots spotP nAreas areaP
                v <- peek view
                cam <- peek camera
                b <- peek blobs
                bits <- peek seenBits
                p <- peek paused
                hd <- floats 6 headP
                pose <- floats 6 poseP
                shown <- peek clipShown
                clip <- floats 9 clipP
                spots <- peek nSpots >>= \n -> floats (7 * fromIntegral n) spotP
                areas <- peek nAreas >>= \n -> floats (3 * fromIntegral n) areaP
                pure
                    HeadScene
                        { sceneView = case v of 1 -> PhysicalView; 2 -> CameraView; _ -> VirtualView
                        , sceneSight = if cam /= cameraTracking then NoTracking else Tracking (if b < 0 then ClipInSight else ClipOutOfSight)
                        , sceneHead = headPose hd
                        , scenePose = headPose pose
                        , sceneHolding = if p /= 0 then ViewHeld else ViewLive
                        , sceneMissing = [which | (which, bit) <- reflectorBits, bits .&. bit == 0]
                        , sceneSpots = spot <$> chunks 7 spots
                        , sceneAreas = [Area x y r | [x, y, r] <- chunks 3 areas]
                        , sceneClip = case clip of
                            [lx, ly, lz, rx, ry, rz, tx', ty', tz'] | shown /= 0 -> Just (V3 lx ly lz, V3 rx ry rz, V3 tx' ty' tz')
                            _ -> Nothing
                        }
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
    -- As BLOBS_MAX and AREAS_MAX.
    blobsMax = 16
    areasMax = 8
