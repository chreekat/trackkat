-- | The few C calls the window makes itself, around ImGui and GLFW:
-- ImGui's OpenGL 3 renderer (see cbits/imgui_opengl3.cpp), clearing the
-- frame, and the clipboard.
module TrackKat.Window.GL (
    rendererInit,
    rendererNewFrame,
    renderDrawData,
    clearFrame,
    setClipboard,
) where

import Bindings.GLFW (C'GLFWwindow, c'glfwSetClipboardString)
import qualified Data.Text as T
import Foreign.C.Types (CFloat (..), CInt (..), CUInt (..))
import Foreign.Ptr (Ptr)
import qualified Graphics.UI.GLFW as GLFW
import Graphics.UI.GLFW.C (toC)

import DearImGui.Internal.Text (withCString)
import DearImGui.Raw (DrawData (..))

foreign import ccall unsafe "trackkat_opengl3_init" c_init :: IO CInt
foreign import ccall unsafe "trackkat_opengl3_new_frame" rendererNewFrame :: IO ()
foreign import ccall unsafe "trackkat_opengl3_render" c_render :: Ptr () -> IO ()

-- | Start ImGui's OpenGL 3 renderer, in the current OpenGL context; False
-- if it can't.
rendererInit :: IO Bool
rendererInit = (/= 0) <$> c_init

renderDrawData :: DrawData -> IO ()
renderDrawData (DrawData p) = c_render p

-- OpenGL 1.1, which every OpenGL library exports by name; on 64-bit
-- Windows its calling convention is C's.
foreign import ccall unsafe "glViewport" glViewport :: CInt -> CInt -> CInt -> CInt -> IO ()
foreign import ccall unsafe "glClearColor" glClearColor :: CFloat -> CFloat -> CFloat -> CFloat -> IO ()
foreign import ccall unsafe "glClear" glClear :: CUInt -> IO ()

-- | Clear a frame of a width and height in pixels to a colour, red, green
-- and blue from 0 to 1.
clearFrame :: Int -> Int -> (Float, Float, Float) -> IO ()
clearFrame w h (r, g, b) = do
    glViewport 0 0 (fromIntegral w) (fromIntegral h)
    glClearColor (realToFrac r) (realToFrac g) (realToFrac b) 1
    glClear colorBufferBit
  where
    colorBufferBit = 0x4000

-- | Put text on the clipboard. GLFW takes UTF-8, which GLFW-b's own
-- setClipboardString doesn't give it outside a UTF-8 locale.
setClipboard :: GLFW.Window -> String -> IO ()
setClipboard win s = withCString (T.pack s) (c'glfwSetClipboardString (toC win :: Ptr C'GLFWwindow))
