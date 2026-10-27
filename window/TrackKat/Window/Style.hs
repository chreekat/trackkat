{-# LANGUAGE PatternSynonyms #-}

-- | How trackkat's window looks: its colours, fonts and spacing, scaled
-- for the screen.
module TrackKat.Window.Style (
    Colour (..),
    rgb,
    vec4,
    u32,
    withAlpha,
    background,
    panel,
    box,
    grid,
    textColour,
    dim,
    accent,
    headTick,
    green,
    amber,
    red,
    grey,
    Fonts (..),
    loadFonts,
    baseFontSize,
    smallFontSize,
    withStyle,
) where

import Control.Exception (bracket_)
import Control.Monad (filterM)
import Data.Bits (shiftL, (.|.))
import Data.Foldable (traverse_)
import Data.Word (Word32, Word8)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

import DearImGui hiding (end, flags, size, spacing, w, x, y, z)
import qualified DearImGui.FontAtlas as FontAtlas
import qualified DearImGui.Raw.Font as Raw.Font

-- | A colour as red, green and blue bytes, and opacity from 0 to 1.
data Colour = Colour Word8 Word8 Word8 Float

rgb :: Word8 -> Word8 -> Word8 -> Colour
rgb r g b = Colour r g b 1

withAlpha :: Float -> Colour -> Colour
withAlpha a (Colour r g b _) = Colour r g b a

vec4 :: Colour -> ImVec4
vec4 (Colour r g b a) = ImVec4 (f r) (f g) (f b) a
  where
    f v = fromIntegral v / 255

-- | As ImGui packs a colour for drawing.
u32 :: Colour -> Word32
u32 (Colour r g b a) =
    (fromIntegral (round (a * 255) :: Int) `shiftL` 24)
        .|. (fromIntegral b `shiftL` 16)
        .|. (fromIntegral g `shiftL` 8)
        .|. fromIntegral r

-- | The window behind everything; the live panel; the boxes things are
-- drawn in; the lines that grid them.
background, panel, box, grid :: Colour
background = rgb 24 27 32
panel = rgb 30 34 40
box = rgb 38 43 51
grid = rgb 56 62 72

textColour, dim, accent, headTick :: Colour
textColour = rgb 222 226 232
dim = rgb 135 142 154
accent = rgb 90 170 255
headTick = rgb 255 200 80

-- | Status lights.
green, amber, red, grey :: Colour
green = rgb 80 200 120
amber = rgb 240 170 60
red = rgb 230 85 75
grey = rgb 120 126 138

data Fonts = Fonts
    { regular :: Font
    , bold :: Font
    , mono :: Font
    }

-- | The size of text, at 100% scaling, in pixels.
baseFontSize :: Float
baseFontSize = 15

-- | The size of notes and labels, at 100% scaling.
smallFontSize :: Float
smallFontSize = 13

-- | Load the system's interface, bold and fixed-width fonts, or ImGui's
-- own where they can't be found. ImGui renders glyphs at any size as
-- they are needed.
loadFonts :: IO Fonts
loadFonts = do
    windows <- maybe [] (\d -> [d </> "Fonts"]) <$> lookupEnv "WINDIR"
    let dirs = windows ++ ["/usr/share/fonts/opentype/inter", "/usr/share/fonts/truetype/dejavu", "/usr/share/fonts/TTF"]
        font names = do
            found <- filterM doesFileExist [d </> n | d <- dirs, n <- names]
            case found of
                path : _ -> FontAtlas.addFontFromFileTTF path Nothing Nothing
                [] -> pure Nothing
    r <- font ["segoeui.ttf", "Inter-Regular.otf", "DejaVuSans.ttf"]
    b <- font ["segoeuib.ttf", "Inter-SemiBold.otf", "DejaVuSans-Bold.ttf"]
    m <- font ["consola.ttf", "DejaVuSansMono.ttf"]
    fallback <- maybe Raw.Font.addFontDefaultVector pure r
    pure (Fonts fallback (maybe fallback id b) (maybe fallback id m))

-- | Draw in trackkat's colours and font, with spacing scaled by a factor:
-- 1 at 100% scaling.
withStyle :: Float -> Fonts -> IO a -> IO a
withStyle scale fonts body =
    bracket_ (traverse_ (\(i, c) -> pushStyleColor i (pure c :: IO ImVec4)) colours >> traverse_ (\(i, v') -> pushStyleVar i (pure v' :: IO ImVec2)) spacing) (popStyleColor (fromIntegral (length colours)) >> popStyleVar (length spacing)) $
        withFontWithSize (regular fonts) (realToFrac (baseFontSize * scale)) body
  where
    v a b = ImVec2 (a * scale) (b * scale)
    spacing =
        [ (ImGuiStyleVar_WindowPadding, v 14 12)
        , (ImGuiStyleVar_FramePadding, v 8 4)
        , (ImGuiStyleVar_ItemSpacing, v 8 7)
        , (ImGuiStyleVar_ItemInnerSpacing, v 6 4)
        , (ImGuiStyleVar_CellPadding, v 4 4)
        , (ImGuiStyleVar_SeparatorTextPadding, v 0 4)
        ]
    colours =
        map
            (fmap vec4)
            [ (ImGuiCol_WindowBg, background)
            , (ImGuiCol_ChildBg, withAlpha 0 background)
            , (ImGuiCol_PopupBg, rgb 44 49 58)
            , (ImGuiCol_Border, grid)
            , (ImGuiCol_Text, textColour)
            , (ImGuiCol_TextDisabled, dim)
            , (ImGuiCol_FrameBg, box)
            , (ImGuiCol_FrameBgHovered, rgb 48 55 66)
            , (ImGuiCol_FrameBgActive, rgb 56 64 77)
            , (ImGuiCol_Button, rgb 46 53 64)
            , (ImGuiCol_ButtonHovered, rgb 60 70 85)
            , (ImGuiCol_ButtonActive, rgb 54 96 140)
            , (ImGuiCol_CheckMark, accent)
            , (ImGuiCol_SliderGrab, rgb 110 125 145)
            , (ImGuiCol_SliderGrabActive, accent)
            , (ImGuiCol_Separator, grid)
            , (ImGuiCol_ScrollbarBg, withAlpha 0 background)
            , (ImGuiCol_ScrollbarGrab, rgb 56 62 72)
            , (ImGuiCol_ScrollbarGrabHovered, rgb 70 78 90)
            , (ImGuiCol_ScrollbarGrabActive, rgb 84 94 108)
            , (ImGuiCol_TextSelectedBg, withAlpha 0.35 accent)
            , (ImGuiCol_NavCursor, accent)
            , (ImGuiCol_TableBorderLight, grid)
            , (ImGuiCol_TableBorderStrong, grid)
            ]
