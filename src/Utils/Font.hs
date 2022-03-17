module Utils.Font(chars, drawText, centerText) where
import Utils.Math
import qualified Graphics.Rendering.OpenGL as GL
import Control.Monad
import Data.Char (ord)
import Graphics.Rendering.OpenGL (vertex, renderPrimitive, PrimitiveMode(LineStrip))
import Utils.Chars (chars)
import Utils.Rendering


-- Draws a single character on the screen
drawChar ::
    Float ->    -- The size of the character
    Vector2d -> -- The top left position of the character
    Char ->     -- The character to draw
    IO Vector2d -- The position where the next character would go
drawChar size pos@(Vector2d x y) c = do
    let (w, vs) = chars !! (ord c - 32)
    let conv = (pos ^+^) . (size/32.0 !*^) . (uncurry Vector2d)

    GL.color $ white
    forM_ (map (map (toVertex . conv)) vs) $ renderPrimitive LineStrip <$> mapM_ vertex

    return $ Vector2d (x + (size*w)/32.0) y


-- Draws text on the screen
drawText ::
    Float ->    -- The size of the text
    Vector2d -> -- The top left position of the text
    String ->   -- The string to type out
    IO ()
drawText size pos = foldM_ (drawChar size) pos


-- Draws text centered on a box
centerText ::
    Float ->    -- The size of the text
    Vector2d -> -- The top left of the box
    Vector2d -> -- The bottom right of the box
    String ->   -- The text
    IO ()
centerText size (Vector2d x1 y1) (Vector2d x2 y2) text = drawText size pos text
    where
        pos = Vector2d (x1 + (x2-x1)/2 - (size*textWidth text)/2) (y1 + (y2-y1)/2 - (size/2))
        textWidth = foldr ((+) <$> \c -> (fst $ chars !! (ord c - 32))/32.0) 0.0
