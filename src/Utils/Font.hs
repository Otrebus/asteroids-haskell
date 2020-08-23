module Font(chars, drawText, centerText) where
import Math
import qualified Graphics.Rendering.OpenGL as GL
import Control.Monad
import Data.Char (ord)
import Graphics.Rendering.OpenGL (vertex, clear, ClearBuffer(ColorBuffer), renderPrimitive, PrimitiveMode(Lines, Points, LineStrip))
import Chars (chars)


drawChar :: Float -> Vector2d -> Char -> IO Vector2d
drawChar size pos@(Vector2d x y) c = do
    let (w, vs) = chars !! (ord c - 32)
    let conv = (pos ^+^) . (size/32.0 !*^) . (uncurry Vector2d)

    GL.color $ GL.Color4 1 1 1 (1 :: GL.GLfloat)
    forM_ (map (map (toVertex . conv)) vs) $ renderPrimitive LineStrip <$> mapM_ vertex

    return $ Vector2d (x + (size*w)/32.0) y


drawText :: Float -> Vector2d -> String -> IO ()
drawText size pos = foldM_ (drawChar size) pos


centerText :: Float -> Vector2d -> Vector2d -> String -> IO ()
centerText size (Vector2d x1 y1) (Vector2d x2 y2) text = drawText size pos text
    where
        pos = Vector2d (x1 + (x2-x1)/2 - (size*textWidth text)/2) (y1 + (y2-y1)/2 - (size/2))
        textWidth = foldr ((+) <$> \c -> (fst $ chars !! (ord c - 32))/32.0) 0.0
