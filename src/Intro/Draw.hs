module Intro.Draw where

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL hiding (get)
import Graphics.Rendering.OpenGL (vertex, clear, ClearBuffer(ColorBuffer), PrimitiveMode(Points), renderPrimitive)
import State hiding (rndFloat)
import Utils.Math
import Utils.Rendering
import Control.Monad.State
import Utils.Font


drawStars :: [Star] -> Time -> IO ()
drawStars particles time = do
    renderPrimitive Points $ forM_ particles $ \s -> do
        let Star (Vector2d x y) spawnTime = s

        let c = color (time - spawnTime)
        GL.color $ gray c

        let z = 10.0 - (time - spawnTime)
        vertex (toVertex $ Vector2d (x/z) (y/z))

    where
    color t
        | t < 2.0 = 0.65*t/2.0
        | t > 9.0 = 0.65*(10-t)
        | otherwise = 0.65


draw :: GLFW.Window -> IntroState -> IO ()
draw window introState = do
    clear [ColorBuffer]

    centerText 0.3 (Vector2d (-1.0) (-0.0)) (Vector2d 1.0 0.3) "Asteroids"
    centerText 0.1 (Vector2d (-1.0) (-0.5)) (Vector2d 1.0 0.3) "Press enter to start"

    drawStars (is_stars introState) (is_time introState)

    return ()
