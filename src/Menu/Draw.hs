module Menu.Draw where

import Utils.Rendering
import State
import Graphics.Rendering.OpenGL (ClearBuffer(ColorBuffer), clear)
import Utils.Font
import Utils.Math
import Game.Draw -- TODO: move drawPolygon to utils
import qualified Graphics.UI.GLFW as GLFW
import Player


-- Draws the cursor next to selected menu item
drawCursor ::
    Float -> -- The x position of the cursor
    Float -> -- The y position of the cursor
    IO ()
drawCursor x y = do
    drawFigure white black (pm, [pm])
    where
        pm = map ((0.8 !*^) . ((Vector2d x y) ^+^)) (map (rotate (-pi/2)) (fst playerModel))


-- Draws the menu
draw ::
    MenuState ->   -- The state of the menu
    IO ()
draw menuState = do
    clear [ColorBuffer]

    -- Hard-coded, if this were to grow into an actual menu I'd do this differently
    centerText 0.1 (Vector2d (-0.3) (0.3)) (Vector2d 0.3 0.2) "Game Paused"
    drawText 0.08 (Vector2d (-0.23) (0.04)) "Continue"
    drawText 0.08 (Vector2d (-0.23) (-0.06)) "Quit"

    case (ms_menuChoice menuState) of
        Continue -> do
            drawCursor (-0.37) (0.08)
        Quit -> do
            drawCursor (-0.37) (-0.04)
