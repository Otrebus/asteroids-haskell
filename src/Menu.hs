module Menu where

import State
import Graphics.Rendering.OpenGL (ClearBuffer(ColorBuffer), clear)
import qualified Graphics.Rendering.OpenGL as GL hiding (get, rotate)
import Control.Monad.State
import Font
import Math
import Player
import Draw
import qualified Graphics.UI.GLFW as GLFW
import Debug.Trace


runFrame :: [GLFW.Key] -> State ProgramState ()
runFrame input = do
    
    state <- get

    let prevPressed = gls_keysPressed state
    let newDown = filter (\k -> not (k `elem` prevPressed)) input

    trace (show newDown) $ return ()

    case ((ms_menuChoice . gls_menuState) state) of
        Continue -> do
            when (GLFW.Key'D `elem` newDown) $ do
                put $ state { gls_menuState = (gls_menuState state) { ms_menuChoice = Quit } }
            when (GLFW.Key'Enter `elem` newDown || GLFW.Key'Space `elem` newDown) $ do
                put $ state { gls_mode = Playing }
        Quit -> do
            when (GLFW.Key'E `elem` newDown) $ do
                put $ state { gls_menuState = (gls_menuState state) { ms_menuChoice = Continue } }
            when (GLFW.Key'Enter `elem` newDown || GLFW.Key'Space `elem` newDown) $ do
                put $ state { gls_mode = Exiting }

    state <- get
    when (GLFW.Key'Escape `elem` newDown) $ do
        put $ state { gls_mode = Playing }



drawCursor :: Float -> Float -> IO ()
drawCursor x y = do
    drawPolygon (GL.Color4 0.00 0.00 0.00 1.0) (GL.Color4 1.0 1.0 1.0 1.0) pm
    where
        pm = map ((0.8 !*^) . ((Vector2d x y) ^+^)) (map (rotate (-pi/2)) (fst playerModel))


draw :: GLFW.Window -> MenuState -> IO ()
draw window menuState = do
    clear [ColorBuffer]

    centerText 0.1 (Vector2d (-0.3) (0.3)) (Vector2d 0.3 0.2) "Game Paused"
    drawText 0.08 (Vector2d (-0.23) (0.08)) "Continue"
    drawText 0.08 (Vector2d (-0.23) (-0.02)) "Quit"

    case (ms_menuChoice menuState) of
        Continue -> do
            drawCursor (-0.37) (0.04)
        Quit -> do
            drawCursor (-0.37) (-0.06)
