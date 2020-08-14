module Intro where

import qualified Graphics.UI.GLFW as GLFW
import State
import Graphics.Rendering.OpenGL (ClearBuffer(ColorBuffer), clear)
import Math
import Control.Monad.State
import Font


updateParticles :: Time -> Time -> [Vector3d] -> [Vector3d]
updateParticles time deltaT particles = particles


runFrame :: [GLFW.Key] -> State ProgramState ()
runFrame input = do
    
    state <- get

    let prevPressed = gls_keysPressed state
    let newDown = filter (\k -> not (k `elem` prevPressed)) input

    when (GLFW.Key'Enter `elem` newDown || GLFW.Key'Space `elem` newDown) $ do
        put $ state { gls_mode = Playing }

    let newState = state {
        gls_introState = execState runIntroFrame (gls_introState state)
    }

    put $ newState { gls_mode = if GLFW.Key'Escape `elem` newDown then Menu else Playing }

    let prevPressed = gls_keysPressed state
    let newDown = filter (\k -> not (k `elem` prevPressed)) input

    put $ newState { gls_mode = if GLFW.Key'Escape `elem` newDown then Menu else Playing }


runIntroFrame :: State IntroState ()
runIntroFrame = do
    state <- get
    let particles = updateParticles 0.0 0.0 (is_particles state)
    put $ state { is_particles = particles }
    
    return ()

draw :: GLFW.Window -> IntroState -> IO ()
draw window menuState = do
    clear [ColorBuffer]

    centerText 0.5 (Vector2d (-1.0) (-0.3)) (Vector2d 1.0 0.3) "Asteroids"
    return ()
