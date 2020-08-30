module Intro where

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL hiding (get)
import Graphics.Rendering.OpenGL (vertex, clear, ClearBuffer(ColorBuffer), PrimitiveMode(Points), renderPrimitive)
import State hiding (rndFloat)
import Math
import System.Random
import Control.Monad.State
import Font


rndFloat :: Float -> Float -> State IntroState (Float)
rndFloat min max = do

    state <- get
    let (value, newGenerator) = randomR (min, max) (is_rng state)
    put (state { is_rng = newGenerator } )
    return value


spawnStars :: Float -> Float -> State IntroState ()
spawnStars lastStar time = do

    state <- get

    r <- rndFloat 0.001 0.002
    let nextStar = lastStar + r

    when (nextStar < time) $ do

        x <- rndFloat (-20.0) 20.0
        y <- rndFloat (-20.0) 20.0

        let randomStar = Star (Vector2d x y) nextStar

        state <- get
        put $ state {
            is_stars = randomStar:(is_stars state),
            is_lastStar = nextStar
        }

        spawnStars nextStar time


runFrame :: [GLFW.Key] -> State ProgramState ()
runFrame input = do
    
    state <- get

    put $ state {
        gls_introState = execState runIntroFrame (gls_introState state)
    }

    let prevPressed = gls_keysPressed state
    let newDown = filter (\k -> not (k `elem` prevPressed)) input

    when (GLFW.Key'Enter `elem` newDown) $ do
        state <- get
        put $ state { gls_mode = Playing }

    return ()


runIntroFrame :: State IntroState ()
runIntroFrame = do

    state <- get
    spawnStars (is_lastStar state) (is_time state)
    state <- get
    put $ (onStars ((filter (\s ->  (is_time state) - (st_startTime s) < 10.0)))) state
    
    return ()


drawStars :: [Star] -> Time -> IO ()
drawStars particles time = do
    renderPrimitive Points $ forM_ particles $ \s -> do
        let Star (Vector2d x y) spawnTime = s

        let c = color (time - spawnTime)
        GL.color $ GL.Color4 c c c c

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
