module Intro.Update where

import qualified Graphics.UI.GLFW as GLFW
import State hiding (rndFloat)
import Utils.Math
import System.Random
import Control.Monad.State

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