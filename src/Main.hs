module Main where

import Debug.Trace
import System.IO (hPutStrLn, stderr)
import Control.Monad


import Player
import Data.List (sort, sortOn)
import System.Exit (exitFailure)
import Math
import qualified Graphics.Rendering.OpenGL as GL hiding (get, rotate)
import State
import Control.Monad.State (State, put, execState, get)
import Font (chars)
import Data.Maybe
import qualified Graphics.UI.GLFW as GLFW
import Debug.Trace
import Control.Monad.Random
import System.Random
import Font
import qualified Draw
import qualified Menu


errorCallback :: GLFW.ErrorCallback
errorCallback _ = hPutStrLn stderr


resizeWindow :: GLFW.WindowSizeCallback
resizeWindow win w h = do
    GL.viewport GL.$= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))


initialize :: String -> IO GLFW.Window
initialize title = do
  GLFW.setErrorCallback (Just errorCallback)
  
  successfulInit <- GLFW.init

  if not successfulInit then exitFailure else do
      GLFW.windowHint $ GLFW.WindowHint'OpenGLDebugContext True
      GLFW.windowHint $ GLFW.WindowHint'DepthBits (Just 16)
      mw <- GLFW.createWindow 480 480 title Nothing Nothing
      
      case mw of
          Nothing -> GLFW.terminate >> exitFailure
          Just window -> do
              GLFW.makeContextCurrent mw
              GLFW.setWindowSizeCallback window (Just resizeWindow)
              return window


getInput window = filterM (isPressed window) keys

isPressed window key = do
    a <- GLFW.getKey window key 
    return (a == GLFW.KeyState'Pressed)

keys = [GLFW.Key'E, GLFW.Key'S, GLFW.Key'D, GLFW.Key'F, GLFW.Key'Space, GLFW.Key'Escape, GLFW.Key'Enter]


mainLoop :: GLFW.Window -> ProgramState -> IO ()
mainLoop w gls@(ProgramState gameState menuState mode input) = do
    close <- GLFW.windowShouldClose w

    unless (close || mode == Exiting) $ do
        newTime <- GLFW.getTime

        GLFW.swapBuffers w
        GLFW.pollEvents
        input <- getInput w

        case mode of
            Playing -> Draw.draw gameState
            Menu -> Menu.draw w menuState

        case newTime of
            Nothing -> mainLoop w gls
            Just time -> mainLoop w newState where
                
                newState = case mode of
                    Playing -> (execState (Draw.runFrame input) newTimeState) { gls_keysPressed = input }
                    Menu -> (execState (Menu.runFrame input) gls) { gls_keysPressed = input, gls_gameState = gameState { gs_time = realToFrac time, gs_prevTime = realToFrac time } }

                    where
                        newTimeState = gls { gls_gameState = gameState { gs_time = realToFrac time , gs_prevTime = prevTime } }
                        prevTime = (gs_time . gls_gameState) gls



main :: IO ()
main = do
    window <- initialize "Asteroids"
    time <- GLFW.getTime
    GLFW.setWindowAspectRatio window $ Just (1, 1)
    GLFW.setStickyKeysInputMode window GLFW.StickyKeysInputMode'Enabled

    rng <- newStdGen
    let poly = (evalRand (randomPolygon 13 (Vector2d 0.21 0.21) 0.5 0.5) rng)
    let asteroid = Asteroid 0.25 (Vector2d 0.0 0.0) poly
    let menuState = MenuState Continue
    let keysPressed = []
    let mode = Playing
    case time of
        Just t -> do
            let gameState = GameState (PlayerState startPos startDir startVel 0 thrusters 0.0 Alive) [] [] [] [asteroid] (realToFrac t) (realToFrac t) rng
            mainLoop window (ProgramState gameState menuState mode keysPressed)
        Nothing ->
            return ()

    return ()
