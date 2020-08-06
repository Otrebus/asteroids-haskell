module Main where

import Debug.Trace
import System.IO (hPutStrLn, stderr)
import Control.Monad


import Player
import Data.List (sort, sortOn)
import System.Exit (exitFailure)
import Math
import qualified Graphics.Rendering.OpenGL as GL hiding (get, rotate)
import GameState
import Control.Monad.State (State, put, execState, get)
import Font (chars)
import Data.Maybe
import qualified Graphics.UI.GLFW as GLFW
import Debug.Trace
import Control.Monad.Random
import System.Random
import Font
import Draw


errorCallback :: GLFW.ErrorCallback
errorCallback _ = hPutStrLn stderr


keyCallback :: GLFW.KeyCallback
keyCallback window key _ action _ = when (key == GLFW.Key'Escape && action == GLFW.KeyState'Pressed) $ GLFW.setWindowShouldClose window True


resizeWindow :: GLFW.WindowSizeCallback
resizeWindow win w h = GL.viewport GL.$= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))


getInput window = map snd <$> (filterM ((isPressed window) . fst) keyCommands)


initialize :: String -> IO GLFW.Window
initialize title = do
  GLFW.setErrorCallback (Just errorCallback)
  
  successfulInit <- GLFW.init

  if not successfulInit then exitFailure else do
      GLFW.windowHint $ GLFW.WindowHint'OpenGLDebugContext True
      GLFW.windowHint $ GLFW.WindowHint'DepthBits 16
      mw <- GLFW.createWindow 480 480 title Nothing Nothing
      
      case mw of
          Nothing -> GLFW.terminate >> exitFailure
          Just window -> do
              GLFW.makeContextCurrent mw
              GLFW.setKeyCallback window (Just keyCallback)
              GLFW.setWindowSizeCallback window (Just resizeWindow)
              return window


isPressed window key = do
    a <- GLFW.getKey window key 
    return (a == GLFW.KeyState'Pressed)


keyCommands = [(GLFW.Key'E, Accelerating),
               (GLFW.Key'S, TurningLeft),
               (GLFW.Key'D, Decelerating),
               (GLFW.Key'F, TurningRight),
               (GLFW.Key'Space, Shooting)]


mainLoop :: (GameState -> IO ()) -> GLFW.Window -> GameState -> IO ()
mainLoop draw w state = do
    close <- GLFW.windowShouldClose w
    let prevTime = gs_time state

    unless close $ do
        newTime <- GLFW.getTime
        draw state
        GLFW.swapBuffers w
        GLFW.pollEvents
        input <- getInput w

        case newTime of
            Nothing -> mainLoop draw w state
            Just time -> mainLoop draw w newState where
                newState = (execState (runFrame input) state { gs_time = ftime, gs_prevTime = prevTime })
                ftime = realToFrac time


someFunc :: IO ()
someFunc = do
    window <- initialize "Asteroids"
    time <- GLFW.getTime

    rng <- newStdGen
    let poly = (evalRand (randomPolygon 13 (Vector2d 0.21 0.21) 0.5 0.5) rng)

    let asteroid = Asteroid 0.25 (Vector2d 0.0 0.0) poly

    case time of
        Just t ->
            mainLoop draw window (GameState (PlayerState startPos startDir startVel 0 thrusters 0.0 Alive) [] [] [] [asteroid] (realToFrac t) (realToFrac t) rng)
        Nothing ->
            return ()
    return ()


main :: IO ()
main = someFunc
