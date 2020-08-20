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
import qualified Intro


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


mainLoop :: GLFW.Window -> ProgramState -> Time -> IO ()
mainLoop w gls@(ProgramState gameState menuState introState mode input) prevTime = do
    close <- GLFW.windowShouldClose w

    is <- initState

    unless (close || mode == Exiting) $ do

        GLFW.swapBuffers w
        GLFW.pollEvents
        input <- getInput w

        case mode of
            Playing -> Draw.draw gameState
            Menu -> Menu.draw w menuState
            Intro -> Intro.draw w introState
            _ -> return ()

        newTime <- GLFW.getTime
        case newTime of
            Nothing -> mainLoop w gls prevTime
            Just time -> let 

                newState = case mode of
                    Playing -> (execState (Draw.runFrame input) newTimeState) { gls_keysPressed = input }
                    Menu -> (execState (Menu.runFrame input) newTimeState) { gls_keysPressed = input }
                    Intro -> (execState (Intro.runFrame input) newTimeState) { gls_keysPressed = input }
                    Restarting -> (execState (Draw.runFrame input) newTimeState) { gls_gameState = is }

                newTimeState = case mode of
                    Playing -> gls { gls_gameState = gameState { gs_time = ((gs_time . gls_gameState) gls) + (realToFrac time) - prevTime, gs_prevTime = (gs_time . gls_gameState) gls } }
                    Intro -> gls { gls_introState = introState { is_time = ((is_time . gls_introState) gls) + (realToFrac time) - prevTime, is_prevTime = (is_time . gls_introState) gls } }
                    _ -> gls

                in mainLoop w newState (realToFrac time)


initState = do
    rng <- newStdGen
    let poly = (evalRand (randomPolygon 13 (Vector2d 0.21 0.21) 0.5 0.5) rng)
    let asteroid = Asteroid 0.25 (Vector2d 0.0 0.0) poly
    return $ GameState (PlayerState startPos startDir startVel 0 thrusters 0.0 Alive) [] [] [] [asteroid] 0.0 0.0 0.0 3 rng


main :: IO ()
main = do
    window <- initialize "Asteroids"
    time <- GLFW.getTime
    GLFW.setWindowAspectRatio window $ Just (1, 1)
    GLFW.setStickyKeysInputMode window GLFW.StickyKeysInputMode'Enabled

    rng2 <- newStdGen
    let menuState = MenuState Continue
    
    gameState <- initState
    let introState = IntroState 0.0 0.0 [] (-10.0) rng2
    let keysPressed = []
    let mode = Intro
    case time of
        Just t -> do
            let rt = realToFrac t
            mainLoop window (ProgramState gameState { gs_time = rt, gs_prevTime = rt } menuState introState mode keysPressed) rt
        Nothing ->
            return ()

    return ()
