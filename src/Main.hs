module Main where

import Update
import System.IO (hPutStrLn, stderr)
import Player
import System.Exit (exitFailure)
import Math
import qualified Graphics.Rendering.OpenGL as GL
import State
import Control.Monad.State (State, execState)
import Control.Monad.Random
import qualified Graphics.UI.GLFW as GLFW
import System.Random
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


getInput :: GLFW.Window -> IO [GLFW.Key]
getInput window = filterM (isPressed window) keys


isPressed :: GLFW.Window -> GLFW.Key -> IO Bool
isPressed window key = liftM2 (==) (GLFW.getKey window key) (return GLFW.KeyState'Pressed)


keys = [GLFW.Key'E, GLFW.Key'S, GLFW.Key'D, GLFW.Key'F, GLFW.Key'Space, GLFW.Key'Escape, GLFW.Key'Enter]


mainLoop :: GLFW.Window -> ProgramState -> Time -> IO ()
mainLoop w gls@(ProgramState gameState menuState introState mode input) prevTime = do
    close <- GLFW.windowShouldClose w

    let is = initState 1 0.0 ((gs_rng . gls_gameState) gls)

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
                    Playing -> (execState (Update.runFrame input) newTimeState) { gls_keysPressed = input }
                    Menu -> (execState (Menu.runFrame input) newTimeState) { gls_keysPressed = input }
                    Intro -> (execState (Intro.runFrame input) newTimeState) { gls_keysPressed = input }
                    Restarting -> (execState (Update.runFrame input) newTimeState) { gls_gameState = is }

                newTimeState = case mode of
                    Playing -> gls { gls_gameState = gameState { gs_time = ((gs_time . gls_gameState) gls) + (realToFrac time) - prevTime, gs_prevTime = (gs_time . gls_gameState) gls } }
                    Intro -> gls { gls_introState = introState { is_time = ((is_time . gls_introState) gls) + (realToFrac time) - prevTime, is_prevTime = (is_time . gls_introState) gls } }
                    _ -> gls

                in mainLoop w newState (realToFrac time)


main :: IO ()
main = do
    window <- initialize "Asteroids"
    time <- GLFW.getTime
    GLFW.setWindowAspectRatio window $ Just (1, 1)
    GLFW.setStickyKeysInputMode window GLFW.StickyKeysInputMode'Enabled

    (rng, rng2) <- liftM2 (,) newStdGen newStdGen

    let menuState = MenuState Continue
    
    let gameState = initState 1 0.0 rng
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
