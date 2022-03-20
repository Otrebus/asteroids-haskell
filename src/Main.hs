module Main where

import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import Utils.Math
import qualified Graphics.Rendering.OpenGL as GL
import State
import Control.Monad.State (execState)
import Control.Monad.Random
import qualified Graphics.UI.GLFW as GLFW
import Intro.Update
import Menu.Update
import Game.Update
import Game.Draw
import Menu.Draw
import Intro.Draw

import Debug.Trace


-- Called on window resize
resizeWindow :: GLFW.WindowSizeCallback
resizeWindow win w h = do
    GL.viewport GL.$= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))


-- Called on window maximize
maximizeWindow :: GLFW.WindowMaximizeCallback
maximizeWindow win _ = do
    GLFW.setWindowAspectRatio win $ Just (1, 1)
    (a, b) <- GLFW.getWindowSize win
    GLFW.setWindowSize win b b


-- Initializes the window
initializeWindow ::
    String ->      -- The title bar name of the window
    IO GLFW.Window
initializeWindow title = do
  GLFW.setErrorCallback $ Just $ const (hPutStrLn stderr)
  
  success <- GLFW.init

  if not success then exitFailure else do
      GLFW.windowHint $ GLFW.WindowHint'OpenGLDebugContext True
      GLFW.windowHint $ GLFW.WindowHint'DepthBits (Just 16)
      mw <- GLFW.createWindow 480 480 title Nothing Nothing
      
      case mw of
          Nothing -> GLFW.terminate >> exitFailure
          Just window -> do
              GLFW.makeContextCurrent mw
              GLFW.setWindowSizeCallback window (Just resizeWindow)
              GLFW.setWindowMaximizeCallback window (Just maximizeWindow)

              GLFW.setWindowAspectRatio window $ Just (1, 1)
              GLFW.setStickyKeysInputMode window GLFW.StickyKeysInputMode'Enabled

              GLFW.setWindowAspectRatio window $ Just (1, 1)

              return window


-- Gets input
getInput ::
    GLFW.Window -> -- The GLFW window handle
    IO [GLFW.Key]
getInput window = filterM (isPressed window) keys
    where isPressed window key = liftM2 (==) (GLFW.getKey window key) (return GLFW.KeyState'Pressed)
          keys = [GLFW.Key'E, GLFW.Key'S, GLFW.Key'D, GLFW.Key'F,
                  GLFW.Key'Space, GLFW.Key'Escape, GLFW.Key'Enter]


-- The main loop of the program, reads input and executes a frame
mainLoop ::
    GLFW.Window ->  -- The GLFW window handle
    ProgramState -> -- The state of the program
    Time ->         -- The previous time the function was invoked
    IO ()
mainLoop w gls@(ProgramState gameState menuState introState mode input newPressed) prevTime = do
    close <- GLFW.windowShouldClose w

    let is = initState 1 0.0 ((gs_rng . gls_gameState) gls)

    unless (close || mode == Exiting) $ do

        GLFW.swapBuffers w
        GLFW.pollEvents
        input <- getInput w

        let prevPressed = gls_keysHeld gls
        let newDown = filter (\k -> not (k `elem` prevPressed)) input

        case mode of
            Playing -> Game.Draw.draw gameState
            Menu -> Menu.Draw.draw w menuState
            Intro -> Intro.Draw.draw w introState
            _ -> return ()

        newTime <- GLFW.getTime
        case newTime of
            Nothing -> mainLoop w gls prevTime
            Just time -> let 

                newState = case mode of
                    Playing -> (execState Game.Update.runFrame nts) { gls_keysHeld = input, gls_keysPressed = newDown }
                    Menu -> (execState Menu.Update.runFrame nts) { gls_keysHeld = input, gls_keysPressed = newDown }
                    Intro -> (execState Intro.Update.runFrame nts) { gls_keysHeld = input, gls_keysPressed = newDown }
                    Restarting -> (execState Game.Update.runFrame nts) { gls_gameState = is }

                nts = case mode of
                    Playing -> gls { gls_gameState = gameState {
                            gs_time = ((gs_time . gls_gameState) gls) + deltaTime,
                            gs_prevTime = (gs_time . gls_gameState) gls
                        }
                    }
                    Intro -> gls { gls_introState = introState {
                            is_time = ((is_time . gls_introState) gls) + deltaTime,
                            is_prevTime = (is_time . gls_introState) gls
                        }
                    }
                    _ -> gls

                in mainLoop w newState (realToFrac time)
                where deltaTime = (realToFrac time) - prevTime


-- The entry point of the program, initializes the window and game state
main :: IO ()
main = do
    window <- initializeWindow "Asteroids"

    time <- GLFW.getTime

    (rng, rng2) <- liftM2 (,) newStdGen newStdGen

    let menuState = MenuState Continue
    let gameState = initState 1 0.0 rng
    let introState = IntroState 0.0 0.0 [] (-10.0) rng2

    case time of
        Just t -> do
            let rt = realToFrac t
            let gameStateTimed = gameState { gs_time = rt, gs_prevTime = rt }
            mainLoop window (ProgramState gameStateTimed menuState introState Intro [] []) rt
        Nothing ->
            return ()

    return ()
