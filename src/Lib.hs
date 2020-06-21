module Lib (someFunc) where

import System.IO (hPutStrLn, stderr)
import Control.Monad
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (vertex, clear, ClearBuffer(ColorBuffer), renderPrimitive, PrimitiveMode(Lines))
import System.Exit (exitFailure)
import Math
import GameState
import Control.Monad.State (State, put, execState, get)
import Debug.Trace


type Vertices = [Vector2d]

type Position = Vector2d

type Direction = Vector2d

data Object = Object Vertices Direction Position


-- type ErrorCallback = Error -> String -> IO ()
errorCallback :: GLFW.ErrorCallback
errorCallback _ = hPutStrLn stderr


keyCallback :: GLFW.KeyCallback
keyCallback window key _ action _ = when (key == GLFW.Key'Escape && action == GLFW.KeyState'Pressed) $ GLFW.setWindowShouldClose window True


initialize :: String -> IO GLFW.Window
initialize title = do
  GLFW.setErrorCallback (Just errorCallback)
  successfulInit <- GLFW.init
  -- if init failed, we exit the program
  if not successfulInit then exitFailure else do
      GLFW.windowHint $ GLFW.WindowHint'OpenGLDebugContext True
      GLFW.windowHint $ GLFW.WindowHint'DepthBits 16
      mw <- GLFW.createWindow 320 320 title Nothing Nothing
      case mw of
          Nothing -> GLFW.terminate >> exitFailure
          Just window -> do
              GLFW.makeContextCurrent mw
              GLFW.setKeyCallback window (Just keyCallback)
              return window


runFrame :: Double -> State GameState Int
runFrame time = do
    ps <- get
    case ps of
        GameState (PlayerState [Accelerating]) c -> do
            put (GameState (PlayerState [Accelerating]) (c+1))
            return (c+1)
        otherwise -> return 0


mainLoop :: IO () -> GLFW.Window -> GameState -> IO ()
mainLoop draw w state = do
    time <- GLFW.getTime
    close <- GLFW.windowShouldClose w
    unless close $ do
        draw
        GLFW.swapBuffers w
        GLFW.pollEvents
        time2 <- GLFW.getTime
        let spf = (-) <$> time2 <*> time

        print $ count state

        case state of
            GameState (PlayerState _) b -> print b

        case spf of
            Nothing -> mainLoop draw w state
            Just spf -> mainLoop draw w newState where
                            newState = (execState (runFrame spf) state)


player = Object [Vector2d (-0.04) (-0.04), Vector2d 0 0.04, Vector2d 0.04 (-0.04)] (Vector2d 0.5 0.868) (Vector2d 0.5 0.5)


repeatTwice :: [a] -> [a]
repeatTwice [] = []
repeatTwice (x:xs) = (repeatTwice' (x:xs)) ++ [x] where
    repeatTwice [] = []
    repeatTwice' [y] = [y]
    repeatTwice' (x:y:ys) = x:y : repeatTwice' (y:ys)


drawObject :: Object -> IO()
drawObject (Object vertices dir pos) = do

    let mat = Matrix2d b a (-a) b

    let y = map (toVertex . (pos ^+) . ((^*) mat)) vertices

    renderPrimitive Lines $ do
        mapM vertex $ repeatTwice y
    return () where
        (Vector2d a b) = dir


draw :: IO ()
draw = do
    clear [ColorBuffer]
    drawObject player


someFunc :: IO ()
someFunc = do
    window <- initialize "Asteroids"
    mainLoop draw window (GameState (PlayerState [Accelerating]) 0)
    return ()
 