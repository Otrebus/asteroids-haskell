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


data Object = Object Vertices Direction Position


turnRate = 2.85 -- radians per second
accRate = 0.01 -- screens per second per second
idMatrix = Matrix2d 1.0 0.0 0.0 1.0


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
      mw <- GLFW.createWindow 480 480 title Nothing Nothing
      case mw of
          Nothing -> GLFW.terminate >> exitFailure
          Just window -> do
              GLFW.makeContextCurrent mw
              GLFW.setKeyCallback window (Just keyCallback)
              return window


keyToAction :: GLFW.Key -> Action
keyToAction key
    | key == GLFW.Key'E = Accelerating


getTurnMatrix :: Float -> Action -> Matrix2d
getTurnMatrix theta TurningLeft = Matrix2d (cos theta) (-sin(theta)) (sin theta) (cos theta)
getTurnMatrix theta TurningRight =  Matrix2d (cos theta) (sin(theta)) (-sin theta) (cos theta)
getTurnMatrix _ _ = idMatrix

runFrame :: Float -> [Action] -> State GameState Int
runFrame time acts = do
    state <- get

    let GameState (PlayerState pos dir vel) count = state

    let theta = turnRate * time

    let turnMatrix = foldr (\a m -> (getTurnMatrix theta a) #*# m) idMatrix acts
    let newVel = if Accelerating `elem` acts then (vel ^+^ (time * accRate)!*^dir ) else vel

    let newPos = pos ^+^ newVel

    put $ (GameState (PlayerState newPos (turnMatrix #*^ dir) newVel) (count+1))
    return 0


isPressed window key = do
    a <- GLFW.getKey window key 
    return (a == GLFW.KeyState'Pressed)


keyCommands = [(GLFW.Key'E, Accelerating), (GLFW.Key'S, TurningLeft), (GLFW.Key'D, Decelerating), (GLFW.Key'F, TurningRight)]

getInput window = map snd <$> (filterM ((isPressed window) . fst) keyCommands)


mainLoop :: (GameState -> IO ()) -> GLFW.Window -> GameState -> IO ()
mainLoop draw w state = do
    time <- GLFW.getTime
    close <- GLFW.windowShouldClose w
    unless close $ do
        draw state

        GLFW.swapBuffers w
        GLFW.pollEvents
        time2 <- GLFW.getTime
        let spf = (-) <$> time2 <*> time

        input <- getInput w

        case spf of
            Nothing -> mainLoop draw w state
            Just spf -> mainLoop draw w newState where
                newState = (execState (runFrame (realToFrac spf) input) state)


playerModel = [Vector2d (-0.04) (-0.04), Vector2d 0 0.04, Vector2d 0.04 (-0.04)]


repeatTwice :: [a] -> [a]
repeatTwice [] = []
repeatTwice (x:xs) = (repeatTwice' (x:xs)) ++ [x] where
    repeatTwice [] = []
    repeatTwice' [y] = [y]
    repeatTwice' (x:y:ys) = x:y : repeatTwice' (y:ys)


drawObject :: Object -> IO()
drawObject (Object vertices dir pos) = do

    let mat = Matrix2d b a (-a) b

    let y = map (toVertex . (pos ^+^) . ((#*^) mat)) vertices

    renderPrimitive Lines $ do mapM_ vertex (repeatTwice y)
    return () where
        (Vector2d a b) = dir


draw :: GameState -> IO ()
draw (GameState playerState _) = do
    clear [ColorBuffer]
    drawObject (Object playerModel (direction playerState) (position playerState))


someFunc :: IO ()
someFunc = do
    window <- initialize "Asteroids"
    mainLoop draw window (GameState (PlayerState (Vector2d 0 0) (Vector2d 0 1.0) (Vector2d 0.0 0.0)) 0)
    return ()
