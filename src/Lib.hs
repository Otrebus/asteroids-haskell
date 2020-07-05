module Lib (someFunc) where

import Debug.Trace
import System.IO (hPutStrLn, stderr)
import Control.Monad
import Graphics.Rendering.OpenGL as GL hiding (Position, get, rotate)
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (vertex, clear, ClearBuffer(ColorBuffer), renderPrimitive, PrimitiveMode(Lines, Points))
import Player
import System.Exit (exitFailure)
import Math
import GameState
import Control.Monad.State (State, put, execState, get)
import Debug.Trace
import System.Random


data Object = Object Vertices Direction Position


turnRate = 2.85 -- radians per second
accRate = 0.2 -- screens per second per second
idMatrix = Matrix2d 1.0 0.0 0.0 1.0


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


rebase :: Vector2d -> Vector2d -> Vector2d
rebase (Vector2d a b) (Vector2d x y) = y!*^(Vector2d a b) ^+^ x!*^(Vector2d b (-a))


rotate :: Float -> Vector2d -> Vector2d
rotate theta v = (Matrix2d(cos theta) (-sin(theta)) (sin theta) (cos theta))#*^v


rndFloat :: Float -> Float -> State GameState (Float)
rndFloat min max = do
    state <- get
    let (value, newGenerator) = randomR (min,max) (gs_rng state)
    put (state { gs_rng = newGenerator})
    return value


addEngineParticle :: Float -> Thruster -> Float -> Vector2d -> Vector2d -> Vector2d -> State GameState ()
addEngineParticle time thruster speed playerPos playerDir playerVel  = do
    let newPos = rebase playerDir (t_position thruster)
    let newDir = rebase playerDir (t_direction thruster)
    f <- rndFloat (-0.2) 0.2
    let newnDir = rotate f newDir
    state <- get
    put (state { gs_particles = ((Particle (playerPos ^+^ newPos) (playerVel ^+^ ((newnDir)^*!speed)) (time + 1.5) 0.5)):(gs_particles state) } )


addEngineParticles :: [Action] -> State GameState ()
addEngineParticles actions = do

    state <- get
    let GameState (PlayerState pos dir vel thrusters) particles _ time rng = state

    when (Accelerating `elem` actions && ((t_nextEmitted . e_main) thrusters < time)) $ do
        addEngineParticle time mainThruster 0.6 pos dir vel

    when (TurningRight `elem` actions && ((t_nextEmitted . e_main) thrusters < time)) $ do
        addEngineParticle time topLeftThruster 0.6 pos dir vel
        addEngineParticle time bottomRightThruster 0.6 pos dir vel

    when (TurningLeft `elem` actions && ((t_nextEmitted . e_main) thrusters < time)) $ do
        addEngineParticle time topRightThruster 0.6 pos dir vel
        addEngineParticle time bottomLeftThruster 0.6 pos dir vel    



getTurnMatrix :: Float -> Action -> Matrix2d
getTurnMatrix theta TurningLeft = Matrix2d (cos theta) (-sin(theta)) (sin theta) (cos theta)
getTurnMatrix theta TurningRight =  Matrix2d (cos theta) (sin(theta)) (-sin theta) (cos theta)
getTurnMatrix _ _ = idMatrix


drawParticles :: [Particle] -> IO()
drawParticles particles = do

    renderPrimitive Points $ forM_ particles renderParticle where
        renderParticle p = do
            let d = p_brightness p
            GL.color $ GL.Color4 d d d (d :: GLfloat)
            vertex ((toVertex . p_position) p)


updateParticles :: Float -> Float -> [Particle] -> [Particle]
updateParticles time deltaT = filter ((>time) . p_lifeTime) . map (\(Particle pos vel life bri) -> (Particle (pos ^+^ deltaT!*^vel) vel life bri))



runFrame :: Float -> [Action] -> State GameState Int
runFrame delta actions = do
    state <- get

    let GameState (playerState@(PlayerState pos dir vel thrusters)) particles count time rng = state

    let theta = turnRate * delta

    let turnMatrix = foldr (\a m -> (getTurnMatrix theta a) #*# m) idMatrix actions
    let newVel = if Accelerating `elem` actions then (vel ^+^ (delta * accRate)!*^dir ) else vel

    let newPos = pos ^+^ (delta!*^newVel)

    addEngineParticles actions

    state <- get
    let newParticles = updateParticles time delta (gs_particles state)
    put $ state { gs_playerState = playerState { ps_position = newPos, ps_direction = (turnMatrix #*^ dir), ps_velocity = newVel }, gs_time = time, gs_count = count+1, gs_particles = newParticles }
    return 0


isPressed window key = do
    a <- GLFW.getKey window key 
    return (a == GLFW.KeyState'Pressed)


keyCommands = [(GLFW.Key'E, Accelerating), (GLFW.Key'S, TurningLeft), (GLFW.Key'D, Decelerating), (GLFW.Key'F, TurningRight)]

getInput window = map snd <$> (filterM ((isPressed window) . fst) keyCommands)


mainLoop :: (GameState -> IO ()) -> GLFW.Window -> GameState -> IO ()
mainLoop draw w state = do
    close <- GLFW.windowShouldClose w
    let prevTime = gs_time state
    let blah = gs_count state

    unless close $ do
        draw state

        GLFW.swapBuffers w
        GLFW.pollEvents
        input <- getInput w
        newTime <- GLFW.getTime

        case newTime of
            Nothing -> mainLoop draw w state
            Just time -> mainLoop draw w newState where
                newState = (execState (runFrame deltaTime input) state { gs_time = ftime })
                deltaTime = (ftime - prevTime)
                ftime = realToFrac time


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

    GL.color $ GL.Color4 1 1 1 (1 :: GLfloat)
    renderPrimitive Lines $ do mapM_ vertex (repeatTwice y)
    return () where
        (Vector2d a b) = dir


draw :: GameState -> IO ()
draw (GameState playerState particles _ _ _) = do
    clear [ColorBuffer]
    drawObject (Object playerModel (ps_direction playerState) (ps_position playerState))
    drawParticles particles


someFunc :: IO ()
someFunc = do
    window <- initialize "Asteroids"
    time <- GLFW.getTime
    rng <- newStdGen
    case time of
        Just t ->
            mainLoop draw window (GameState (PlayerState startPos startDir startVel thrusters) [] 0 (realToFrac t) rng)
        Nothing ->
            return ()
    return ()
