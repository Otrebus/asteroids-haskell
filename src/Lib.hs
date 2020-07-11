module Lib (someFunc) where

import Debug.Trace
import System.IO (hPutStrLn, stderr)
import Control.Monad
import qualified Graphics.Rendering.OpenGL as GL hiding (get, rotate)
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


resizeWindow :: GLFW.WindowSizeCallback
resizeWindow win w h =
    do
      GL.viewport   GL.$= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))


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
    r <- rndFloat 0.0 0.2
    q <- rndFloat 0.0 0.2
    
    x <- rndFloat (-1) 1
    y <- liftM (**1.1) (rndFloat 0 0.3)
    
    s <- rndFloat 0.0 0.15
    let newnDir = rotate (x*y) newDir
    state <- get
    put (state { gs_particles = ((Particle (playerPos ^+^ newPos) (playerVel ^+^ ((newnDir)^*!(speed + s))) time (time + 2.5 + 0.2) (0.5 + q))):(gs_particles state) } )


addEngineParticles pos dir vel action actions thrusterGetter fp start current time = do
    state <- get
    let nextParticleTime = current + (t_emissionInterval (thrusterGetter state))
    when (action `elem` actions && nextParticleTime < time) $ do
        addEngineParticle nextParticleTime (thrusterGetter state) 0.6 (pos ^+^ (vel ^*! (nextParticleTime-start))) dir vel
        state <- get
        put $ ((onPlayerState . onThrusters . fp) (\t -> t { t_lastEmitted = nextParticleTime } )) state
        addEngineParticles pos dir vel action actions thrusterGetter fp start nextParticleTime time


addEnginesParticles :: [Action] -> State GameState ()
addEnginesParticles actions = do

    gs@(GameState (PlayerState pos dir vel _) _ time prevTime _) <- get

    let thfun = (ps_thrusters . gs_playerState)
    let engines = [(Accelerating, e_main . thfun, onMainThruster),
                   (TurningLeft, e_topright . thfun, onTopRightThruster),
                   (TurningRight, e_topleft . thfun, onTopLeftThruster),
                   (TurningLeft, e_bottomleft . thfun, onBottomLeftThruster),
                   (TurningRight, e_bottomright . thfun, onBottomRightThruster)]

    forM_ engines $ \(ac, th, fp) -> do
        let t0 = t_lastEmitted (th gs); int = t_emissionInterval (th gs)
        let t = t0 + int * realToFrac (floor ((prevTime - t0)/int))
        addEngineParticles pos dir vel ac actions th fp t t time


getTurnMatrix :: Float -> Action -> Matrix2d
getTurnMatrix theta TurningLeft = Matrix2d (cos theta) (-sin(theta)) (sin theta) (cos theta)
getTurnMatrix theta TurningRight =  Matrix2d (cos theta) (sin(theta)) (-sin theta) (cos theta)
getTurnMatrix _ _ = idMatrix


drawParticles :: [Particle] -> IO()
drawParticles particles = do

    renderPrimitive Points $ forM_ particles renderParticle where
        renderParticle p = do
            let d = p_brightness p
            GL.color $ GL.Color4 d d d (d :: GL.GLfloat)
            vertex ((toVertex . p_position) p)


updateParticles :: Float -> Float -> [Particle] -> [Particle]
updateParticles time deltaT = filter ((>time) . p_lifeTime) . map (\(Particle pos vel start life bri) -> (Particle (pos ^+^ (min (time-start) deltaT)!*^vel) vel start life ((bri*(life-time)/(life-start))**0.85)))


runFrame :: [Action] -> State GameState Int
runFrame actions = do
    state <- get

    let GameState (playerState@(PlayerState pos dir vel thrusters)) particles time prevTime rng = state
    let delta = time - prevTime
    let theta = turnRate * delta

    let turnMatrix = foldr (\a m -> (getTurnMatrix theta a) #*# m) idMatrix actions
    let newVel = if Accelerating `elem` actions then (vel ^+^ (delta * accRate)!*^dir ) else vel
    let newPos = pos ^+^ (delta!*^vel)

    addEnginesParticles actions

    state <- get
    let newParticles = updateParticles time delta (gs_particles state)
    put $ state { gs_playerState = (gs_playerState state) { ps_position = newPos, ps_direction = (turnMatrix #*^ dir), ps_velocity = newVel }, gs_time = time, gs_particles = newParticles }
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

    GL.color $ GL.Color4 1 1 1 (1 :: GL.GLfloat)
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
            mainLoop draw window (GameState (PlayerState startPos startDir startVel thrusters) [] (realToFrac t) (realToFrac t) rng)
        Nothing ->
            return ()
    return ()
