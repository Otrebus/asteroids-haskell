module Lib (someFunc) where

import Debug.Trace
import System.IO (hPutStrLn, stderr)
import Control.Monad
import qualified Graphics.Rendering.OpenGL as GL hiding (get, rotate)
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (vertex, clear, ClearBuffer(ColorBuffer), renderPrimitive, PrimitiveMode(Lines, Points, QuadStrip, TriangleStrip, LineLoop))
import Player
import System.Exit (exitFailure)
import Data.Fixed (mod')
import Math
import GameState
import Control.Monad.State (State, put, execState, get)
import Font (chars)
import Debug.Trace
import Control.Monad.Random
import System.Random
import Font


data Object = Object (Vertices, [Vertices]) Direction Position

bulletVel = 0.90 
fireRate = 0.23 -- bullets per second
accRate = 0.2 -- screens per second per second
angularAcc = 2
idMatrix = Matrix2d 1.0 0.0 0.0 1.0


errorCallback :: GLFW.ErrorCallback
errorCallback _ = hPutStrLn stderr


keyCallback :: GLFW.KeyCallback
keyCallback window key _ action _ = when (key == GLFW.Key'Escape && action == GLFW.KeyState'Pressed) $ GLFW.setWindowShouldClose window True


resizeWindow :: GLFW.WindowSizeCallback
resizeWindow win w h = GL.viewport GL.$= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))


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


addEngineParticle :: Float -> Thruster -> Float -> Position -> Direction -> Velocity -> State GameState ()
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


addEngineParticles pos dir vel angVel action actions thrusterGetter fp start current time = do
    state <- get
    let nextParticleTime = current + (t_emissionInterval (thrusterGetter state))
    when (action `elem` actions && nextParticleTime < time) $ do

        let deltaAngle = angVel * (nextParticleTime - time)
        let turnMatrix = getTurnMatrix deltaAngle

        let newDir = turnMatrix #*^ dir

        addEngineParticle nextParticleTime (thrusterGetter state) 0.6 (pos ^+^ (vel ^*! (nextParticleTime-start))) newDir vel
        state <- get
        put $ ((onPlayerState . onThrusters . fp) (\t -> t { t_lastEmitted = nextParticleTime } )) state
        addEngineParticles pos dir vel angVel action actions thrusterGetter fp start nextParticleTime time


addEnginesParticles :: [Action] -> State GameState ()
addEnginesParticles actions = do

    gs@(GameState (PlayerState pos dir vel angVel _ _) _ _ time prevTime _) <- get

    let thfun = (ps_thrusters . gs_playerState)
    let engines = [(Accelerating, e_main . thfun, onMainThruster),
                   (TurningLeft, e_topright . thfun, onTopRightThruster),
                   (TurningRight, e_topleft . thfun, onTopLeftThruster),
                   (TurningLeft, e_bottomleft . thfun, onBottomLeftThruster),
                   (TurningRight, e_bottomright . thfun, onBottomRightThruster)]

    forM_ engines $ \(ac, th, fp) -> do
        let t0 = t_lastEmitted (th gs); int = t_emissionInterval (th gs)
        let t = t0 + int * realToFrac (floor ((prevTime - t0)/int))
        addEngineParticles pos dir vel angVel ac actions th fp t t time


addBullets :: State GameState ()
addBullets = do
    state <- get
    let GameState (playerState@(PlayerState pos dir vel _ thrusters lastBullet)) _ bullets time _ _ = state
    when (lastBullet + fireRate < time) $ do
        let bulletPos = pos ^+^ rebase dir plT
        put state {
            gs_playerState = playerState {
                ps_lastBullet = time
            },
            gs_bullets = Bullet {
                b_position = bulletPos,
                b_direction = dir,
                b_velocity = bulletVel!*^dir ^+^ vel,
                b_lifeTime = time + 2.0
            }:bullets
        }


getTurnMatrix :: Angle -> Matrix2d
getTurnMatrix theta = Matrix2d (cos theta) (sin(theta)) (-sin theta) (cos theta)


drawParticles :: [Particle] -> IO ()
drawParticles particles = do
    renderPrimitive Points $ forM_ particles renderParticle where
        renderParticle p = do
            let d = p_brightness p
            GL.color $ GL.Color4 d d d (d :: GL.GLfloat)
            vertex ((toVertex . p_position) p)


drawPoly :: [Vector2d] -> IO ()
drawPoly vertices = do

    GL.color $ GL.Color4 1 1 1 (1 :: GL.GLfloat)
    renderPrimitive LineLoop $ do mapM_ vertex (map toVertex vertices)
    return ()


updateParticles :: Time -> Time -> [Particle] -> [Particle]
updateParticles time deltaT particles = newParticles
    where
        filteredParticles = filter ((>time) . p_lifeTime) $ filter ((>0.01) . (p_brightness)) particles
        newParticles = map (\(Particle pos vel start life bri) -> (
            let newBri = (bri*(life-time)/(life-start))**0.85
                newPos = pos ^+^ (min (time-start) deltaT)!*^vel
            in  Particle newPos vel start life newBri)) filteredParticles

updateBullets :: Time -> Time -> [Bullet] -> [Bullet]
updateBullets time deltaT = filter ((>time) . b_lifeTime) . map (\(Bullet pos dir vel life) -> (Bullet (pos ^+^ (deltaT)!*^vel) dir vel life ))


wrap :: Vector2d -> Vector2d
wrap (Vector2d x y) = Vector2d (mod' (x + 1.0) 2.0 - 1.0) (mod' (y + 1.0) 2.0 - 1.0)


normalizePositions :: State GameState ()
normalizePositions = do
    state <- get
    put $ (onPlayerState . onPlayerPos) (wrap) state
    state <- get
    put $ onParticles (map (\ps -> ps { p_position = wrap (p_position ps) })) state
    state <- get
    put $ onBullets (map (\bs -> bs { b_position = wrap (b_position bs) })) state


runFrame :: [Action] -> State GameState ()
runFrame actions = do
    state <- get

    let GameState (playerState@(PlayerState pos dir vel angVel thrusters lastBullet)) particles bullets time prevTime rng = state
    let delta = time - prevTime

    let turnAcc = ((if TurningRight `elem` actions then angularAcc else 0) +
                   (if TurningLeft `elem` actions then (-angularAcc) else 0))
    let angularVel = angVel + turnAcc*delta
    let deltaAngle = angVel * delta + 0.5 * turnAcc * delta * delta

    let turnMatrix = getTurnMatrix deltaAngle
    let newVel = if Accelerating `elem` actions then (vel ^+^ (delta * accRate)!*^dir ) else vel
    let newPos = pos ^+^ (delta!*^vel) ^+^ (delta*delta*accRate)!*^dir

    addEnginesParticles actions
    when (Shooting `elem` actions) $ do
        addBullets

    state <- get
    let newParticles = updateParticles time delta (gs_particles state)
    let newBullets = updateBullets time delta (gs_bullets state)
    put $ state {
        gs_playerState = (gs_playerState state) {
            ps_position = newPos,
            ps_direction = (turnMatrix #*^ dir),
            ps_angularVelocity = angularVel,
            ps_velocity = newVel
        },
        gs_time = time,
        gs_particles = newParticles,
        gs_bullets = newBullets
    }
    normalizePositions


isPressed window key = do
    a <- GLFW.getKey window key 
    return (a == GLFW.KeyState'Pressed)


keyCommands = [(GLFW.Key'E, Accelerating),
               (GLFW.Key'S, TurningLeft),
               (GLFW.Key'D, Decelerating),
               (GLFW.Key'F, TurningRight),
               (GLFW.Key'Space, Shooting)]

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


drawObject :: Object -> IO ()
drawObject (Object (lineVertices, triangles) dir pos) = do
    let mat = Matrix2d b a (-a) b
    let tris = map (\v -> (map (toVertex . (pos ^+^) . ((#*^) mat)) v)) triangles
    let y = map (toVertex . (pos ^+^) . ((#*^) mat)) lineVertices

    GL.color $ GL.Color4 0 0 0 (1 :: GL.GLfloat)
    forM_ tris (\tri -> renderPrimitive TriangleStrip $ (do mapM_ vertex tri))

    GL.color $ GL.Color4 1 1 1 (1 :: GL.GLfloat)
    renderPrimitive LineLoop $ do mapM_ vertex y
    return () where
        (Vector2d a b) = dir


drawDuplicates :: Object -> IO ()
drawDuplicates (Object (vectors, _) dir pos) = do
    let fx (Vector2d x y) = x; fy (Vector2d x y) = y

    let wrap = [(fx, (<), Vector2d 1.0 0.0, maximum), (fx, (>), Vector2d (-1.0) 0.0, minimum),
                (fy, (<), Vector2d 0.0 1.0, maximum), (fy, (>), Vector2d 0.0 (-1.0), minimum)]

    forM_ wrap $ \(fn, op, v, mm) -> do
        when (fn v `op`(mm (map (fn . (^+^ pos)) vectors))) $ do
            drawObject (Object playerModel dir (pos ^-^ v^*!2))


trans t (Object (ls, vs) x y) = Object (map (\(Vector2d x y) -> Vector2d (x + t) (y + t)) ls, map (map (\(Vector2d a b) -> Vector2d (a + t) (b + t))) vs) x y


draw :: GameState -> IO ()
draw (GameState playerState particles bullets _ _ _) = do
    clear [ColorBuffer]
    drawText ("Number of particles: " ++ (show (length particles))) 0.1 (Vector2d (-0.9) 0.9)

    drawObject $ Object playerModel (ps_direction playerState) (ps_position playerState)

    drawParticles particles

    let rng = mkStdGen $ 122 + fromIntegral (toInteger (round((realToFrac (length particles) / 200.0))))
    let poly = (evalRand (randomPolygon 12) rng)
    drawPoly poly

    forM_ bullets $ \(Bullet pos dir vel _) -> do
        drawObject (Object bulletModel dir pos)
        drawDuplicates (Object bulletModel dir pos)


someFunc :: IO ()
someFunc = do
    window <- initialize "Asteroids"
    time <- GLFW.getTime

    rng <- newStdGen

    case time of
        Just t ->
            mainLoop draw window (GameState (PlayerState startPos startDir startVel 0 thrusters 0.0) [] [] (realToFrac t) (realToFrac t) rng)
        Nothing ->
            return ()
    return ()
