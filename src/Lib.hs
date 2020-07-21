module Lib (someFunc) where

import Debug.Trace
import System.IO (hPutStrLn, stderr)
import Control.Monad
import qualified Graphics.Rendering.OpenGL as GL hiding (get, rotate)
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (vertex, clear, ClearBuffer(ColorBuffer), renderPrimitive, PrimitiveMode(Lines, Points, QuadStrip, TriangleStrip, LineLoop, TriangleFan))
import Player
import System.Exit (exitFailure)
import Data.Fixed (mod')
import Math
import GameState
import Control.Monad.State (State, put, execState, get)
import Font (chars)
import Data.Maybe
import Debug.Trace
import Control.Monad.Random
import System.Random
import Font

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

    gs@(GameState (PlayerState pos dir vel angVel _ _ _) _ _ _ time prevTime _) <- get

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
    let GameState (playerState@(PlayerState pos dir vel _ thrusters lastBullet aliveState)) _ bullets _ time _ _ = state
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


bulletImpact :: Asteroid -> Bullet -> Float -> Maybe ((Vector2d, Vector2d), Float)
bulletImpact asteroid bullet delta = if impact then Just (mini, t) else Nothing
    where 
          mini = if impact then (head is) else (undefined, undefined)
          (s, t) = if impact then intersect (b1, b2) (head is) else (0, 0)
          impact = (not . null) is
          is = filter (\v -> intersects (b1, b2) v) vs
          (b1, b2) = (b_position bullet, (b_position bullet) ^+^ (b_velocity bullet)^*!delta)
          vs = zip vertices ((tail . cycle) vertices)
          vertices = a_vertices asteroid


getTurnMatrix :: Angle -> Matrix2d
getTurnMatrix theta = Matrix2d (cos theta) (sin(theta)) (-sin theta) (cos theta)


drawParticles :: [Particle] -> IO ()
drawParticles particles = do
    renderPrimitive Points $ forM_ particles renderParticle where
        renderParticle p = do
            let d = p_brightness p
            GL.color $ GL.Color4 d d d (d :: GL.GLfloat)
            vertex ((toVertex . p_position) p)


drawAsteroid :: Vertices -> IO ()
drawAsteroid verts = do

    GL.color $ GL.Color4 0.05 0.05 0.05 (1 :: GL.GLfloat)
    renderPrimitive TriangleFan $ do mapM_ vertex (map toVertex verts)
    GL.color $ GL.Color4 1 1 1 (1 :: GL.GLfloat)
    renderPrimitive LineLoop $ do mapM_ vertex (map toVertex verts)
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


inside :: Vertices -> Vertices -> Bool
inside as bs = or [and [(c ^-^ b) ^%^ (a ^-^ b) > 0 | (b, c) <- zip bs ((tail . cycle) bs)] | a <- as]


intersect :: (Vector2d, Vector2d) -> (Vector2d, Vector2d) -> (Float, Float)
intersect (Vector2d p1x p1y, Vector2d p2x p2y) (Vector2d v1x v1y, Vector2d v2x v2y) =
    (s, t)
    where
        a = p1x - p2x; b = v2x - v1x; c = p1y - p2y; d = v2y - v1y; e = p1x - v1x; f = p1y - v1y
        s = (e*d - b*f)/(a*d - b*c); t = (a*f - e*c)/(a*d - b*c)


intersects a b = let (s, t) = intersect a b in t >= 0 && t <= 1 && s >= 0 && s <= 1


detectCollision :: Vertices -> Vertices -> Direction -> Bool
detectCollision ps vs dir = or [intersects (p, p ^+^ dir) (v1, v2) | p <- ps, (v1, v2) <- zip vs ((tail . cycle) vs)]


detectCollisions :: State GameState (Bool)
detectCollisions = do
    state <- get
    let GameState (playerState@(PlayerState pos dir vel angVel thrusters lastBullet _)) particles bullets asteroids time prevTime rng = state

    let (verts, tris) = playerModel

    let p1s = map ((^+^ pos) . (rebase dir)) verts
    let tri1s = [map ((^+^ pos) . (rebase dir)) tri | tri <- tris]

    let a = or (map (\(Asteroid adir avel avert) -> detectCollision p1s avert (vel^*!(time - prevTime))) asteroids)
    let b = or (map (\(Asteroid adir avel avert) -> detectCollision avert p1s (vel^*!(prevTime - time))) asteroids)
    let c = or (map (\(Asteroid adir avel avert) -> inside p1s avert) asteroids)
    let d = or [or (map (\(Asteroid adir avel avert) -> inside avert t) asteroids) | t <- tri1s]

    return (a || b || c || d)


polyArea :: Vertices -> Float
polyArea vs = 0.5*(sum [(xComp v)*(yComp w) - (yComp v)*(xComp w) | (v, w) <- zip vs ((tail . cycle) vs)])


polyCirc :: Vertices -> Float
polyCirc vs = sum [len (v ^-^ w) | (v, w) <- zip vs ((tail . cycle) vs)]


calcRatio :: Vertices -> Float
calcRatio [] = 0
calcRatio vs = let area = polyArea vs; circ = polyCirc vs in (area)/(circ*circ*4*pi)


loopSplits :: (Vector2d, Vector2d) -> Float -> Vertices -> (Vector2d, Vector2d) -> Vertices -> (Vertices, Vertices)
loopSplits (p1, p2) t (xs) (v1, v2) [] = ([], [])
loopSplits (p1, p2) t (xs) (v1, v2) (y:ys)
    | otherwise = if (calcRatio cc)*(calcRatio dd) > (calcRatio aa)*(calcRatio bb) then (cc, dd) else (aa, bb)
    where
        (cc, dd) = ((interpolate p1 p2 t):p2:xs ++ (v1:[interpolate v1 v2 0.5]), (interpolate v1 v2 0.5):v2:y:(ys ++ [p1, interpolate p1 p2 t]))
        (aa, bb) = loopSplits (p1, p2) t (xs ++ [v2]) (v2, y) ys


maprest :: [a] -> [[a]]
maprest (x:xs) = (x:xs) : maprest xs


splitAsteroid :: Asteroid -> (Vector2d, Vector2d) -> Float -> (Asteroid, Asteroid)
splitAsteroid asteroid (p1, p2) t =
    (Asteroid (Vector2d 0 0) (Vector2d 0 0) as, Asteroid (Vector2d 0 0) (Vector2d 0 0) bs)
    where
        (as, bs) = loopSplits (p1, p2) t [] (v1, v2) (takeWhile (/= p1) rest)
        (v1:v2:rest):_ = (dropWhile ((/= p2) . head)) $ maprest (cycle vs)
        vs = a_vertices asteroid


runFrame :: [Action] -> State GameState ()
runFrame actions = do
    state <- get

    let GameState (playerState@(PlayerState pos dir vel angVel thrusters lastBullet aliveState)) particles bullets asteroids time prevTime rng = state
    let delta = time - prevTime

    let turnAcc = ((if TurningRight `elem` actions then angularAcc else 0) +
                   (if TurningLeft `elem` actions then (-angularAcc) else 0))
    let angularVel = angVel + turnAcc*delta
    let deltaAngle = angVel * delta + 0.5 * turnAcc * delta * delta

    let turnMatrix = getTurnMatrix deltaAngle
    let newVel = if Accelerating `elem` actions then (vel ^+^ (delta * accRate)!*^dir ) else vel
    let newPos = pos ^+^ (delta!*^vel) ^+^ (0.5*delta*delta*(if Accelerating `elem` actions then accRate else 0))!*^dir

    addEnginesParticles actions
    when (Shooting `elem` actions) $ do
        addBullets

    b <- detectCollisions

    when b $ do
        trace "BOOM" $ return ()
        state <- get
        put $ onPlayerState (\s -> s { ps_aliveState = Dead }) state

    let impacts = filter (\(bul, ast, m) -> isJust m) [(bul, ast, bulletImpact ast bul delta) | ast <- asteroids, bul <- bullets]
    let impactedAsteroids = map (\(_, ast, _) -> ast) impacts

    let newAsteroidPairs = map ((\(bul, ast, Just ((v1, v2), t)) -> splitAsteroid ast (v1, v2) t)) impacts
    let newAsteroids = concat $ map (\(a, b) -> [a, b]) newAsteroidPairs

    when (newAsteroids /= []) $ do
        trace (show (newAsteroids)) $ return ()

        -- bulletImpact :: Asteroid -> Bullet -> Float -> Maybe ((Vector2d, Vector2d), Float)

    when ((not . null) impacts) $ do

        trace "POW" $ return ()
        state <- get
        put $ state {
            gs_asteroids = (filter (\ast -> (not (elem ast impactedAsteroids))) (gs_asteroids state)) ++ newAsteroids,
            gs_bullets = filter (\b -> (and [(b /= bl) | (bl, _, _) <- impacts])) (gs_bullets state)
        }

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
drawDuplicates (Object (vectors, tris) dir pos) = do
    let fx (Vector2d x y) = x; fy (Vector2d x y) = y

    let wrap = [(fx, (<), Vector2d 1.0 0.0, maximum), (fx, (>), Vector2d (-1.0) 0.0, minimum),
                (fy, (<), Vector2d 0.0 1.0, maximum), (fy, (>), Vector2d 0.0 (-1.0), minimum)]

    forM_ wrap $ \(fn, op, v, mm) -> do
        when (fn v `op`(mm (map (fn . (^+^ pos)) vectors))) $ do
            drawObject (Object (vectors, tris) dir (pos ^-^ v^*!2))


draw :: GameState -> IO ()
draw gs@(GameState playerState particles bullets asteroids _ _ _) = do
    clear [ColorBuffer]
    drawText ("Number of particles: " ++ (show (length particles))) 0.1 (Vector2d (-0.9) 0.9)
    drawText ("Fps: " ++ (show (1.0/(gs_time gs - gs_prevTime gs)))) 0.05 (Vector2d (-0.9) (-0.9))

    when (ps_aliveState playerState == Alive) $ do
        drawObject $ Object playerModel (ps_direction playerState) (ps_position playerState)
        drawDuplicates $ Object playerModel (ps_direction playerState) (ps_position playerState)

    drawParticles particles

    let rng = mkStdGen $ 1220 + fromIntegral (toInteger (round((realToFrac (length particles) / 200.0))))

    forM_ bullets $ \(Bullet pos dir vel _) -> do
        drawObject (Object bulletModel dir pos)
        drawDuplicates (Object bulletModel dir pos)

    forM_ asteroids $ \(Asteroid dir vel vert) -> do
        drawAsteroid vert


someFunc :: IO ()
someFunc = do
    window <- initialize "Asteroids"
    time <- GLFW.getTime

    rng <- newStdGen
    let poly = (evalRand (randomPolygon 15 (Vector2d 0.01 0.01) 0.5 0.5) rng)

    let asteroid = Asteroid (Vector2d 0.0 0.0) (Vector2d 0.0 0.0) poly

    case time of
        Just t ->
            mainLoop draw window (GameState (PlayerState startPos startDir startVel 0 thrusters 0.0 Alive) [] [] [asteroid] (realToFrac t) (realToFrac t) rng)
        Nothing ->
            return ()
    return ()
