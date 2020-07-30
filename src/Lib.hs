module Lib (someFunc) where

import Debug.Trace
import System.IO (hPutStrLn, stderr)
import Control.Monad
import qualified Graphics.Rendering.OpenGL as GL hiding (get, rotate)
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (vertex, clear, ClearBuffer(ColorBuffer), renderPrimitive, PrimitiveMode(Lines, Points, QuadStrip, TriangleStrip, LineLoop, TriangleFan))
import Player
import Data.List (sort, sortOn)
import System.Exit (exitFailure)
import Data.Fixed (mod')
import Math
import GameState
import Control.Monad.State (State, put, execState, get)
import Font (chars)
import Data.Foldable (maximumBy)
import Data.Ord (comparing)
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


addExplosionParticle :: Time -> Vector2d -> Vector2d -> Float -> State GameState (Particle)
addExplosionParticle time pos dir pw = do

    v <- liftM (**1.1) (rndFloat 0 pw)
    ang <- liftM (**1.1) (rndFloat 0 2.3)
    x <- rndFloat (-1) 1

    let pvel = normalize (rotate (x*ang) dir)^*!v
    let ppos = pos

    return (Particle ppos pvel time (time + 1.5) 0.8)


addExplosion :: Time -> Vector2d -> Vector2d -> Int -> Float -> State GameState ()
addExplosion time pos dir n pw = do

    particles <- replicateM n (addExplosionParticle time pos dir pw)

    state <- get
    put (state { gs_particles = particles ++ (gs_particles state) } )


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
bulletImpact asteroid bullet delta = if impacts /= [] then Just (v, t) else Nothing
    where 
          (a, b, v, s, t) = mini
          mini = if impacts /= [] then (head impacts) else undefined
          -- HACK, bigger s range due to rotations
          impacts = filter (\(b1, b2, e, s, t) -> s >= -2.5 && s <= 2.5 && t >= 0.0 && t <= 1.0) is
          is = [
              (b1, b2, v, s, t) | v <- (take nvs vs),
              bp <- [bulPos, bulPos ^+^ Vector2d 2.0 0.0, bulPos ^+^ Vector2d (-2.0) 0.0, bulPos ^+^ Vector2d 0.0 2.0, bulPos ^+^ Vector2d 0.0 (-2.0)],
              let (b1, b2) = (bp, bp ^+^ ((bulVel ^-^ astVel) ^-^ rotV centroid v (a_angularVelocity asteroid)) ^*! delta),
              let (s, t) = intersect (b1, b2) v]

          bulVel = b_velocity bullet
          bulPos = b_position bullet
          astVel = a_velocity asteroid

          rotV c (v1, v2) av = let Vector2d x y = ((interpolate v1 v2 0.5) ^-^ c) ^*! av in Vector2d (-y) x

          nvs = length vertices
          centroid = polyCentroid (vertices)
          vs = zip vertices ((tail . cycle) vertices)
          vertices = a_vertices asteroid


-- bulletImpact :: Asteroid -> Bullet -> Float -> Maybe ((Vector2d, Vector2d), Float)
-- bulletImpact asteroid bullet delta = if impact then Just (mini, t) else Nothing
--     where 
--           mini = if impact then (head is) else (undefined, undefined)
--           (s, t) = if impact then intersect (b1, b2) (head is) else (0, 0)
--           impact = (not . null) is
--           is = filter (\v -> intersects (b1, b2) v) vs
--           (b1, b2) = (b_position bullet, (b_position bullet) ^+^ ((b_velocity bullet) ^-^ (a_velocity asteroid))^*!delta)
--           vs = zip vertices ((tail . cycle) vertices)
--           vertices = a_vertices asteroid


getTurnMatrix :: Angle -> Matrix2d
getTurnMatrix theta = Matrix2d (cos theta) (-sin(theta)) (sin theta) (cos theta)


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


rotateVertices asteroid delta = map (\v -> ( (getTurnMatrix (delta*angVel))#*^(v ^-^ cent) ^+^ cent)) vs
    where
        cent = polyCentroid vs
        angVel = a_angularVelocity asteroid
        vs = a_vertices asteroid


moveVertices vel delta = map (\v -> v ^+^ (delta)!*^vel)


updateAsteroids time delta =
    map (\asteroid -> 
    let 
        movedVertices = moveVertices (a_velocity asteroid) delta rotatedVertices
        rotatedVertices = rotateVertices asteroid delta
    in asteroid { a_vertices = movedVertices })

wrap :: Vector2d -> Vector2d
wrap (Vector2d x y) = Vector2d (mod' (x + 1.0) 2.0 - 1.0) (mod' (y + 1.0) 2.0 - 1.0)


wrapVertices :: Vertices -> Vertices
wrapVertices vs = map (^+^ (wrap (polyCentroid vs))) cfVs
    where cfVs = map (^-^ polyCentroid vs) vs


normalizePositions :: State GameState ()
normalizePositions = do
    state <- get
    put $ (onPlayerState . onPlayerPos) (wrap) state
    state <- get
    put $ onParticles (map (\ps -> ps { p_position = wrap (p_position ps) })) state
    state <- get
    put $ onBullets (map (\bs -> bs { b_position = wrap (b_position bs) })) state
    state <- get
    put $ onAsteroids (map (\as -> as { a_vertices = wrapVertices (a_vertices as) })) state


inside :: Vertices -> Vertices -> Bool
inside as bs = or [and [(c ^-^ b) ^%^ (a ^-^ b) > 0 | (b, c) <- zip bs ((tail . cycle) bs)] | a <- as]


intersect :: (Vector2d, Vector2d) -> (Vector2d, Vector2d) -> (Float, Float)
intersect (Vector2d p1x p1y, Vector2d p2x p2y) (Vector2d v1x v1y, Vector2d v2x v2y) =
    (s, t)
    where
        a = p1x - p2x; b = v2x - v1x; c = p1y - p2y; d = v2y - v1y; e = p1x - v1x; f = p1y - v1y
        s = (e*d - b*f)/(a*d - b*c); t = (a*f - e*c)/(a*d - b*c)


intersects a b = let (s, t) = intersect a b in t >= 0 && t <= 1 && s >= 0.0 && s <= 1


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


polyCentroid :: Vertices -> Vector2d
polyCentroid vs = (Vector2d xs ys)^/!(6*area)
    where
        area = polyArea vs
        xs = sum [ (xi+xi1)*(xi*yi1 - xi1*yi) | ((Vector2d xi yi), (Vector2d xi1 yi1)) <- zip vs ((tail . cycle) vs)]
        ys = sum [ (yi+yi1)*(xi*yi1 - xi1*yi) | ((Vector2d xi yi), (Vector2d xi1 yi1)) <- zip vs ((tail . cycle) vs)]


calcRatio :: Vertices -> Float
calcRatio [] = 0
calcRatio vs = let area = polyArea vs; circ = polyCirc vs in (area*4*pi)/(circ*circ)


loopSplits :: (Vector2d, Vector2d) -> Float -> Vertices -> (Vector2d, Vector2d) -> Vertices -> (Vertices, Vertices)
loopSplits (p1, p2) t (xs) (v1, v2) [] = ([], [])
loopSplits (p1, p2) t (xs) (v1, v2) (y:ys)
    | otherwise = maximumBy (comparing (\(a, b) -> (calcRatio a)*(calcRatio b))) (ds:cs)
    where
        cs = [splitAt u | u <- [0.1, 0.2..1]]
        ds = loopSplits (p1, p2) t (xs ++ [v2]) (v2, head ys) (ys)
        splitAt s =
            ((interpolate p1 p2 t):xs ++ ([interpolate v1 v2 s]), (interpolate v1 v2 s):y:(ys ++ [interpolate p1 p2 t]))
        maximumOn f = foldr1 (\a xs -> if f a > f xs then a else xs)


maprest :: [a] -> [[a]]
maprest (x:xs) = (x:xs) : maprest xs


splitAsteroid :: Asteroid -> (Vector2d, Vector2d) -> Float -> (Asteroid, Asteroid)
splitAsteroid asteroid (p1, p2) t =
    (Asteroid (a_angularVelocity asteroid) ((a_velocity asteroid) ^+^ Vector2d (-day) dax ^+^ (normalize a)^*!0.05) as, Asteroid (a_angularVelocity asteroid) ((a_velocity asteroid) ^+^ Vector2d (-dby) dbx ^+^ (normalize b)^*!0.05) bs)
    where
        Vector2d dax day = a^*!(a_angularVelocity asteroid)
        Vector2d dbx dby = b^*!(a_angularVelocity asteroid)
        a = (polyCentroid as) ^-^ (polyCentroid (a_vertices asteroid))
        b = (polyCentroid bs) ^-^ (polyCentroid (a_vertices asteroid))
        (as, bs) = loopSplits (p1, p2) t [p2] (v1, v2) (v2:takeWhile (/= p2) rest)
        (v1:v2:rest):_ = (dropWhile ((/= p2) . head)) $ maprest (cycle vs)
        vs = a_vertices asteroid


explodeNewAsteroids :: Time -> [Asteroid] -> Int -> State GameState ()
explodeNewAsteroids time [] n = return ()
explodeNewAsteroids time (x:y:zs) n = do

    let vs = a_vertices x
    addExplosion time (head vs) (normalize ((head vs) ^-^ (last vs))) 100 2.5
    addExplosion time (last vs) (normalize ((last vs) ^-^ (head vs))) 15 1

    explodeNewAsteroids time zs n


randomPolyDivision :: [Vector2d] -> Int -> Rand StdGen [[Vector2d]]
randomPolyDivision vs 0 = return [vs]
randomPolyDivision v r = do
    let n = length v
    let vs = cycle v

    i <- getRandomR(0, n-2)
    j <- liftM (i+) (getRandomR (i+1, n-1))

    if i == j
        then
            randomPolyDivision v r
        else do
            a <- getRandomR(0.4, 0.6 :: Float)
            b <- getRandomR(0.4, 0.6 :: Float)

            let p1 = (interpolate (vs !! i) (vs !! (i+1)) a)
            let p2 = (interpolate (vs !! j) (vs !! (j+1)) b)
            let vs1 = p2 : p1 : (take (j-i) (drop (i+1) vs))
            let vs2 = p1 : p2 : (take ((n-(j-i))) (drop (j+1) vs))

            v1s <- randomPolyDivision vs1 (r-1)
            v2s <- randomPolyDivision vs2 (r-1)

            return (v1s ++ v2s)


runFrame :: [Action] -> State GameState ()
runFrame actions = do
    state <- get

    let GameState (playerState@(PlayerState pos dir vel angVel thrusters lastBullet aliveState)) particles bullets asteroids time prevTime rng = state
    let delta = time - prevTime

    let turnAcc = ((if TurningRight `elem` actions then (-angularAcc) else 0) +
                   (if TurningLeft `elem` actions then angularAcc else 0))
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
        state <- get
        put $ onPlayerState (\s -> s { ps_aliveState = Dead }) state

    forM asteroids $ (\a -> do
        trace (show (polyCirc (a_vertices a))) $ return () )

    let impacts = filter (\(bul, ast, m) -> isJust m) [(bul, ast, bulletImpact ast bul delta) | ast <- asteroids, bul <- bullets]
    let impactedAsteroids = map (\(_, ast, _) -> ast) impacts

    let newAsteroidPairs = map ((\(bul, ast, Just ((v1, v2), t)) -> splitAsteroid ast (v1, v2) t)) impacts
    let newAsteroids = concat $ map (\(a, b) -> [a, b]) newAsteroidPairs

    explodeNewAsteroids time newAsteroids 100

        -- bulletImpact :: Asteroid -> Bullet -> Float -> Maybe ((Vector2d, Vector2d), Float)

    when ((not . null) impacts) $ do
        state <- get
        put $ state {
            gs_asteroids = (filter (\ast -> (not (elem ast impactedAsteroids))) (gs_asteroids state)) ++ newAsteroids,
            gs_bullets = filter (\b -> (and [(b /= bl) | (bl, _, _) <- impacts])) (gs_bullets state)
        }

    state <- get
    -- TODO: do get/put inside these functions instead
    let newParticles = updateParticles time delta (gs_particles state)
    let newBullets = updateBullets time delta (gs_bullets state)
    let newAsteroids = updateAsteroids time delta (gs_asteroids state)

    put $ state {
        gs_playerState = (gs_playerState state) {
            ps_position = newPos,
            ps_direction = (turnMatrix #*^ dir),
            ps_angularVelocity = angularVel,
            ps_velocity = newVel
        },
        gs_time = time,
        gs_particles = newParticles,
        gs_bullets = newBullets,
        gs_asteroids = newAsteroids
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


drawDuplicatesAsteroids :: Vertices -> IO ()
drawDuplicatesAsteroids vectors = do
    let fx (Vector2d x y) = x; fy (Vector2d x y) = y

    let wrap = [(fx, (<), Vector2d 1.0 0.0, maximum), (fx, (>), Vector2d (-1.0) 0.0, minimum),
                (fy, (<), Vector2d 0.0 1.0, maximum), (fy, (>), Vector2d 0.0 (-1.0), minimum)]

    forM_ wrap $ \(fn, op, v, mm) -> do
        when (fn v `op` (mm (map fn vectors))) $ do
            drawAsteroid $ map (^-^ v^*!2) vectors


draw :: GameState -> IO ()
draw gs@(GameState playerState particles bullets asteroids _ _ _) = do
    clear [ColorBuffer]
    drawText ("Number of particles: " ++ (show (length particles))) 0.1 (Vector2d (-0.9) 0.9)
    drawText ("Fps: " ++ (show (1.0/(gs_time gs - gs_prevTime gs)))) 0.05 (Vector2d (-0.9) (-0.9))

    when (ps_aliveState playerState == Alive) $ do
        drawObject $ Object playerModel (ps_direction playerState) (ps_position playerState)
        drawDuplicates $ Object playerModel (ps_direction playerState) (ps_position playerState)

    drawParticles particles

    let poly = (evalRand (randomPolyDivision [Vector2d 0 0, Vector2d 0 0.2, Vector2d 0.2 0.0] 4) (mkStdGen 123))
    print poly
    forM_ poly $ \p -> do
        drawObject (Object (p, [p]) (Vector2d 1.0 0) (Vector2d 0 0))

    let rng = mkStdGen $ 1220 + fromIntegral (toInteger (round((realToFrac (length particles) / 200.0))))

    forM_ bullets $ \(Bullet pos dir vel _) -> do
        drawObject (Object bulletModel dir pos)
        drawDuplicates (Object bulletModel dir pos)

    forM_ asteroids $ \(Asteroid dir vel vert) -> do
        drawAsteroid vert
        drawDuplicatesAsteroids vert


someFunc :: IO ()
someFunc = do
    window <- initialize "Asteroids"
    time <- GLFW.getTime

    rng <- newStdGen
    let poly = (evalRand (randomPolygon 13 (Vector2d 0.21 0.21) 0.5 0.5) rng)

    let asteroid = Asteroid 0.25 (Vector2d 0.0 0.0) poly

    case time of
        Just t ->
            mainLoop draw window (GameState (PlayerState startPos startDir startVel 0 thrusters 0.0 Alive) [] [] [asteroid] (realToFrac t) (realToFrac t) rng)
        Nothing ->
            return ()
    return ()
