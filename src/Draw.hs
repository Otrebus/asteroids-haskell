module Draw where

import qualified Graphics.Rendering.OpenGL as GL hiding (get, rotate)
import Graphics.Rendering.OpenGL (vertex, clear, ClearBuffer(ColorBuffer), renderPrimitive, PrimitiveMode(Lines, Points, QuadStrip, TriangleStrip, LineLoop, TriangleFan))
import Font
import Math
import State
import Control.Monad.State (State, put, execState, get)
import Control.Monad
import Player
import Game
import Data.Maybe
import qualified Graphics.UI.GLFW as GLFW


normalizePositions :: State GameState ()
normalizePositions = do
    state <- get
    put $ (onPlayerState . onPlayerPos) (wrap) state
    state <- get
    put $ onParticles (map (onParticlePos wrap)) state
    state <- get
    put $ onBullets (map (onBulletPos wrap)) state
    state <- get
    put $ onAsteroids (map (onPolygonVertices wrapVertices )) state
    state <- get
    put $ onPolygonParticles (map (onPolygonParticleVertices wrapVertices )) state


drawParticles :: [Particle] -> IO ()
drawParticles particles = do
    renderPrimitive Points $ forM_ particles renderParticle where
        renderParticle p = do
            let d = p_brightness p
            GL.color $ GL.Color4 d d d (d :: GL.GLfloat)
            vertex ((toVertex . p_position) p)


drawPolygon :: GL.Color4 Float -> GL.Color4 Float -> Vertices -> IO ()
drawPolygon back fore verts = do

    GL.color $ back
    renderPrimitive TriangleFan $ do mapM_ vertex (map toVertex verts)
    GL.color $ fore
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


updatePolygonParticles :: Time -> Time -> [PolygonParticle] -> [PolygonParticle]
updatePolygonParticles time deltaT particles = newParticles
    where
        filteredParticles = filter ((>time) . pp_lifeTime) $ particles
        newParticles = map (\(PolygonParticle polys vel angVel life) -> (
            let newPos = (rotateAround (angVel*deltaT) (polyCentroid polys) . ((deltaT)!*^vel ^+^)) in
                PolygonParticle (map newPos polys) vel angVel life)) filteredParticles
                

updateBullets :: Time -> Time -> [Bullet] -> [Bullet]
updateBullets time deltaT = filter ((>time) . b_lifeTime) . map (\(Bullet pos dir vel life) -> (Bullet (pos ^+^ (deltaT)!*^vel) dir vel life ))


runFrame :: [GLFW.Key] -> State ProgramState ()
runFrame input = do

    let keyCommands = [(GLFW.Key'E, Accelerating),
                (GLFW.Key'S, TurningLeft),
                (GLFW.Key'D, Decelerating),
                (GLFW.Key'F, TurningRight),
                (GLFW.Key'Space, Shooting),
                (GLFW.Key'Escape, Escaping),
                (GLFW.Key'Enter, Entering)
                ]

    let actions = map snd $ filter ((`elem` input) . fst) keyCommands

    state <- get

    let newState = state {
        gls_gameState = execState (runGameFrame actions) (gls_gameState state)
    }

    let prevPressed = gls_keysPressed state
    let newDown = filter (\k -> not (k `elem` prevPressed)) input

    put $ newState { gls_mode = if GLFW.Key'Escape `elem` newDown then Menu else Playing }


updatePlayer :: Float -> [Action] -> State GameState ()
updatePlayer delta actions = do

    state <- get

    let GameState (playerState@(PlayerState pos dir vel angVel thrusters lastBullet aliveState)) particles polygonParticles bullets asteroids time prevTime score lives rng = state

    let turnAcc = ((if TurningRight `elem` actions then (-angularAcc) else 0.0) +
                   (if TurningLeft `elem` actions then angularAcc else 0.0))
    let angularVel = angVel + turnAcc*delta
    let deltaAngle = angVel * delta + 0.5 * turnAcc * delta * delta

    let turnMatrix = getTurnMatrix deltaAngle
    let newVel = if Accelerating `elem` actions then (vel ^+^ (delta * accRate)!*^dir ) else vel
    let newPos = pos ^+^ (delta!*^vel) ^+^ (0.5*delta*delta*(if Accelerating `elem` actions then accRate else 0))!*^dir

    put $ state {
        gs_playerState = (gs_playerState state) {
            ps_position = newPos,
            ps_direction = (turnMatrix #*^ dir),
            ps_angularVelocity = angularVel,
            ps_velocity = newVel
        }
    }

    b <- detectCollisions
    when b $ do
        state <- get
        put $ onPlayerState (\s -> s { ps_aliveState = Exploding time }) state
        explodeShip

    addEnginesParticles actions
    when (Shooting `elem` actions) $ do
        addBullets


initiateRespawn :: State GameState ()
initiateRespawn = do
    state <- get
    put $ onPlayerState (\ps -> ps { ps_aliveState = Respawning (gs_time state) } ) state


finalizeRespawn :: State GameState ()
finalizeRespawn = do
    state <- get
    put $ state {
        gs_playerState = PlayerState startPos startDir startVel 0 thrusters 0.0 Alive,
        gs_lives = (gs_lives state) - 1
    }


runGameFrame :: [Action] -> State GameState ()
runGameFrame actions = do

    state <- get

    let GameState (playerState@(PlayerState pos dir vel angVel thrusters lastBullet aliveState)) particles polygonParticles bullets asteroids time prevTime score lives rng = state
    let delta = time - prevTime

    case aliveState of
        Alive -> updatePlayer delta actions
        Exploding since -> do
            when (time - since > 1.0 && Entering `elem` actions && lives > 0) $ do
                initiateRespawn
        Respawning since -> do
            when (time - since > 0.5) $ do
                finalizeRespawn

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
    let newPolygonParticles = updatePolygonParticles time delta (gs_polygonParticles state)

    put $ state {
        gs_time = time,
        gs_particles = newParticles,
        gs_bullets = newBullets,
        gs_asteroids = newAsteroids,
        gs_polygonParticles = newPolygonParticles
    }

    annihilateAsteroids

    normalizePositions


drawObject :: Object -> IO ()
drawObject (Object (lineVertices, triangles) (Vector2d a b) pos) = do

    let mat = Matrix2d b a (-a) b
    let tris = map (\v -> (map (toVertex . (pos ^+^) . ((#*^) mat)) v)) triangles
    let ys = map (toVertex . (pos ^+^) . ((#*^) mat)) lineVertices

    GL.color $ GL.Color4 0 0 0 (1 :: GL.GLfloat)
    forM_ tris (\tri -> renderPrimitive TriangleStrip $ (do mapM_ vertex tri))

    GL.color $ GL.Color4 1 1 1 (1 :: GL.GLfloat)
    renderPrimitive LineLoop $ do mapM_ vertex ys
    return ()


drawDuplicates :: Object -> IO ()
drawDuplicates (Object (vectors, tris) dir pos) = do

    let (Vector2d x y) = pos
    let (Vector2d a b) = dir
    let mat = Matrix2d b a (-a) b
    let mv = map ((pos ^+^) . ((#*^) mat)) vectors

    let fx (Vector2d x y) = x; fy (Vector2d x y) = y

    let a = 2*(-signum x) :: Float
    let b = 2*(-signum y) :: Float

    let wrap = [Vector2d a 0.0, Vector2d 0.0 b, Vector2d a 0.0, Vector2d a b]

    forM_ wrap $ \v -> do
        drawObject (Object (vectors, tris) dir (pos ^+^ v))


drawDuplicatesAsteroids :: Vertices -> IO ()
drawDuplicatesAsteroids vectors = do

    let (Vector2d x y) = polyCentroid vectors

    let a = 2*(-signum x) :: Float
    let b = 2*(-signum y) :: Float

    let fx (Vector2d x y) = x; fy (Vector2d x y) = y

    let wrap = [Vector2d a 0.0, Vector2d 0.0 b, Vector2d a 0.0, Vector2d a b]

    forM_ wrap $ \v -> do
        drawPolygon (GL.Color4 0.05 0.05 0.05 1.0) (GL.Color4 1.0 1.0 1.0 1.0) $ map (^+^ v) vectors


drawLife :: AliveState -> Float -> Int -> Int -> IO ()
drawLife (Respawning since) time maxLife life  = do
    return ()
    let x = 0.92 - ((realToFrac life - 1.0))*0.12
    let t = 2.0*(time - since)
    let startAng = pi/4 - angle startDir
    let (pos, rot, scale) = if maxLife == life then (interpolate (Vector2d x 0.92) (Vector2d 0 0) t, -pi/4 + (startAng+pi/4)*t, 0.8 + (1.0-0.8)*t) else ((Vector2d x 0.92), -pi/4, 0.8)
    let pm = map (pos ^+^) (map ((scale !*^) . (rotate (rot))) (fst playerModel))
    drawPolygon (GL.Color4 0.00 0.00 0.00 1.0) (GL.Color4 1.0 1.0 1.0 1.0) pm
drawLife aliveState time max life = drawPolygon (GL.Color4 0.00 0.00 0.00 1.0) (GL.Color4 1.0 1.0 1.0 1.0) pm
    where
        x = 0.92 - ((realToFrac life - 1.0))*0.12
        pm = map ((Vector2d x 0.92) ^+^) (map ((0.8 !*^) . (rotate (-pi/4))) (fst playerModel))            


drawLives :: GameState -> IO ()
drawLives state = do

    let score = gs_score state
    let lives = gs_lives state
    let time = gs_time state
    let aliveState = (ps_aliveState . gs_playerState) state

    mapM_ (drawLife aliveState time lives) [1..lives]

    return ()

draw :: GameState -> IO ()
draw gs@(GameState playerState particles polygonParticles bullets asteroids time _ score lives _) = do

    clear [ColorBuffer]

    drawText 0.1 (Vector2d (-0.92) 0.88) ("Score: " ++ (show . round $ score))
    drawText 0.05 (Vector2d (-0.9) (-0.9)) ("Fps: " ++ (show (1.0/(gs_time gs - gs_prevTime gs))))

    drawLives gs

    let aliveState = ps_aliveState playerState

    case aliveState of
        Alive -> do
            drawObject $ Object playerModel (ps_direction playerState) (ps_position playerState)
            drawDuplicates $ Object playerModel (ps_direction playerState) (ps_position playerState)
        Exploding since -> do
            when (time - since > 1.0) $ do
                when (lives > 0) $ do
                    centerText 0.1 (Vector2d (-0.3) 0.0) (Vector2d 0.3 0.0) "Press enter to respawn"
                    return ()
                when (lives == 0) $ do
                    centerText 0.1 (Vector2d (-0.3) 0.0) (Vector2d 0.3 0.0) "Game Over"
                    return ()
                return ()
        Respawning t -> do
            return ()

    drawParticles particles

    forM_ bullets $ \(Bullet pos dir vel _) -> do
        drawObject (Object bulletModel dir pos)
        drawDuplicates (Object bulletModel dir pos)

    forM_ asteroids $ \(Asteroid dir vel vert) -> do
        drawPolygon (GL.Color4 0.05 0.05 0.05 1.0) (GL.Color4 1.0 1.0 1.0 1.0) vert
        drawDuplicatesAsteroids vert

    forM_ polygonParticles $ \(PolygonParticle vert vel angVel life) -> do
        let t = abs (life - time)
        let c = min 1.0 t ** 1.2
        drawPolygon (GL.Color4 0.05 0.05 0.05 1.0) (GL.Color4 c c c c) vert
