module Game.Update where
import Utils.Math
import State
import Control.Monad.State (State, put, execState, get, when)
import Player
import qualified Graphics.UI.GLFW as GLFW
import Data.Maybe
import Data.List hiding (intersect)
import Game.Effects
import Game.Asteroids


bulletVel = 0.90 :: Float
fireInterval = 0.23 :: Float
accRate = 0.3 :: Float
angularAcc = 3.0 :: Float


normalizePositions :: State GameState ()
normalizePositions = do
    get >>= put . (onPlayerState . onPlayerPos) wrap
    get >>= put . (onParticles (map (onParticlePos wrap)))
    get >>= put . (onBullets (map (onBulletPos wrap)))
    get >>= put . (onAsteroids (map (onPolygonVertices wrapVertices )))
    get >>= put . (onPolygonParticles (map (onPolygonParticleVertices wrapVertices )))


updateBullets :: Time -> Time -> [Bullet] -> [Bullet]
updateBullets time deltaT = filter ((>time) . b_lifeTime) . map updbul
    where updbul (Bullet pos dir vel life) = (Bullet (pos ^+^ (deltaT)!*^vel) dir vel life )


addBullets :: State GameState ()
addBullets = do
    state <- get

    let playerState@(PlayerState pos dir vel _ _ lastBullet _) = gs_playerState state

    let time = gs_time state

    when (lastBullet + fireInterval < time) $ do
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
            }:(gs_bullets state)
        }


bulletImpact :: Asteroid -> Bullet -> Float -> Maybe (Edge, Float)
bulletImpact asteroid bullet delta = if impacts /= [] then Just (v, t) else Nothing
    where 
          (a, b, v, s, t) = mini
          mini = if impacts /= [] then (head impacts) else undefined
          -- HACK, bigger s range due to rotations
          impacts = filter (\(b1, b2, e, s, t) -> s >= -2.5 && s <= 2.5 && t >= 0.0 && t <= 1.0) is
          is = [
              (b1, b2, v, s, t) | v <- (take nvs vs),
              bp <- map (bulPos ^+^) [Vector2d 0.0 0.0, Vector2d 2.0 0.0, Vector2d (-2.0) 0.0,
                                      Vector2d 0.0 2.0, Vector2d 0.0 (-2.0)],
              let angVel = a_angularVelocity asteroid,
              let (b1, b2) = (bp, bp ^+^ ((bulVel ^-^ astVel) ^-^ rotV centroid v (-angVel)) ^*! delta),
              let (s, t) = intersect (b1, b2) v]

          bulVel = b_velocity bullet
          bulPos = b_position bullet
          astVel = a_velocity asteroid

          rotV c (v1, v2) av = ortho $ ((interpolate v1 v2 0.5) ^-^ c) ^*! av

          nvs = length vertices
          centroid = polyCentroid (vertices)
          vs = polyEdges vertices
          vertices = a_vertices asteroid


detectCollisions :: State GameState (Bool)
detectCollisions = do
    state <- get
    let PlayerState pos dir vel _ _ _ _ = gs_playerState state
    let asteroids = gs_asteroids state
    let time = gs_time state
    let prevTime = gs_time state

    let (verts, tris) = playerModel

    let p1s = map ((^+^ pos) . (rebase dir)) verts
    let tri1s = [map ((^+^ pos) . (rebase dir)) tri | tri <- tris]

    let a = or (map (\(Asteroid _ _ vs) -> detectColl p1s vs (vel^*!(time - prevTime))) asteroids)
    let b = or (map (\(Asteroid _ _ vs) -> detectColl vs p1s (vel^*!(prevTime - time))) asteroids)
    let c = or (map (\(Asteroid _ _ vs) -> inside p1s vs) asteroids)
    let d = or [or (map (\(Asteroid _ _ vs) -> inside vs t) asteroids) | t <- tri1s]

    return (a || b || c || d)

    where detectColl ps vs dir = or [intersects (p, p ^+^ dir) (v1, v2) |
                                     p <- ps, (v1, v2) <- polyEdges vs]


runFrame :: State ProgramState ()
runFrame = do
    let keyCommands = [(GLFW.Key'E, Accelerating),
                       (GLFW.Key'S, TurningLeft),
                       (GLFW.Key'D, Decelerating),
                       (GLFW.Key'F, TurningRight),
                       (GLFW.Key'Space, Shooting),
                       (GLFW.Key'Escape, Escaping),
                       (GLFW.Key'Enter, Entering)]

    state <- get

    let actions = map snd $ filter ((`elem` (gls_keysHeld state)) . fst) keyCommands
    let newDown = gls_keysPressed state

    put $ state {
        gls_mode = if GLFW.Key'Escape `elem` newDown then Menu else Playing,
        gls_gameState = execState (runGameFrame actions) (gls_gameState state)
    }

    state <- get
    
    when ((GLFW.Key'Enter `elem` newDown) && (isGameOver . ps_aliveState . gs_playerState . gls_gameState $ state)) $ do
        put $ state { gls_mode = Restarting }

    where
        isGameOver (GameOver _) = True
        isGameOver _ = False

updatePlayer :: Float -> [Action] -> State GameState ()
updatePlayer delta actions = do
    state <- get

    let playerState = gs_playerState state
    let time = gs_time state
    let PlayerState pos dir vel angVel thrusters _ aliveState = playerState

    let turnAcc = ((if TurningRight `elem` actions then (-angularAcc) else 0.0) +
                   (if TurningLeft `elem` actions then angularAcc else 0.0))
    let angularVel = angVel + turnAcc*delta
    let deltaAngle = angVel * delta + 0.5 * turnAcc * delta * delta

    let turnMatrix = getTurnMatrix deltaAngle
    let newVel = if Accelerating `elem` actions then (vel ^+^ (delta * accRate)!*^dir ) else vel
    let accRate' = (if Accelerating `elem` actions then accRate else 0)
    let newPos = pos ^+^ (delta!*^vel) ^+^ (0.5*delta*delta*accRate')!*^dir

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


advanceLevel :: State GameState ()
advanceLevel = do
    state <- get
    put $ (initState (1 + (gs_level state)) 0.0 (gs_rng state)) {
        gs_score = gs_score state,
        gs_lives = gs_lives state
    }


chipAsts :: [(Asteroid, Maybe Edge)] -> State GameState([Asteroid])
chipAsts [] = return ([])
chipAsts ((ast, Nothing):asts) = do
    xs <- chipAsts asts
    return (ast : xs)
chipAsts ((ast, Just edge):asts) = do
    state <- get

    r <- rndFloat 0.0 1
    u <- rndFloat 0.2 0.5
    xs <- chipAsts asts
    if r < 0.2 then return (ast:xs) else return (chip ++ xs)
    where
        chip = map fst (splitAsteroid splitChip 1 ast edge 0.3)


handleBullets :: State GameState()
handleBullets = do
    state <- get

    let GameState playerState _ _ bullets asteroids time prevTime _ lives _ _ = state
    let delta = time - prevTime
    let impacts = [(bul, ast, m) | ast <- asteroids, bul <- bullets,
                   let m = bulletImpact ast bul delta, isJust m]
    let impactedAsteroids = map (\(_, ast, _) -> ast) impacts

    let newAsts = map ((\(bul, ast, Just ((v1, v2), t)) -> splitAsteroid splitBalanced 1 ast (v1, v2) t)) impacts
    chippedAsts <- chipAsts (concat newAsts)
    let newAsteroids = chippedAsts

    explodeNewAsteroids newAsteroids

    when ((not . null) impacts) $ do
        state <- get
        put $ state {
            gs_asteroids = ((gs_asteroids state) \\ impactedAsteroids) ++ newAsteroids,
            gs_bullets = filter (\b -> (and [(b /= bl) | (bl, _, _) <- impacts])) (gs_bullets state)
        }


runGameFrame :: [Action] -> State GameState ()
runGameFrame actions = do
    state <- get

    let GameState playerState _ _ bullets asteroids time prevTime _ lives _ _ = state
    let aliveState = ps_aliveState playerState
    let delta = time - prevTime

    case aliveState of
        Alive -> updatePlayer delta actions
        Exploding since -> do
            when (time - since > 1.0 && Entering `elem` actions && lives > 0) $ do
                initiateRespawn
            when (lives == 0) $ do
                put $ onPlayerState (\ps -> ps { ps_aliveState = GameOver time }) state
        Respawning since -> do
            when (time - since > 0.5) $ do
                finalizeRespawn
        Winning since -> do
            when (time - since > 0.5 && Entering `elem` actions) $ do
                advanceLevel
            updatePlayer delta actions
        _ -> return ()

    handleBullets

    state <- get

    put $ state {
        gs_time = time,
        gs_particles = updateParticles time delta (gs_particles state),
        gs_bullets = updateBullets time delta (gs_bullets state),
        gs_asteroids = updateAsteroids time delta (gs_asteroids state),
        gs_polygonParticles = updatePolygonParticles time delta (gs_polygonParticles state)
    }

    annihilateAsteroids

    normalizePositions

    when (asteroids == [] && (isAlive . ps_aliveState) playerState) $ do
        state <- get
        put $ onPlayerState (\s -> s { ps_aliveState = Winning time }) state

    where isAlive Alive = True
          isAlive _ = False
