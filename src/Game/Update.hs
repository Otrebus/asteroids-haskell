module Game.Update where
import Utils.Math
import State
import Control.Monad.State (State, put, execState, get, when)
import Data.Ord (comparing)
import Player
import qualified Graphics.UI.GLFW as GLFW
import Data.Maybe
import Data.List hiding (intersect)
import Game.Effects
import Game.Asteroids


-- As an object moves outside the screen, this makes sure the coordinates eventually wrap around
normalizePositions :: State GameState ()
normalizePositions = do
    get >>= put . (onPlayerState . onPlayerPos) wrap
    get >>= put . (onParticles (map (onParticlePos wrap)))
    get >>= put . (onBullets (map (onBulletPos wrap)))
    get >>= put . (onAsteroids (map (onPolygonVertices wrapVertices )))
    get >>= put . (onPolygonParticles (map (onPolygonParticleVertices wrapVertices )))


-- Moves bullets
updateBullets :: Time -> Time -> [Bullet] -> [Bullet]
updateBullets time deltaT = filter ((>time) . b_lifeTime) . map updbul
    where updbul (Bullet pos dir vel life) = (Bullet (pos ^+^ (deltaT)!*^vel) dir vel life )


-- Shoots bullets
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


-- Checks whether an asteroid impacts a bullet during a duration of time
bulletImpact ::
    Asteroid ->         -- The asteroid
    Bullet ->           -- The bullet
    Time ->            -- The duration of time that they move
    Maybe (Edge, Float) -- The edge that was hit and the position (in [0, 1]) where it was hit
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


-- Detects collisions between the player and the asteroids
detectCollisions ::
    State GameState (Bool)
detectCollisions = do
    state <- get
    let PlayerState pos dir vel _ _ _ _ = gs_playerState state
    let asteroids = gs_asteroids state
    let time = gs_time state
    let prevTime = gs_time state

    let (verts, tris) = playerModel

    let p1s = map ((^+^ pos) . (rebase dir)) verts
    let tri1s = [map ((^+^ pos) . (rebase dir)) tri | tri <- tris]

    let a = or (map (\(Asteroid _ _ vs _) -> detectColl p1s vs (vel^*!(time - prevTime))) asteroids)
    let b = or (map (\(Asteroid _ _ vs _) -> detectColl vs p1s (vel^*!(prevTime - time))) asteroids)
    let c = or (map (\(Asteroid _ _ vs _) -> inside p1s vs) asteroids)
    let d = or [or (map (\(Asteroid _ _ vs _) -> inside vs t) asteroids) | t <- tri1s]

    return (a || b || c || d)

    where detectColl ps vs dir = or [intersects (p, p ^+^ dir) (v1, v2) |
                                     p <- ps, (v1, v2) <- polyEdges vs]


-- Updates the position of the player based on time elapsed and the player's actions
updatePlayer ::
    Time ->     -- The amount of time that passed since the last update
    [Action] -> -- The set of actions that the player is performing
    State GameState ()
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


-- Initializes the respawn animation
initiateRespawn ::
    State GameState ()
initiateRespawn = do
    state <- get
    put $ onPlayerState (\ps -> ps { ps_aliveState = Respawning (gs_time state) } ) state


-- After the respawn animation has finished, this is called to get the player starting
finalizeRespawn ::
    State GameState ()
finalizeRespawn = do
    state <- get
    put $ state {
        gs_playerState = PlayerState startPos startDir startVel 0 thrusters 0.0 Alive,
        gs_lives = (gs_lives state) - 1
    }


-- Advances the game to the next level with more and bigger asteroids
advanceLevel ::
    State GameState ()
advanceLevel = do
    state <- get
    put $ (initState (1 + (gs_level state)) 0.0 (gs_rng state)) {
        gs_score = gs_score state,
        gs_lives = gs_lives state
    }


-- Checks for and handles possible impacts of bullets currently traveling in the world
handleImpacts ::
    Time ->                              -- The delta of the time that passed since the previous call
    [Asteroid] ->                        -- The asteroids in the world
    State GameState([(Bool, Asteroid)])  -- Each asteroid and whether it was hit
handleImpacts _ [] = return []
handleImpacts delta (ast:xs) = do
    rest <- handleImpacts delta xs

    state <- get
    let bullets = gs_bullets state
    let impacts = [(bul, ast, m) | bul <- bullets, let m = bulletImpact ast bul delta, isJust m]
    case ((not . null) $ impacts) of
        True -> do
            state <- get
            put $ state {
                gs_bullets = filter (\b -> not $ b `elem` ((map (\(i, _, _) -> i) impacts))) (gs_bullets state)
            }

            let (_, ast, Just ((v1, v2), t)) = head impacts
            let splitBalanced = maximumBy (comparing (\((a, _), (b, _)) -> (calcRatio a)*(calcRatio b)))
            x <- chipAsteroids (splitAsteroid splitBalanced 1 ast (v1, v2) t)

            state <- get

            return ((map (\l -> (True, l)) x) ++ rest)
        False ->
            return ((False, ast):rest)


-- Handles the effects of bullets that are currently travelling through the world
handleBullets :: State GameState()
handleBullets = do
    state <- get

    let GameState playerState _ _ bullets asteroids time prevTime _ lives _ _ = state
    let delta = time - prevTime

    newAsteroids <- handleImpacts delta asteroids

    explodeNewAsteroids $ map snd (filter fst newAsteroids)

    state <- get
    put $ state { gs_asteroids = map snd newAsteroids }


-- The top level function of the gameplay loop that invokes other functions to update the game
-- based on the actions of the player and the behavior of the gameplay objects
runGameFrame ::
    [Action] ->        -- The current input of the player
    State GameState ()
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
        gs_asteroids = updateAsteroids delta (gs_asteroids state),
        gs_polygonParticles = updatePolygonParticles time delta (gs_polygonParticles state)
    }

    annihilateAsteroids
    normalizePositions

    when (asteroids == [] && (isAlive . ps_aliveState) playerState) $ do
        state <- get
        put $ onPlayerState (\s -> s { ps_aliveState = Winning time }) state

    where isAlive Alive = True
          isAlive _ = False


-- Changes the global state of the program depending on the player's actions (escaping to the menu, for example)
runFrame ::
    State ProgramState ()
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
