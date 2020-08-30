module Update where

import Math
import State
import Control.Monad.State (State, put, execState, get, when, filterM, liftM, forM_, replicateM)
import Player
import Data.Ord (comparing)
import Data.Foldable (maximumBy)
import qualified Graphics.UI.GLFW as GLFW
import Data.Maybe


bulletVel = 0.90 :: Float
fireRate = 0.23 :: Float -- time between bullets
accRate = 0.2 :: Float -- screens per second per second
angularAcc = 2.0 :: Float


rotateVertices asteroid delta = map (\v -> ( (getTurnMatrix (delta*angVel))#*^(v ^-^ cent) ^+^ cent)) vs
    where
        cent = polyCentroid vs
        angVel = a_angularVelocity asteroid
        vs = a_vertices asteroid


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


addExplosionParticle :: Time -> Vector2d -> Vector2d -> Vector2d -> Float -> Float -> State GameState (Particle)
addExplosionParticle time pos dir vel pw life = do

    v <- liftM (**1.1) (rndFloat 0 pw)
    ang <- liftM (**1.1) (rndFloat 0 2.3)
    x <- rndFloat (-1) 1
    ang2 <- rndFloat 0 (2*pi)

    let pvel = if (dir /= (Vector2d 0.0 0.0)) then (normalize (rotate (x*ang) dir))^*!v*v else (rotate ang2 (Vector2d 1.0 0.0))^*!v
    let ppos = pos

    return (Particle ppos (pvel ^+^ vel) time (time + life) 0.8)


addExplosion :: Time -> Vector2d -> Vector2d -> Vector2d -> Int -> Float -> Float -> State GameState ()
addExplosion time pos dir vel n pw life = do

    particles <- replicateM n (addExplosionParticle time pos dir vel pw life)

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


addEngineParticles action actions thrusterGetter fp start current = do

    state@(GameState (PlayerState pos dir vel angVel _ _ _) _ _ _ _ time prevTime _ _ _ _) <- get

    let nextParticleTime = current + (t_emissionInterval (thrusterGetter state))
    when (action `elem` actions && nextParticleTime < time) $ do

        let deltaAngle = angVel * (nextParticleTime - time)
        let turnMatrix = getTurnMatrix deltaAngle

        let newDir = turnMatrix #*^ dir

        addEngineParticle nextParticleTime (thrusterGetter state) 0.6 (pos ^+^ (vel ^*! (nextParticleTime-start))) newDir vel
        state <- get
        put $ ((onPlayerState . onThrusters . fp) (\t -> t { t_lastEmitted = nextParticleTime } )) state
        addEngineParticles action actions thrusterGetter fp start nextParticleTime


addEnginesParticles :: [Action] -> State GameState ()
addEnginesParticles actions = do

    gs@(GameState (PlayerState pos dir vel _ _ _ _) _ _ _ _ time prevTime _ _ _ _) <- get

    let thfun = (ps_thrusters . gs_playerState)
    let engines = [(Accelerating, e_main . thfun, onMainThruster),
                   (TurningLeft, e_topright . thfun, onTopRightThruster),
                   (TurningRight, e_topleft . thfun, onTopLeftThruster),
                   (TurningLeft, e_bottomleft . thfun, onBottomLeftThruster),
                   (TurningRight, e_bottomright . thfun, onBottomRightThruster)]

    forM_ engines $ \(ac, th, fp) -> do
        let t0 = t_lastEmitted (th gs); int = t_emissionInterval (th gs)
        let t = t0 + int * realToFrac (floor ((prevTime - t0)/int))
        addEngineParticles ac actions th fp t t


addBullets :: State GameState ()
addBullets = do
    state <- get
    let GameState (playerState@(PlayerState pos dir vel _ thrusters lastBullet aliveState)) _ _ bullets _ time _ _ _ _ _ = state
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


maprest :: [a] -> [[a]]
maprest (x:xs) = (x:xs) : maprest xs


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
    addExplosion time (head vs) (normalize ((head vs) ^-^ (last vs))) (a_velocity x) 100 1.5 1.5
    addExplosion time (last vs) (normalize ((last vs) ^-^ (head vs))) (a_velocity y) 15 1 1.5

    explodeNewAsteroids time zs n


randomPolyDivision :: [Vector2d] -> Int -> State GameState [[Vector2d]]
randomPolyDivision vs 0 = return [vs]
randomPolyDivision v r = do
    let n = length v
    let vs = cycle v

    i <- rndInt 0 (n `div` 2 - 1)
    let j = (i + (n `div` 2)) `mod` n

    if i == j then randomPolyDivision v r
    else do
        a <- rndFloat 0.4 0.6
        b <- rndFloat 0.4 0.6

        let p1 = (interpolate (vs !! i) (vs !! (i+1)) a)
        let p2 = (interpolate (vs !! j) (vs !! (j+1)) b)
        let vs1 = p2 : p1 : (take (j-i) (drop (i+1) vs))
        let vs2 = p1 : p2 : (take ((n-(j-i))) (drop (j+1) vs))

        v1s <- randomPolyDivision vs1 (r-1)
        v2s <- randomPolyDivision vs2 (r-1)

        return (v1s ++ v2s)


explodePolygon :: Vertices -> Vector2d -> Vector2d -> Float -> State GameState ()
explodePolygon p pos vel time = do
    state <- get
    newPs <- randomPolyDivision p 3
    ppps <- mapM (\p -> (launchPolygon p pos time vel)) newPs
    put $ (onPolygonParticles (\pps -> ppps ++ pps)) state

    where 
        launchPolygon poly pos time vel = do
            v <- liftM (**1.2) (rndFloat 5.0 15.0)
            ang <- liftM (**1.1) (rndFloat 0 2.3)
            x <- rndFloat (-1) 1
            angVel <- liftM (**2.1) (rndFloat 0 3.0)
            dirAng <- rndFloat (-1) 1

            let polyVel = ((polyCentroid poly) ^-^ pos)^*!v

            return (PolygonParticle poly (vel ^+^ polyVel) (dirAng*angVel) (time + 1.5))


explodeShip :: State GameState ()
explodeShip = do

    state <- get

    let GameState playerState particles polygonParticles bullets asteroids time prevTime score lives level rng = state

    let (Vector2d a b) = ps_direction playerState
    let vel = ps_velocity playerState

    let (lineVertices, triangles) = playerModel
    let (dir, pos) = ((ps_direction playerState), (ps_position playerState))

    let mat = Matrix2d b a (-a) b
    let tris = map (\v -> (map ((pos ^+^) . ((#*^) mat)) v)) triangles

    addExplosion time pos (Vector2d 0.0 0.0) vel 1450 1.0 5.0

    forM_ tris $ \p -> do
        explodePolygon p pos vel time


explodeAsteroid :: Asteroid -> State GameState ()
explodeAsteroid asteroid = do

    state <- get

    let time = gs_time state
    let vertices = a_vertices asteroid

    let pos = polyCentroid . a_vertices $ asteroid
    let vel = a_velocity asteroid

    addExplosion time pos (Vector2d 0.0 0.0) vel (round ((polyArea . a_vertices $ asteroid)*100000)) 1.0 5.0

    explodePolygon (a_vertices asteroid) pos vel (gs_time state)


annihilateAsteroids :: State GameState ()
annihilateAsteroids = do
    asteroids <- liftM gs_asteroids get

    remainder <- filterM (\a -> do
        let area = polyArea (a_vertices a)
        let lives = area > 0.002
        when (not lives) $ do
            state <- get
            put $ state { gs_score = (gs_score state) + area*10000.0 }
            explodeAsteroid a
        return lives) asteroids

    state <- get
    put $ onAsteroids (\a -> remainder) state


updateAsteroids :: Float -> Float -> [Asteroid] -> [Asteroid]
updateAsteroids time delta =
    map (\asteroid -> 
    let 
        movedVertices = moveVertices (a_velocity asteroid) delta rotatedVertices
        rotatedVertices = rotateVertices asteroid delta
    in asteroid { a_vertices = movedVertices })


detectCollision :: Vertices -> Vertices -> Direction -> Bool
detectCollision ps vs dir = or [intersects (p, p ^+^ dir) (v1, v2) | p <- ps, (v1, v2) <- zip vs ((tail . cycle) vs)]


detectCollisions :: State GameState (Bool)
detectCollisions = do
    state <- get
    let GameState (playerState@(PlayerState pos dir vel angVel thrusters lastBullet _)) particles polygonParticles bullets asteroids time prevTime _ _ _ rng = state

    let (verts, tris) = playerModel

    let p1s = map ((^+^ pos) . (rebase dir)) verts
    let tri1s = [map ((^+^ pos) . (rebase dir)) tri | tri <- tris]

    let a = or (map (\(Asteroid adir avel avert) -> detectCollision p1s avert (vel^*!(time - prevTime))) asteroids)
    let b = or (map (\(Asteroid adir avel avert) -> detectCollision avert p1s (vel^*!(prevTime - time))) asteroids)
    let c = or (map (\(Asteroid adir avel avert) -> inside p1s avert) asteroids)
    let d = or [or (map (\(Asteroid adir avel avert) -> inside avert t) asteroids) | t <- tri1s]

    return (a || b || c || d)


runFrame :: [GLFW.Key] -> State ProgramState ()
runFrame input = do

    let keyCommands = [(GLFW.Key'E, Accelerating),
                       (GLFW.Key'S, TurningLeft),
                       (GLFW.Key'D, Decelerating),
                       (GLFW.Key'F, TurningRight),
                       (GLFW.Key'Space, Shooting),
                       (GLFW.Key'Escape, Escaping),
                       (GLFW.Key'Enter, Entering)]

    let actions = map snd $ filter ((`elem` input) . fst) keyCommands

    state <- get

    let newState = state {
        gls_gameState = execState (runGameFrame actions) (gls_gameState state)
    }

    let prevPressed = gls_keysPressed state
    let newDown = filter (\k -> not (k `elem` prevPressed)) input

    put $ newState { gls_mode = if GLFW.Key'Escape `elem` newDown then Menu else Playing }

    state <- get
    
    case (ps_aliveState . gs_playerState . gls_gameState) state of
        GameOver _ -> do
            when (GLFW.Key'Enter `elem` newDown) $ do
                put $ state { gls_mode = Restarting }
        _ -> return ()
    

updatePlayer :: Float -> [Action] -> State GameState ()
updatePlayer delta actions = do

    state <- get

    let GameState (playerState@(PlayerState pos dir vel angVel thrusters lastBullet aliveState)) particles polygonParticles bullets asteroids time prevTime score lives level rng = state

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


advanceLevel :: State GameState ()
advanceLevel = do
    state <- get
    put $ (initState (1 + (gs_level state)) 0.0 (gs_rng state)) { gs_score = gs_score state }


runGameFrame :: [Action] -> State GameState ()
runGameFrame actions = do

    state <- get

    let GameState (playerState@(PlayerState pos dir vel angVel thrusters lastBullet aliveState)) particles polygonParticles bullets asteroids time prevTime score lives level rng = state
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

    put $ state {
        gs_time = time,
        gs_particles = updateParticles time delta (gs_particles state),
        gs_bullets = updateBullets time delta (gs_bullets state),
        gs_asteroids = updateAsteroids time delta (gs_asteroids state),
        gs_polygonParticles = updatePolygonParticles time delta (gs_polygonParticles state)
    }

    annihilateAsteroids

    normalizePositions

    when ((null asteroids)) $ do
        case (ps_aliveState playerState) of
            Alive -> do
                state <- get
                put $ onPlayerState (\s -> s { ps_aliveState = Winning time }) state
            _ -> return ()
        return ()
