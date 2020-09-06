module Game.Effects where

import Utils.Math
import State
import Control.Monad.State (State, put, execState, get, when, filterM, liftM, forM_, replicateM)
import Player
import Data.Ord (comparing)
import Data.Foldable (maximumBy)
import qualified Graphics.UI.GLFW as GLFW
import Data.Maybe
import Data.List hiding (intersect)


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


addExplosionParticle :: Time -> Vector2d -> Vector2d -> Vector2d -> Float -> Float -> State GameState (Particle)
addExplosionParticle time pos dir vel pw life = do

    v <- liftM (**1.1) (rndFloat 0 pw)
    ang <- liftM (**1.1) (rndFloat 0 2.3)
    x <- rndFloat (-1) 1
    ang2 <- rndFloat 0 (2*pi)

    let anyDir = (rotate ang2 (Vector2d 1.0 0.0))
    let narrowDir = (normalize (rotate (x*ang) dir))
    let pvel = if (dir /= (Vector2d 0.0 0.0)) then narrowDir^*!v*v else anyDir^*!v
    let ppos = pos

    return (Particle ppos (pvel ^+^ vel) time (time + life) 0.8)


addExplosion :: Time -> Vector2d -> Vector2d -> Vector2d -> Int -> Float -> Float -> State GameState ()
addExplosion time pos dir vel n pw life = do

    particles <- replicateM n (addExplosionParticle time pos dir vel pw life)

    state <- get
    put (state { gs_particles = particles ++ (gs_particles state) } )


addEngineParticle :: Float -> Thruster -> Float -> Position -> Direction -> Velocity -> State GameState ()
addEngineParticle time thruster speed playerPos playerDir playerVel = do
    let newPos = rebase playerDir (t_position thruster)
    let newDir = rebase playerDir (t_direction thruster)
    r <- rndFloat 0.0 0.2
    q <- rndFloat 0.0 0.2

    x <- rndFloat (-1) 1
    y <- liftM (**1.1) (rndFloat 0 0.3)

    s <- rndFloat 0.0 0.15
    let newnDir = rotate (x*y) newDir
    state <- get

    let pos = (playerPos ^+^ newPos)
    let vel = (playerVel ^+^ ((newnDir)^*!(speed + s)))
    let lifeTime = (time + 2.5 + 0.2)
    let brightness = (0.5 + q)

    put $ onParticles ((:) (Particle pos vel time lifeTime brightness)) state


addEngineParticles :: (GameState -> Thruster) -> (Lifter Thruster Thrusters) -> Time -> Float -> State GameState ()
addEngineParticles thrusterGetter fp start current = do

    state <- get

    let time = gs_time state
    let (PlayerState pos dir vel angVel _ _ _) = gs_playerState state
    let thruster = thrusterGetter state

    let nextParticleTime = current + (t_emissionInterval thruster)
    when (nextParticleTime < time) $ do

        let deltaAngle = angVel * (nextParticleTime - time)
        let turnMatrix = getTurnMatrix deltaAngle

        let newDir = turnMatrix #*^ dir

        addEngineParticle nextParticleTime thruster 0.6 (pos ^+^ (vel ^*! (nextParticleTime-start))) newDir vel
        state <- get
        put $ ((onPlayerState . onThrusters . fp) (\t -> t { t_lastEmitted = nextParticleTime } )) state
        addEngineParticles thrusterGetter fp start nextParticleTime


addEnginesParticles :: [Action] -> State GameState ()
addEnginesParticles actions = do
    
    state <- get
    let (time, prevTime) = (gs_time state, gs_prevTime state)

    let thfun = (ps_thrusters . gs_playerState)
    let engines = [(Accelerating, e_main . thfun, onMainThruster),
                   (TurningLeft, e_topright . thfun, onTopRightThruster),
                   (TurningRight, e_topleft . thfun, onTopLeftThruster),
                   (TurningLeft, e_bottomleft . thfun, onBottomLeftThruster),
                   (TurningRight, e_bottomright . thfun, onBottomRightThruster)]

    forM_ engines $ \(ac, th, fp) -> do
        let t0 = t_lastEmitted (th state); int = t_emissionInterval (th state)
        let t = t0 + int * realToFrac (floor ((prevTime - t0)/int))
        when (ac `elem` actions) $ addEngineParticles th fp t t


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

    let (playerState, time) = (gs_playerState state, gs_time state)

    let (Vector2d a b) = ps_direction playerState
    let vel = ps_velocity playerState

    let (lineVertices, triangles) = playerModel
    let (dir, pos) = ((ps_direction playerState), (ps_position playerState))

    let mat = Matrix2d b a (-a) b
    let tris = map (\v -> (map ((pos ^+^) . ((#*^) mat)) v)) triangles

    addExplosion time pos (Vector2d 0.0 0.0) vel 1450 1.0 5.0

    forM_ tris $ \p -> do
        explodePolygon p pos vel time