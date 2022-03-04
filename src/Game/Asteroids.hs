module Game.Asteroids where
import Utils.Math
import State
import Control.Monad.State (State, put, get, when, filterM, liftM)
import Data.Ord (comparing)
import Data.List hiding (intersect)
import Game.Effects


rotateVertices :: Asteroid -> Float -> Vertices
rotateVertices asteroid delta = map (\v -> ( (getTurnMatrix (delta*angVel))#*^(v ^-^ c) ^+^ c)) vs
    where
        c = polyCentroid vs
        angVel = a_angularVelocity asteroid
        vs = a_vertices asteroid


loopSplits :: Edge -> Float -> Vertices -> Edge -> Vertices -> (Vertices, Vertices)
loopSplits (p1, p2) t (xs) (v1, v2) [] = ([], [])
loopSplits (p1, p2) t (xs) (v1, v2) (y:ys)
    | otherwise = maximumBy (comparing (\(a, b) -> (calcRatio a)*(calcRatio b))) (ds:cs)
    where
        cs = [splitAt u | u <- [0.1, 0.2..1]]
        ds = loopSplits (p1, p2) t (xs ++ [v2]) (v2, head ys) (ys)
        splitAt s =
            ((interpolate p1 p2 t):xs ++ ([interpolate v1 v2 s]),
             (interpolate v1 v2 s):y:(ys ++ [interpolate p1 p2 t]))


splitAsteroid :: Asteroid -> Edge -> Float -> (Asteroid, Asteroid)
splitAsteroid asteroid (p1, p2) t =
    (Asteroid (angVel) ((a_velocity asteroid) ^+^ Vector2d (-day) dax ^+^ (normalize a)^*!0.05) as,
     Asteroid (angVel) ((a_velocity asteroid) ^+^ Vector2d (-dby) dbx ^+^ (normalize b)^*!0.05) bs)
    where
        Vector2d dax day = a^*!(angVel)
        Vector2d dbx dby = b^*!(angVel)
        a = (polyCentroid as) ^-^ (polyCentroid (a_vertices asteroid))
        b = (polyCentroid bs) ^-^ (polyCentroid (a_vertices asteroid))
        (as, bs) = loopSplits (p1, p2) t [p2] (v1, v2) (v2:takeWhile (/= p2) rest)
        (v1:v2:rest):_ = (dropWhile ((/= p2) . head)) $ maprest (cycle vs)
        vs = a_vertices asteroid
        maprest (x:xs) = (x:xs) : maprest xs
        angVel = a_angularVelocity asteroid


explodeNewAsteroids :: Time -> [Asteroid] -> State GameState ()
explodeNewAsteroids time [] = return ()
explodeNewAsteroids time (x:y:zs) = do
    let vs = a_vertices x
    addExplosion time (head vs) (normalize ((head vs) ^-^ (last vs))) (a_velocity x) 100 1.5 1.5
    addExplosion time (last vs) (normalize ((last vs) ^-^ (head vs))) (a_velocity y) 15 1 1.5

    explodeNewAsteroids time zs


explodeAsteroid :: Asteroid -> State GameState ()
explodeAsteroid asteroid = do
    state <- get

    let time = gs_time state
    let vertices = a_vertices asteroid

    let pos = polyCentroid . a_vertices $ asteroid
    let vel = a_velocity asteroid

    let nParticles = round ((polyArea . a_vertices $ asteroid)*100000)
    addExplosion time pos (Vector2d 0.0 0.0) vel nParticles 1.0 5.0

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
