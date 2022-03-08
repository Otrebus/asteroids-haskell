module Game.Asteroids where
import Utils.Math
import State
import Control.Monad.State (State, put, get, when, filterM, liftM)
import Data.Ord (comparing)
import Data.List hiding (intersect)
import Game.Effects


minSize = 0.002 :: Float


type SplitFunction = [(SplitPolygon, SplitPolygon)] -> (SplitPolygon, SplitPolygon)


-- Rotates an asteroid over an interval of time and returns the result.
--
--   asteroid: The asteroid to rotate
--   delta:    The amount of time that passed since the last rotation
rotateVertices :: Asteroid -> Time -> Vertices
rotateVertices asteroid delta = map (\v -> ( (getTurnMatrix (delta*angVel))#*^(v ^-^ c) ^+^ c)) vs
    where
        c = polyCentroid vs
        angVel = a_angularVelocity asteroid
        vs = a_vertices asteroid


split ::
-- Determines where best to split an asteroid given an impact, and returns the vertex lists
-- representing the resulting asteroid halves. 
    SplitFunction -> -- A function that selects the best split out of all pairs of split polygons
    Edge ->          -- The near splitting edge (the point of the bullet impact)
    Float ->         -- The parameter point of the near splitting edge of where the bullet impacted
    Vertices ->      -- The interior of the left asteroid
    Edge ->          -- The second splitting edge
    Vertices ->      -- The interior of the right asteroid
    (SplitPolygon, SplitPolygon)

split splitFun (p1, p2) t (xs) (v1, v2) [] = (([], undefined), ([], undefined))
split splitFun (p1, p2) t (xs) (v1, v2) (y:ys) =
    splitFun (ds:cs)
    where
        cs = [splitAt u | u <- [0.01, 0.02..1]]
        ds = split splitFun (p1, p2) t (xs ++ [v2]) (v2, head ys) (ys)
        splitAt s =
            (((interpolate p1 p2 t):xs ++ [interpolate v1 v2 s], (interpolate v1 v2 s, interpolate p1 p2 t)),
             ((interpolate v1 v2 s):y:(ys ++ [interpolate p1 p2 t]), (interpolate p1 p2 t, interpolate v1 v2 s)))


splitAsteroid :: SplitFunction -> Int -> Asteroid -> Edge -> Float -> [Asteroid]
splitAsteroid splitFun depth asteroid (p1, p2) t
    | depth == 0 = [asteroid { a_splitEdge = Nothing }]
    | depth == 1 = [a1, a2]
    | depth <= 2 = (splitAsteroid splitFun (depth-1) a1 r1 0.5) ++ (splitAsteroid splitFun (depth-1) a2 r2 0.5)
        where
            a1 = Asteroid (angVel) ((a_velocity asteroid) ^+^ Vector2d (-day) dax ^+^ (normalize a)^*!0.05) as (Just r1)
            a2 = Asteroid (angVel) ((a_velocity asteroid) ^+^ Vector2d (-dby) dbx ^+^ (normalize b)^*!0.05) bs (Just r2)
            Vector2d dax day = a^*!(angVel)
            Vector2d dbx dby = b^*!(angVel)
            a = (polyCentroid as) ^-^ (polyCentroid (a_vertices asteroid))
            b = (polyCentroid bs) ^-^ (polyCentroid (a_vertices asteroid))
            ((as, r1), (bs, r2)) = split splitFun (p1, p2) t [p2] (p2, p3) (p3:takeWhile (/= p2) rest)
            (_:p3:rest):_ = (dropWhile ((/= p2) . head)) $ maprest (cycle vs)
            vs = a_vertices asteroid
            maprest (x:xs) = (x:xs) : maprest xs
            angVel = a_angularVelocity asteroid


-- Adds explosion particles to the the given asteroids, along the line of the edge formed by the first
-- and last vertices of each asteroid
--
--  (x:y:zs): The list of asteroids
explodeNewAsteroids  :: [Asteroid] -> State GameState ()
explodeNewAsteroids [] = return ()
explodeNewAsteroids (x@(Asteroid _ _ _ (Just (v1, v2))):xs) = do
    state <- get
    let time = gs_time state

    let vs = a_vertices x
    addExplosion time v1 (normalize (v1 ^-^ v2)) (a_velocity x) 100 1.5 1.5
    addExplosion time v2 (normalize (v2 ^-^ v1)) (a_velocity x) 15 1 1.5

    explodeNewAsteroids xs


chipAsts :: [Asteroid] -> State GameState([Asteroid])
chipAsts [] = return ([])
chipAsts (ast@(Asteroid _ _ _ Nothing):asts) = do
    xs <- chipAsts asts
    return $ (ast { a_splitEdge = Nothing }) : xs
chipAsts (ast@(Asteroid _ _ _ (Just edge)):asts) = do
    state <- get

    r <- rndFloat 0.0 1
    u <- rndFloat 0.2 0.5
    xs <- chipAsts asts
    
    let splitChip = minimumBy (comparing (\((a, _), (b, _)) -> abs (10 - (polyArea a)/(polyArea b))))
    let chip = splitAsteroid splitChip 1 ast edge u
    let allBigEnough = all (\a -> (polyArea . a_vertices) a > minSize) chip

    if allBigEnough then return (chip ++ xs) else return $ ast { a_splitEdge = Just edge }:xs


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
        let lives = area > minSize
        when (not lives) $ do
            state <- get
            put $ state { gs_score = (gs_score state) + area*10000.0 }
            explodeAsteroid a
        return lives) asteroids

    state <- get

    put $ onAsteroids (\a -> remainder) state


updateAsteroids :: Time -> Time -> [Asteroid] -> [Asteroid]
updateAsteroids time delta =
    map (\asteroid -> 
    let 
        movedVertices = moveVertices (a_velocity asteroid) delta rotatedVertices
        rotatedVertices = rotateVertices asteroid delta
    in asteroid { a_vertices = movedVertices })
