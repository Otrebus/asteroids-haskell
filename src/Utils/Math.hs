module Math where

import Graphics.Rendering.OpenGL hiding (Angle)
import Control.Monad.Random
import Data.List (sort, sortOn)
import Debug.Trace
import Control.Applicative
import Data.Fixed (mod')


data Vector2d = Vector2d { xComp :: Float, yComp :: Float } deriving (Show, Eq)

data Matrix2d = Matrix2d Float Float Float Float deriving Show

-- The logic behind the operator syntax is that # is a matrix, ^ is a vector and ! is a scalar,
-- so for example #*^ is the operator that multiplies a matrix with a vector

infixl 4 ^+^
(^+^) :: Vector2d -> Vector2d -> Vector2d
(^+^) (Vector2d a b) (Vector2d c d) = Vector2d (a + c) (b + d)

infixl 4 ^-^
(^-^) :: Vector2d -> Vector2d -> Vector2d
(^-^) (Vector2d a b) (Vector2d c d) = Vector2d (a - c) (b - d)

infixl 5 #*^
(#*^) :: Matrix2d -> Vector2d -> Vector2d
(#*^) (Matrix2d a b c d) (Vector2d x y) = Vector2d (x*a + y*b) (x*c + y*d)

infixl 5 #*#
(#*#) :: Matrix2d -> Matrix2d -> Matrix2d
(#*#) (Matrix2d a b c d) (Matrix2d e f g h) = Matrix2d (a*e + b*g) (f*a + h*b) (c*e + d*g) (c*f + d*h)

infixl 6 !*^
(!*^) :: Float -> Vector2d -> Vector2d
(!*^) x (Vector2d a b) = Vector2d (x*a) (x*b)

infixl 6 ^*!
(^*!) :: Vector2d -> Float -> Vector2d
(^*!) (Vector2d a b) x = Vector2d (x*a) (x*b)

infixl 6 ^/!
(^/!) :: Vector2d -> Float -> Vector2d
(^/!) (Vector2d x y) t = Vector2d (x/t) (y/t)

infixl 6 ^%^ -- "Cross product"
(^%^) :: Vector2d -> Vector2d -> Float
(^%^) (Vector2d a b) (Vector2d c d) = a*d - b*c

len :: Vector2d -> Float
len (Vector2d x y) = sqrt (x*x + y*y)

normalize :: Vector2d -> Vector2d
normalize v = v ^/! (len v)

toVertex :: Vector2d -> Vertex2 Float
toVertex (Vector2d a b) = Vertex2 (a :: GLfloat) (b :: GLfloat)

fromPair :: (Float, Float) -> Vector2d
fromPair = uncurry Vector2d

type Vertices = [Vector2d]

type Position = Vector2d

type Direction = Vector2d

type Velocity = Vector2d

type Time = Float

type Angle = Float

idMatrix :: Matrix2d
idMatrix = Matrix2d 1.0 0.0 0.0 1.0

getTurnMatrix :: Angle -> Matrix2d
getTurnMatrix theta = Matrix2d (cos theta) (-sin(theta)) (sin theta) (cos theta)

bbox vs = (minimum . (map xComp) $ vs, minimum . (map yComp) $ vs,
           maximum . (map xComp) $ vs, maximum . (map yComp) $ vs)


randomPolygon :: Int -> Vector2d -> Float -> Float -> Rand StdGen [Vector2d]
randomPolygon n pos width height = do

    (dx, mdx) <- vectorize (n)
    (dy, mdy) <- vectorize (n)

    rs <- getRandomRs(0.0, 1.0 :: Float)
    let rndY = map snd $ sortOn fst (zip rs (dy++mdy))

    let vs = zipWith Vector2d (dx++mdx) rndY 
    let vss = sortOn (liftA2 atan2 yComp xComp) vs

    let t = scanl1 (^+^) vss

    let (minX, minY, maxX, maxY) = bbox t

    let transV = map (^-^ (Vector2d minX minY)) t
    let scaledV = map (\(Vector2d x y) -> Vector2d (width*x/(maxX-minX)) (height*y/(maxY-minY))) transV

    return $ map (^+^ pos) (scaledV)

    where 
        vectorize n = do
            xs <- (sort . (take n)) <$> getRandomRs (0.0, 1.0)
            (x1, x2) <- rndSplit xs
            let (minX, maxX) = (minimum xs, maximum xs)
            let dx = (filter (/= 0.0)) (deltas $ minX:x1 ++ [maxX])
            let mdx = (filter (/= 0.0)) ((deltas . reverse) $ minX:x2 ++ [maxX])
            return (dx, mdx)

        deltas = zipWith (-) <*> tail

        rndSplit [] = return ([], [])
        rndSplit (x:xs) = do
            (as, bs) <- rndSplit xs
            c <- getRandom
            return (if c then (x:as, bs) else (as, x:bs))


rebase :: Vector2d -> Vector2d -> Vector2d
rebase (Vector2d a b) (Vector2d x y) = y!*^(Vector2d a b) ^+^ x!*^(Vector2d b (-a))


rotate :: Float -> Vector2d -> Vector2d
rotate theta v = (Matrix2d(cos theta) (-sin(theta)) (sin theta) (cos theta))#*^v


angle :: Vector2d -> Float
angle (Vector2d x y) = atan2 y x


rotateAround :: Float -> Vector2d -> Vector2d -> Vector2d
rotateAround theta o v = (Matrix2d(cos theta) (-sin(theta)) (sin theta) (cos theta))#*^(v ^-^ o) ^+^ o


moveVertices vel delta = map (\v -> v ^+^ (delta)!*^vel)


wrap :: Vector2d -> Vector2d
wrap (Vector2d x y) = Vector2d (mod' (x + 1.0) 2.0 - 1.0) (mod' (y + 1.0) 2.0 - 1.0)


wrapVertices :: Vertices -> Vertices
wrapVertices vs = map (^+^ (wrap (polyCentroid vs))) cfVs
    where cfVs = map (^-^ polyCentroid vs) vs


polyCentroid :: Vertices -> Vector2d
polyCentroid vs = (Vector2d xs ys)^/!(6*area)
    where
        area = polyArea vs
        xs = sum [ (xi+xi1)*(xi*yi1 - xi1*yi) | ((Vector2d xi yi), (Vector2d xi1 yi1)) <- zip vs ((tail . cycle) vs)]
        ys = sum [ (yi+yi1)*(xi*yi1 - xi1*yi) | ((Vector2d xi yi), (Vector2d xi1 yi1)) <- zip vs ((tail . cycle) vs)]


polyArea :: Vertices -> Float
polyArea vs = 0.5*(sum [(xComp v)*(yComp w) - (yComp v)*(xComp w) | (v, w) <- zip vs ((tail . cycle) vs)])


polyCirc :: Vertices -> Float
polyCirc vs = sum [len (v ^-^ w) | (v, w) <- zip vs ((tail . cycle) vs)]


calcRatio :: Vertices -> Float
calcRatio [] = 0
calcRatio vs = let area = polyArea vs; circ = polyCirc vs in (area*4*pi)/(circ*circ)


intersect :: (Vector2d, Vector2d) -> (Vector2d, Vector2d) -> (Float, Float)
intersect (Vector2d p1x p1y, Vector2d p2x p2y) (Vector2d v1x v1y, Vector2d v2x v2y) =
    (s, t)
    where
        a = p1x - p2x; b = v2x - v1x; c = p1y - p2y; d = v2y - v1y; e = p1x - v1x; f = p1y - v1y
        s = (e*d - b*f)/(a*d - b*c); t = (a*f - e*c)/(a*d - b*c)


interpolate :: Vector2d -> Vector2d -> Float -> Vector2d
interpolate vec1 vec2 t = vec1 ^+^ ((vec2 ^-^ vec1) ^*! t)


intersects a b = let (s, t) = intersect a b in t >= 0 && t <= 1 && s >= 0.0 && s <= 1


inside :: Vertices -> Vertices -> Bool
inside as bs = or [and [(c ^-^ b) ^%^ (a ^-^ b) > 0 | (b, c) <- zip bs ((tail . cycle) bs)] | a <- as]
