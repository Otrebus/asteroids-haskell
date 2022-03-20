module Utils.Math where

import Graphics.Rendering.OpenGL hiding (Angle)
import Control.Monad.Random
import Data.List (sort, sortOn)
import Control.Applicative
import Data.Fixed (mod')


data Vector2d = Vector2d { xComp :: Float, yComp :: Float } deriving (Show, Eq)

data Matrix2d = Matrix2d Float Float Float Float deriving Show

-- The logic behind the operator syntax is that # is a matrix, ^ is a vector and ! is a scalar,
-- so for example #*^ is the operator that multiplies a matrix with a vector

infixl 4 ^+^ -- Vector/vector addition
(^+^) :: Vector2d -> Vector2d -> Vector2d
(^+^) (Vector2d a b) (Vector2d c d) = Vector2d (a + c) (b + d)

infixl 4 ^-^ -- Vector/vector subtraction
(^-^) :: Vector2d -> Vector2d -> Vector2d
(^-^) (Vector2d a b) (Vector2d c d) = Vector2d (a - c) (b - d)

infixl 5 #*^ -- Matrix/vector multiplication
(#*^) :: Matrix2d -> Vector2d -> Vector2d
(#*^) (Matrix2d a b c d) (Vector2d x y) = Vector2d (x*a + y*b) (x*c + y*d)

infixl 5 #*# -- Matrix/matrix multiplication
(#*#) :: Matrix2d -> Matrix2d -> Matrix2d
(#*#) (Matrix2d a b c d) (Matrix2d e f g h) = Matrix2d(a*e + b*g) (f*a + h*b) (c*e + d*g) (c*f + d*h)

infixl 6 !*^ -- Vector/scalar multiplication (scaling)
(!*^) :: Float -> Vector2d -> Vector2d
(!*^) x (Vector2d a b) = Vector2d (x*a) (x*b)

infixl 6 ^*! -- Vector/scalar multiplication (scaling)
(^*!) :: Vector2d -> Float -> Vector2d
(^*!) (Vector2d a b) x = Vector2d (x*a) (x*b)

infixl 6 ^/! -- Vector/scalar division (scaling)
(^/!) :: Vector2d -> Float -> Vector2d
(^/!) (Vector2d x y) t = Vector2d (x/t) (y/t)

infixl 6 ^%^ -- "Cross product"
(^%^) :: Vector2d -> Vector2d -> Float
(^%^) (Vector2d a b) (Vector2d c d) = a*d - b*c

ortho :: Vector2d -> Vector2d -- Returns a vector orthogonal to the given
ortho (Vector2d x y) = Vector2d y (-x)

len :: Vector2d -> Float -- The length of a vector
len (Vector2d x y) = sqrt (x*x + y*y)

normalize :: Vector2d -> Vector2d -- Vector normalization
normalize v = v ^/! (len v)

toVertex :: Vector2d -> Vertex2 Float -- Turns a vector into an OpenGL vertex
toVertex (Vector2d a b) = Vertex2 (a :: GLfloat) (b :: GLfloat)

fromPair :: (Float, Float) -> Vector2d -- Turns a vector into a pair of floats
fromPair = uncurry Vector2d

type Vertices = [Vector2d]

type Edge = (Vector2d, Vector2d)

type Position = Vector2d

type Direction = Vector2d

type Velocity = Vector2d

type Time = Float

type Angle = Float

idMatrix :: Matrix2d
idMatrix = Matrix2d 1.0 0.0 0.0 1.0


-- The turn matrix which returns a vector a certain angle
getTurnMatrix ::
    Angle -> -- The angle to rotate
    Matrix2d -- The turn matrix
getTurnMatrix theta = Matrix2d (cos theta) (-sin(theta)) (sin theta) (cos theta)


-- R the top left and bottom right corners of the bounding box of a set of vectors
bbox :: [Vector2d] -> (Float, Float, Float, Float)
bbox vs = (minimum . (map xComp) $ vs, minimum . (map yComp) $ vs,
           maximum . (map xComp) $ vs, maximum . (map yComp) $ vs)


-- Creates a random convex polygon using Valtr's algorithm
randomPolygon ::
    Int ->      -- The number of vertices of the polygon
    Vector2d -> -- The position of the polygon
    Float ->    -- The width of the polygon
    Float ->    -- The height of the polygon
    Rand StdGen [Vector2d]
randomPolygon n pos width height = do

    (dx, mdx) <- vectorize (n)
    (dy, mdy) <- vectorize (n)

    rs <- getRandomRs(0.0, 1.0 :: Float)
    let rndY = map snd $ sortOn fst (zip rs (dy++mdy))

    let vs = zipWith Vector2d (dx++mdx) rndY 
    let vss = sortOn (liftA2 atan2 yComp xComp) vs

    let t = scanl1 (^+^) vss

    let (minX, minY, maxX, maxY) = bbox t

    let tranV = map (^-^ (Vector2d minX minY)) t
    let scalV = map (\(Vector2d x y) -> Vector2d (width*x/(maxX-minX)) (height*y/(maxY-minY))) tranV

    return $ map (^+^ pos) (scalV)

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

-- Expresses a vector using another vector as a coordinate basis
rebase ::
    Vector2d -> -- The coordinate basis
    Vector2d -> -- The vector
    Vector2d    -- The result
rebase (Vector2d a b) (Vector2d x y) = y!*^(Vector2d a b) ^+^ x!*^(Vector2d b (-a))


-- Rotates a vector a certain number of radians
rotate ::
    Angle ->    -- The amount to rotate
    Vector2d -> -- The vector to ritate
    Vector2d    -- The result
rotate theta v = (Matrix2d(cos theta) (-sin(theta)) (sin theta) (cos theta))#*^v


-- Returns the angle of the vector counterclockwise from the x axis
angle ::
    Vector2d -> -- The vector
    Float
angle (Vector2d x y) = atan2 y x


-- Rotates a given vector a certain angle around another vector
rotateAround ::
    Angle ->    -- The angle
    Vector2d -> -- The vector to rotate
    Vector2d ->
    Vector2d
rotateAround theta o v = getTurnMatrix theta#*^(v ^-^ o) ^+^ o


-- Wraps a position around to [-1.0, 1.0]
wrap ::
    Vector2d -> -- The position
    Vector2d    -- The wrapped position
wrap (Vector2d x y) = Vector2d (mod' (x + 1.0) 2.0 - 1.0) (mod' (y + 1.0) 2.0 - 1.0)


-- Wraps an set of associated vertices
wrapVertices ::
    Vertices -> -- The input vertices
    Vertices    -- The output vertices
wrapVertices vs = map (^+^ (wrap (polyCentroid vs))) cfVs
    where cfVs = map (^-^ polyCentroid vs) vs


-- Returns the edges of a polygon defined by its vertices
polyEdges ::
    [Vector2d] ->           -- The vertices of the polygon
    [(Vector2d, Vector2d)]  -- The edges of the polygon
polyEdges = zip <*> tail . cycle


-- Returns the centroid of a polygon
polyCentroid ::
    Vertices -> -- The vertices of the polygon
    Vector2d    -- The centroid of the polygon
polyCentroid vs = (Vector2d xs ys)^/!(6*area)
    where
        area = polyArea vs
        xs = sum [ (xi+xi1)*(xi*yi1 - xi1*yi) | (Vector2d xi yi, Vector2d xi1 yi1) <- polyEdges vs]
        ys = sum [ (yi+yi1)*(xi*yi1 - xi1*yi) | (Vector2d xi yi, Vector2d xi1 yi1) <- polyEdges vs]


-- Returns the area of a polygon
polyArea ::
    Vertices -> -- The polygon vertices
    Float       -- The area of the polygon
polyArea vs = 0.5*(sum [(xComp v)*(yComp w) - (yComp v)*(xComp w) | (v, w) <- polyEdges vs])


-- Returns the circumference of a polygon
polyCirc ::
    Vertices -> -- The vertices of the polygon
    Float       -- The circumference of the polygon
polyCirc vs = sum [len (v ^-^ w) | (v, w) <- polyEdges vs]


-- Returns a ratio that estimates how circular a polygon is
calcRatio ::
    Vertices -> -- The vertices of the polygon
    Float       -- The ratio
calcRatio [] = 0
calcRatio vs = let area = polyArea vs; circ = polyCirc vs in (area*4*pi)/(circ*circ)


-- Intersects two lines
intersect ::
    (Vector2d, Vector2d) -> -- The first line
    (Vector2d, Vector2d) -> -- The second line
    (Float, Float)          -- The parameters of the intersections
intersect (Vector2d p1x p1y, Vector2d p2x p2y) (Vector2d v1x v1y, Vector2d v2x v2y) =
    (s, t)
    where
        a = p1x - p2x; b = v2x - v1x; c = p1y - p2y; d = v2y - v1y; e = p1x - v1x; f = p1y - v1y
        s = (e*d - b*f)/(a*d - b*c); t = (a*f - e*c)/(a*d - b*c)


-- Interpolates between two vector positions
interpolate ::
    Vector2d -> -- The first vector
    Vector2d -> -- The second vector
    Float ->    -- The interpolation parameter
    Vector2d    -- The output
interpolate vec1 vec2 t = vec1 ^+^ ((vec2 ^-^ vec1) ^*! t)


-- Checks whether two line segments intersect
intersects ::
    (Vector2d, Vector2d) -> -- The first line segment
    (Vector2d, Vector2d) -> -- The second line segment
    Bool                    -- Whether they intersect
intersects a b = let (s, t) = intersect a b in t >= 0 && t <= 1 && s >= 0.0 && s <= 1


-- Checks whether one polygon is inside another
inside ::
    Vertices -> -- The first polygon
    Vertices -> -- The second
    Bool        -- Whether one is inside the other
inside as bs = or [and [(c ^-^ b) ^%^ (a ^-^ b) > 0 | (b, c) <- polyEdges bs] | a <- as]
