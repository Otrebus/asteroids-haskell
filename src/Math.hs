module Math where

import Graphics.Rendering.OpenGL
import Control.Monad.Random
import Data.List (sort, sortOn)

data Vector2d = Vector2d Float Float deriving Show

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

len :: Vector2d -> Float
len (Vector2d x y) = sqrt (x*x + y*y)

normalize :: Vector2d -> Vector2d
normalize v = v ^/! (len v)

toVertex :: Vector2d -> Vertex2 Float
toVertex (Vector2d a b) = Vertex2 (a :: GLfloat) (b :: GLfloat)

type Vertices = [Vector2d]

type Position = Vector2d

type Direction = Vector2d

type Velocity = Vector2d

type Time = Float

type Angle = Float

deltas :: Num a => [a] -> [a]
deltas (x:y:ys) = (y-x) : deltas (y:ys)
deltas _ = []

rndSplit :: [a] -> Rand StdGen ([a], [a])
rndSplit (x:xs) = do
    (as, bs) <- rndSplit(xs)
    c <- getRandomR(1, 2 :: Int)
    case c == 1 of
        True -> return (x:as, bs)
        False -> return (as, x:bs)

rndSplit _ = return ([], [])
    

randomPolygon :: Int -> Rand StdGen [Vector2d]
randomPolygon n = do
    
    xs <- liftM (sort . (take n)) $ getRandomRs (0.0 :: Float, 1.0)
    ys <- liftM (sort . (take n)) $ getRandomRs (0.0 :: Float, 1.0)

    splitX <- getRandomR(1, n-1)
    splitY <- getRandomR(1, n-1)

    (x1, x2) <- rndSplit xs
    (y1, y2) <- rndSplit ys

    let minX = minimum x1
    let maxX = maximum x2

    let minY = minimum y1
    let maxY = maximum y2

    let dx = deltas $ minX:x1 ++ [maxX]
    let mdx = (deltas . reverse) $ minX:x2 ++ [maxX]
    let dy = deltas $ minY:y1 ++ [maxY]
    let mdy = (deltas . reverse) $ minY:y2 ++ [maxY]

    rs <- getRandomRs(0 :: Float, 1.0)
    let rndY = map snd $ sortOn fst (zip rs (dy++mdy))

    let vs = zipWith (\a b -> Vector2d a b) (dx++mdx) rndY

    let vss = sortOn (\(Vector2d x y) -> atan2 y x) vs

    return $ scanl1 (^+^) vss
