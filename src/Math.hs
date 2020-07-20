module Math where

import Graphics.Rendering.OpenGL
import Control.Monad.Random
import Data.List (sort, sortOn)
import Debug.Trace
import Control.Applicative

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

type Vertices = [Vector2d]

type Position = Vector2d

type Direction = Vector2d

type Velocity = Vector2d

type Time = Float

type Angle = Float


bbox vs = (minimum . (map xComp) $ vs, minimum . (map yComp) $ vs, maximum . (map xComp) $ vs, maximum . (map yComp) $ vs)


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


-- The above should be equal to this but generates a polygon way too large, why?
-- rndSplit = foldM (\(as, bs) x -> (getRandom >>= (\r -> return (if r then (x:as, bs) else (as, x:bs))))) ([], [])
