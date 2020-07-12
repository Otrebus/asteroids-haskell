module Math where

import Graphics.Rendering.OpenGL

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
