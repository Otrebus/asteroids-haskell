module Math where

import Graphics.Rendering.OpenGL

data Vector2d = Vector2d Float Float

data Matrix2d = Matrix2d Float Float Float Float

-- The logic behind the operator syntax is that # is a matrix and ^ is a vector, so #*^ is the operator that
-- multiplies a vector with a matrix

infixl 4 ^+^
(^+^) :: Vector2d -> Vector2d -> Vector2d
(^+^) (Vector2d a b) (Vector2d c d) = Vector2d (a + c) (b + d)

infixl 5 #*^
(#*^) :: Matrix2d -> Vector2d -> Vector2d
(#*^) (Matrix2d a b c d) (Vector2d x y) = Vector2d (x*a + y*b) (x*c + y*d)

infixl 5 #*#
(#*#) :: Matrix2d -> Matrix2d -> Matrix2d
(#*#) (Matrix2d a b c d) (Matrix2d e f g h) = Matrix2d (a*e + b*g) (f*a + h*b) (c*e + d*g) (c*f + d*h)

infixl 6 !*^
(!*^) :: Float -> Vector2d -> Vector2d
(!*^) x (Vector2d a b) = Vector2d (x*a) (x*b)

toVertex :: Vector2d -> Vertex2 Float
toVertex (Vector2d a b) = Vertex2 (a :: GLfloat) (b :: GLfloat)

type Vertices = [Vector2d]

type Position = Vector2d

type Direction = Vector2d

type Velocity = Vector2d