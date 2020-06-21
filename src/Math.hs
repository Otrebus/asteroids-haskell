module Math where

import Graphics.Rendering.OpenGL

data Vector2d = Vector2d Float Float

data Matrix2d = Matrix2d Float Float Float Float

infixl 4 ^+
(^+) :: Vector2d -> Vector2d -> Vector2d
(^+) (Vector2d a b) (Vector2d c d) = Vector2d (a + c) (b + d)

infixl 5 ^*
(^*) :: Matrix2d -> Vector2d -> Vector2d
(^*) (Matrix2d a b c d) (Vector2d x y) = Vector2d (x*a + y*b) (x*c + y*d)

toVertex :: Vector2d -> Vertex2 Float
toVertex (Vector2d a b) = Vertex2 (a :: GLfloat) (b :: GLfloat)