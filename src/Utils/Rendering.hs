module Utils.Rendering where

import qualified Graphics.Rendering.OpenGL as GL

white = GL.Color4 1.0 1.0 1.0 (1.0 :: GL.GLfloat)
black = GL.Color4 0.0 0.0 0.0 (1.0 :: GL.GLfloat)
darkGray = GL.Color4 0.05 0.05 0.05 (1.0 :: GL.GLfloat)
gray c = GL.Color4 c c c (1.0 :: GL.GLfloat)
