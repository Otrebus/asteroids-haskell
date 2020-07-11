module Player where
import Math
import GameState


bottomLeftCorner = Vector2d (-0.04) (-0.04)
topCorner = Vector2d 0 0.04
bottomRightCorner = Vector2d 0.04 (-0.04)


playerModel = [bottomLeftCorner, topCorner, bottomRightCorner]


interpolate :: Vector2d -> Vector2d -> Float -> Vector2d
interpolate vec1 vec2 t = vec1 ^+^ ((vec2 ^-^ vec1) ^*! t)


mainThruster = Thruster 0.0 0.001 (interpolate bottomLeftCorner bottomRightCorner 0.5) (Vector2d 0 (-1))
reverseThruster = Thruster 0.0 0.05 topCorner (Vector2d 0 1)
topLeftThruster = Thruster 0.0 0.01 (interpolate topCorner bottomLeftCorner 0.2) (Vector2d (-1) 0)
topRightThruster = Thruster 0.0 0.01 (interpolate topCorner bottomRightCorner 0.2) (Vector2d (1) 0)
bottomLeftThruster = Thruster 0.0 0.01 (interpolate topCorner bottomLeftCorner 0.8) (Vector2d (-1) 0)
bottomRightThruster = Thruster 0.0 0.01 (interpolate topCorner bottomRightCorner 0.8) (Vector2d (1) 0)


thrusters = Thrusters mainThruster reverseThruster topLeftThruster topRightThruster bottomLeftThruster bottomRightThruster


startPos = (Vector2d 0 0)
startDir = (Vector2d 0 1.0)
startVel = (Vector2d 0.0 0.0)