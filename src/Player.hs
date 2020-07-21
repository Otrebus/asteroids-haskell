module Player where
import Math
import GameState

plBl = Vector2d (-0.04) (-0.04) -- Bottom left corner
plT = Vector2d 0 0.04           -- Top corner
plBr = Vector2d 0.04 (-0.04)    -- Bottom right corner
plB = Vector2d 0.00 (-0.02)    -- Bottom


playerModel = ([plBl, plB, plBr, plT], [[plBl, plB, plT], [plT, plB, plBr]])

interpolate :: Vector2d -> Vector2d -> Float -> Vector2d -- TODO: use more, and move
interpolate vec1 vec2 t = vec1 ^+^ ((vec2 ^-^ vec1) ^*! t)


mainThruster = Thruster 0.0 0.001 plB (Vector2d 0 (-1))
reverseThruster = Thruster 0.0 0.05 plT (Vector2d 0 1)
topLeftThruster = Thruster 0.0 0.01 (interpolate plT plBl 0.2) (Vector2d (-1) 0)
topRightThruster = Thruster 0.0 0.01 (interpolate plT plBr 0.2) (Vector2d (1) 0)
bottomLeftThruster = Thruster 0.0 0.01 (interpolate plT plBl 0.8) (Vector2d (-1) 0)
bottomRightThruster = Thruster 0.0 0.01 (interpolate plT plBr 0.8) (Vector2d (1) 0)


thrusters = Thrusters mainThruster reverseThruster topLeftThruster topRightThruster bottomLeftThruster bottomRightThruster


startPos = (Vector2d 0 0)
startDir = (Vector2d 0 1.0)
startVel = (Vector2d 0.0 0.0)


blL = Vector2d (-0.002) (0.00)
blR = Vector2d 0.002 0.00
blT = Vector2d 0.00 0.01
blB = Vector2d 0 (-0.005)


bulletModel = ([blL, blT, blR, blB], [[blL, blT, blR, blB]])
