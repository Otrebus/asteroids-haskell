module Player (
    playerModel,
    bulletModel,
    mainThruster,
    topLeftThruster,
    topRightThruster,
    bottomLeftThruster,
    bottomRightThruster,
    thrusters,
    startPos,
    startDir,
    startVel,
    plT,
    Thrusters(..),
    Thruster(..),

 ) where
import Math


data Thrusters = Thrusters {
    e_main :: Thruster,
    e_topleft :: Thruster,
    e_topright :: Thruster,
    e_bottomleft :: Thruster,
    e_bottomright :: Thruster
} deriving (Show)


data Thruster = Thruster {
    t_lastEmitted :: Float,
    t_emissionInterval :: Float,
    t_position :: Vector2d,
    t_direction :: Vector2d
} deriving (Show)


plBl = Vector2d (-0.04) (-0.04) -- Bottom left corner
plT = Vector2d 0 0.04           -- Top corner
plBr = Vector2d 0.04 (-0.04)    -- Bottom right corner
plB = Vector2d 0.00 (-0.02)    -- Bottom
playerModel = ([plBl, plB, plBr, plT], [[plBl, plB, plT], [plT, plB, plBr]])


mainThruster = Thruster 0.0 0.001 plB (Vector2d 0 (-1))
topLeftThruster = Thruster 0.0 0.01 (interpolate plT plBl 0.2) (Vector2d (-1) 0)
topRightThruster = Thruster 0.0 0.01 (interpolate plT plBr 0.2) (Vector2d (1) 0)
bottomLeftThruster = Thruster 0.0 0.01 (interpolate plT plBl 0.8) (Vector2d (-1) 0)
bottomRightThruster = Thruster 0.0 0.01 (interpolate plT plBr 0.8) (Vector2d (1) 0)
thrusters = Thrusters mainThruster topLeftThruster topRightThruster bottomLeftThruster bottomRightThruster


blL = Vector2d (-0.002) (0.00)
blR = Vector2d 0.002 0.00
blT = Vector2d 0.00 0.01
blB = Vector2d 0 (-0.005)
bulletModel = ([blL, blT, blR, blB], [[blL, blT, blR, blB]])


startPos = (Vector2d 0 0)
startDir = normalize (Vector2d 0.2 0.5)
startVel = (Vector2d 0.0 0.0)
