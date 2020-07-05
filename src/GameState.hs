

module GameState (GameState (..), PlayerState (..), Action (..), Thrusters(..), Thruster(..), Particle(..)) where
import Math
import System.Random

data Action = Accelerating | Decelerating | TurningLeft | TurningRight deriving (Show, Eq)

data PlayerState = PlayerState {
    ps_position :: Position,
    ps_direction :: Direction,
    ps_velocity :: Velocity,
    ps_thrusters :: Thrusters
}

data Thrusters = Thrusters {
    e_main :: Thruster,
    e_reverse :: Thruster,
    e_topleft :: Thruster,
    e_topright :: Thruster,
    e_bottomleft :: Thruster,
    e_bottomright :: Thruster
}

data Thruster = Thruster {
    t_nextEmitted :: Float,
    t_position :: Vector2d,
    t_direction :: Vector2d
}

data Particle = Particle {
    p_position :: Position,
    p_velocity :: Velocity,
    p_lifeTime :: Float,
    p_brightness :: Float
} deriving (Show)

data GameState = GameState {
    gs_playerState :: PlayerState,
    gs_particles :: [Particle],
    gs_count :: Int,
    gs_time :: Float,
    gs_rng :: StdGen
}