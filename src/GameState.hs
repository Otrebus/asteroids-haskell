

module GameState (
    GameState (..),
    PlayerState (..),
    Action (..),
    Thrusters(..),
    Thruster(..),
    Particle(..),
    onThrusters,
    onPlayerState,
    onMainThruster,
    onTopLeftThruster,
    onTopRightThruster,
    onBottomLeftThruster,
    onBottomRightThruster) where
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
    t_lastEmitted :: Float,
    t_emissionInterval :: Float,
    t_position :: Vector2d,
    t_direction :: Vector2d
}

data Particle = Particle {
    p_position :: Position,
    p_velocity :: Velocity,
    p_timeStart :: Float,
    p_lifeTime :: Float,
    p_brightness :: Float
} deriving (Show)

data GameState = GameState {
    gs_playerState :: PlayerState,
    gs_particles :: [Particle],
    gs_time :: Float,
    gs_prevTime :: Float,
    gs_rng :: StdGen
}

type Unop a = a -> a

type Lifter p q = Unop p -> Unop q

-- onNextEmitted :: Lifter Float Thrusters
-- onNextEmitted f th = th { t_nextEmitted = f (t_nextEmitted th) }

onMainThruster :: Lifter Thruster Thrusters
onMainThruster f t = t { e_main = f (e_main t) }

onTopLeftThruster :: Lifter Thruster Thrusters
onTopLeftThruster f t = t { e_topleft = f (e_topleft t) }

onTopRightThruster :: Lifter Thruster Thrusters
onTopRightThruster f t = t { e_topright = f (e_topright t) }

onBottomLeftThruster :: Lifter Thruster Thrusters
onBottomLeftThruster f t = t { e_bottomleft = f (e_bottomleft t) }

onBottomRightThruster :: Lifter Thruster Thrusters
onBottomRightThruster f t = t { e_bottomright = f (e_bottomright t) }

onThrusters :: Lifter Thrusters PlayerState
onThrusters f ps = ps { ps_thrusters = f (ps_thrusters ps) }

onPlayerState :: Lifter PlayerState GameState
onPlayerState f gs = gs { gs_playerState = f (gs_playerState gs) }

