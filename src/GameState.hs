module GameState (
    GameState (..),
    PlayerState (..),
    Action (..),
    Thrusters(..),
    Thruster(..),
    Particle(..),
    PolygonParticle(..),
    Bullet(..),
    Asteroid(..),
    AliveState(..),
    onThrusters,
    onPlayerState,
    onPlayerPos,
    onMainThruster,
    onTopLeftThruster,
    onTopRightThruster,
    onBottomLeftThruster,
    onBottomRightThruster,
    onParticles,
    onBullets,
    onAsteroids,
    onPolygonParticles,
    onParticlePos,
    onBulletPos,
    Object(..)) where
import Math
import System.Random


data Action = Shooting | Accelerating | Decelerating | TurningLeft | TurningRight deriving (Show, Eq)
data AliveState = Alive | Dead deriving (Show, Eq)

data Object = Object (Vertices, [Vertices]) Direction Position

data PlayerState = PlayerState {
    ps_position :: Position,
    ps_direction :: Direction,
    ps_velocity :: Velocity,
    ps_angularVelocity :: Float,
    ps_thrusters :: Thrusters,
    ps_lastBullet :: Time,
    ps_aliveState :: AliveState
} deriving (Show)


data Bullet = Bullet {
    b_position :: Position,
    b_direction :: Direction,
    b_velocity :: Velocity,
    b_lifeTime :: Float
} deriving (Show, Eq)


data Asteroid = Asteroid {
    a_angularVelocity :: Float,
    a_velocity :: Velocity,
    a_vertices :: Vertices
} deriving (Show, Eq)


data Thrusters = Thrusters {
    e_main :: Thruster,
    e_reverse :: Thruster,
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


data Particle = Particle {
    p_position :: Position,
    p_velocity :: Velocity,
    p_timeStart :: Float,
    p_lifeTime :: Float,
    p_brightness :: Float
} deriving (Show)


data PolygonParticle = PolygonParticle {
    pp_vertices :: Vertices,
    pp_velocity :: Direction,
    pp_angularVelocity :: Float,
    pp_lifeTime :: Float
} deriving (Show)


data GameState = GameState {
    gs_playerState :: PlayerState,
    gs_particles :: [Particle],
    gs_polygonParticles :: [PolygonParticle],
    gs_bullets :: [Bullet],
    gs_asteroids :: [Asteroid],
    gs_time :: Float,
    gs_prevTime :: Float,
    gs_rng :: StdGen
} deriving (Show)

type Unop a = a -> a

type Lifter p q = Unop p -> Unop q

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

onPlayerPos :: Lifter Position PlayerState
onPlayerPos f ps = ps { ps_position = f (ps_position ps) }

onParticles :: Lifter [Particle] GameState
onParticles f gs = gs { gs_particles = f (gs_particles gs) }

onPolygonParticles :: Lifter [PolygonParticle] GameState
onPolygonParticles f gs = gs { gs_polygonParticles = f (gs_polygonParticles gs) }

onBullets :: Lifter [Bullet] GameState
onBullets f gs = gs { gs_bullets = f (gs_bullets gs) }

onAsteroids :: Lifter [Asteroid] GameState
onAsteroids f gs = gs { gs_asteroids = f (gs_asteroids gs) }

onParticlePos :: Lifter Position Particle
onParticlePos f p = p { p_position = f (p_position p) }

onBulletPos :: Lifter Position Bullet
onBulletPos f b = b { b_position = f (b_position b) }
