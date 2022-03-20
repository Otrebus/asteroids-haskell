module State (
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
    ProgramState(..),
    MenuState(..),
    ProgramMode(..),
    IntroState(..),
    MenuChoice(..),
    Star(..),
    Lifter,
    SplitPolygon,
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
    onPolygonParticleVertices,
    onPolygonParticles,
    onPolygonVertices,
    onParticlePos,
    onBulletPos,
    onStars,
    Object(..),
    rndFloat,
    rndInt,
    initState) where
import Utils.Math
import Player
import System.Random
import Control.Monad.State (State, put, get)
import qualified Graphics.UI.GLFW as GLFW (Key)
import Control.Monad.Random


data Action = Shooting | Accelerating | Decelerating | TurningLeft |
              TurningRight | Escaping | Entering deriving (Show, Eq)

data AliveState = Alive | Exploding Float | Respawning Float |
                  GameOver Float | Winning Float deriving (Show, Eq)

data ProgramMode = Intro | Playing | Menu | Exiting |
                   Restarting | Progressing deriving (Show, Eq)

data MenuChoice = Continue | Quit | Yes | No deriving (Show, Eq)

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
    a_vertices :: Vertices,
    a_splitEdge :: Maybe Edge
} deriving (Show, Eq)


type SplitPolygon = (Vertices, Edge)


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
    gs_score :: Float,
    gs_lives :: Int,
    gs_level :: Int,
    gs_rng :: StdGen
} deriving (Show)

data MenuState = MenuState {
    ms_menuChoice :: MenuChoice
}

data Star = Star {
    st_pos :: Vector2d,
    st_startTime :: Float
} deriving Show

data IntroState = IntroState {
    is_time :: Float,
    is_prevTime :: Float,
    is_stars :: [Star],
    is_lastStar :: Float,
    is_rng :: StdGen
} deriving Show

data ProgramState = ProgramState {
    gls_gameState :: GameState,
    gls_menuState :: MenuState,
    gls_introState :: IntroState,
    gls_mode :: ProgramMode,
    gls_keysHeld :: [GLFW.Key],
    gls_keysPressed :: [GLFW.Key]
}

-- The semantic editor combinators to access state

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

onStars :: Lifter [Star] IntroState
onStars f is = is { is_stars = f (is_stars is) }

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

onPolygonVertices :: Lifter [Vector2d] Asteroid
onPolygonVertices f as = as { a_vertices = f (a_vertices as) }

onPolygonParticleVertices :: Lifter [Vector2d] PolygonParticle
onPolygonParticleVertices f pp = pp { pp_vertices = f (pp_vertices pp) }

onGameState :: Lifter GameState ProgramState
onGameState f ps = ps { gls_gameState = f (gls_gameState ps) }


-- Returns an integer between two numbers
rndInt ::
    Int -> -- The lower bound
    Int -> -- The upper bound
    State GameState (Int)
rndInt min max = do
    state <- get
    let (value, newGenerator) = randomR (min, max) (gs_rng state)
    put (state { gs_rng = newGenerator })
    return value


-- Returns a float between two numbers
rndFloat ::
    Float -> -- The lower bound
    Float -> -- The upper bound
    State GameState (Float)
rndFloat min max = do
    state <- get
    let (value, newGenerator) = randomR (min,max) (gs_rng state)
    put (state { gs_rng = newGenerator})
    return value



-- Initializes a set of asteroids on the screen
initAsteroids ::
    Int -> -- The number of asteroids to add
    Rand StdGen [Asteroid]
initAsteroids n = do

    let size = 0.15*rtf n

    t0 <- getRandomR (0, 2*pi)

    let poses = [Vector2d (cos a) (sin a) | i <- [1..n], let a = t0 + rtf i*2.0*pi/rtf n]

    forM poses $ \pos -> do
        let diag = size*1.44
        dist <- getRandomR (diag*1.2, max (diag*1.2) (1.0-diag*1.2))
        poly <- (randomPolygon 13 (dist!*^pos) size size)
        velRot <- getRandomR (-0.2, 0.2)
        dir <- fromList [(1, 0.5), (-1, 0.5)]
        let vel = dir !*^ rotate velRot (0.15!*^((normalize . ortho) pos))
        return (Asteroid (0.25*rtf n) vel poly Nothing)

    where rtf = realToFrac


-- Initializes the state of the game
initState ::
    Int ->    -- The game level
    Float ->  -- The current score
    StdGen -> -- The random number generator
    GameState
initState level score rng =
    let (asteroids, rnd) = runRand (initAsteroids level) rng
        initPlayerState = PlayerState startPos startDir startVel 0 thrusters score Alive
    in GameState initPlayerState [] [] [] asteroids 0.0 0.0 0 3 level rng
