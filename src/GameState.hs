module GameState (GameState (..), PlayerState (..), Action (..)) where
import Math

data Action = Accelerating | Decelerating | TurningLeft | TurningRight deriving (Show, Eq)

data PlayerState = PlayerState {
    position :: Position,
    direction :: Direction,
    speed :: Velocity
}

data GameState = GameState {
    playerState :: PlayerState,
    count :: Int
}