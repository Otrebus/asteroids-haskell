module GameState (GameState (..), PlayerState (..), Action (..)) where
import Math

data Action = Accelerating | Breaking | TurningLeft | TurningRight | Shooting

data PlayerState = PlayerState {
    actions :: [Action],
    position :: Position
}

data GameState = GameState {
    playerState :: PlayerState,
    count :: Int
}