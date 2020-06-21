module GameState where

data Action = Accelerating | Breaking | TurningLeft | TurningRight | Shooting

data PlayerState = PlayerState {
    actions :: [Action] 
}

data GameState = GameState {
    playerState :: PlayerState,
    count :: Int
}