module Menu.Update where

import State
import Control.Monad.State
import qualified Graphics.UI.GLFW as GLFW


runFrame :: [GLFW.Key] -> State ProgramState ()
runFrame input = do
    
    state <- get

    let prevPressed = gls_keysPressed state
    let newDown = filter (\k -> not (k `elem` prevPressed)) input

    case ((ms_menuChoice . gls_menuState) state) of
        -- Hard-coded, if this were to grow larger I'd operate this on some sort of tree
        Continue -> do
            when (GLFW.Key'D `elem` newDown) $ do
                put $ state { gls_menuState = (gls_menuState state) { ms_menuChoice = Quit } }
            when (GLFW.Key'Enter `elem` newDown || GLFW.Key'Space `elem` newDown) $ do
                put $ state { gls_mode = Playing }
        Quit -> do
            when (GLFW.Key'E `elem` newDown) $ do
                put $ state { gls_menuState = (gls_menuState state) { ms_menuChoice = Continue } }
            when (GLFW.Key'Enter `elem` newDown || GLFW.Key'Space `elem` newDown) $ do
                put $ state { gls_mode = Exiting }

    state <- get
    when (GLFW.Key'Escape `elem` newDown) $ do
        put $ state { gls_mode = Playing }
