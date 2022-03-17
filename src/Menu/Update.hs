module Menu.Update where

import State
import Control.Monad.State
import qualified Graphics.UI.GLFW as GLFW


-- Updates the menu and the state of the program based on the player's actions in the menu
runFrame :: State ProgramState ()
runFrame = do
    
    state <- get

    let prevPressed = gls_keysHeld state
    let pressed = gls_keysPressed state

    case ((ms_menuChoice . gls_menuState) state) of
        -- Hard-coded, if this were to grow larger I'd operate this on some sort of tree
        Continue -> do
            when (GLFW.Key'D `elem` pressed) $ do
                put $ state { gls_menuState = (gls_menuState state) { ms_menuChoice = Quit } }
            when (GLFW.Key'Enter `elem` pressed || GLFW.Key'Space `elem` pressed) $ do
                put $ state { gls_mode = Playing }
        Quit -> do
            when (GLFW.Key'E `elem` pressed) $ do
                put $ state { gls_menuState = (gls_menuState state) { ms_menuChoice = Continue } }
            when (GLFW.Key'Enter `elem` pressed || GLFW.Key'Space `elem` pressed) $ do
                put $ state { gls_mode = Exiting }

    state <- get
    when (GLFW.Key'Escape `elem` pressed) $ do
        put $ state { gls_mode = Playing }
