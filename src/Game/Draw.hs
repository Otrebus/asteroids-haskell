module Game.Draw where

import qualified Graphics.Rendering.OpenGL as GL hiding (get, rotate)
import Graphics.Rendering.OpenGL (
    vertex,
    clear,
    ClearBuffer(ColorBuffer),
    renderPrimitive,
    PrimitiveMode(
        Points,
        TriangleStrip,
        LineLoop,
        TriangleFan)
    )
import Utils.Font
import Utils.Math
import State
import Control.Monad.State ()
import Control.Monad
import Player
import Utils.Rendering


-- Draws particles (dots)
drawParticles ::
    Time ->       -- The current time
    [Particle] -> -- The particles to draw
    IO ()
drawParticles time particles = do
    renderPrimitive Points $ forM_ particles renderParticle where
        renderParticle (Particle pos _ start life bri) = do
            let d = bri*((life-time)/(life-start))**3
            GL.color $ gray d
            vertex $ toVertex pos


-- Draws an OpenGL polygon on the screen
drawPolygon ::
    GL.Color4 Float -> -- The foreground color
    GL.Color4 Float -> -- The interior color
    Vertices ->        -- The vertices of the polygon
    IO ()
drawPolygon back fore verts = do
    GL.color $ back
    renderPrimitive TriangleFan $ do mapM_ vertex (map toVertex verts)
    GL.color $ fore
    renderPrimitive LineLoop $ do mapM_ vertex (map toVertex verts)
    return ()


-- Draws an object, consisting of a set of triangles, on the screen
drawObject ::
    Object -> -- The object to draw
    IO ()
drawObject (Object (lineVertices, triangles) (Vector2d a b) pos) = do
    let mat = Matrix2d b a (-a) b
    let tris = map (\v -> (map (toVertex . (pos ^+^) . ((#*^) mat)) v)) triangles
    let ys = map (toVertex . (pos ^+^) . ((#*^) mat)) lineVertices

    GL.color darkGray
    forM_ tris (\tri -> renderPrimitive TriangleStrip $ (do mapM_ vertex tri))

    GL.color white
    renderPrimitive LineLoop $ do mapM_ vertex ys
    return ()


-- Draws an object as it toroidally wraps around the edges of the screen
drawDuplicateObjects ::
    Object -> -- The object to draw
    IO ()
drawDuplicateObjects (Object (vectors, tris) dir pos) = do
    let (Vector2d x y) = pos
    let (Vector2d a b) = dir
    let mat = Matrix2d b a (-a) b
    let mv = map ((pos ^+^) . ((#*^) mat)) vectors

    let fx (Vector2d x y) = x; fy (Vector2d x y) = y

    let a = 2*(-signum x) :: Float
    let b = 2*(-signum y) :: Float

    let wrap = [Vector2d a 0.0, Vector2d 0.0 b, Vector2d a 0.0, Vector2d a b]

    forM_ wrap $ \v -> do
        drawObject (Object (vectors, tris) dir (pos ^+^ v))


-- Draws an asteroid as it toroidally wraps around the edges of the screen
drawduplicateAsteroid ::
    Vertices -> -- The vertices of the asteroid
    IO ()
drawduplicateAsteroid vectors = do
    let (Vector2d x y) = polyCentroid vectors

    let a = 2*(-signum x) :: Float
    let b = 2*(-signum y) :: Float

    let fx (Vector2d x y) = x; fy (Vector2d x y) = y

    let wrap = [Vector2d a 0.0, Vector2d 0.0 b, Vector2d a 0.0, Vector2d a b]

    forM_ wrap $ \v -> do
        drawPolygon darkGray white $ map (^+^ v) vectors


-- Draws the animation of the ship icon moving from the top right to the ship position in the game
-- as the player is respawning
drawLife ::
    AliveState -> -- Whether the player is currently being respawned or playing
    Time ->      --  The current time
    Int ->       --  The maximum number of lives
    Int ->       --  The current number of lives
    IO ()
drawLife (Respawning since) time maxLife life = drawPolygon black white pm
    where
        x = 0.92 - ((realToFrac life - 1.0))*0.12
        t = 2.0*(time - since)
        startAng = pi/4 - angle startDir

        movePos = interpolate (Vector2d x 0.92) (Vector2d 0 0) t
        moveRot = -pi/4 + (startAng+pi/4)*t
        moveScale = 0.8 + (1.0-0.8)*t

        (pos, rot, scale) = if maxLife == life then (movePos, moveRot, moveScale) else ((Vector2d x 0.92), -pi/4, 0.8)
        pm = map (pos ^+^) (map ((scale !*^) . (rotate (rot))) (fst playerModel))
    
drawLife aliveState time max life = drawPolygon black white pm
    where
        x = 0.92 - ((realToFrac life - 1.0))*0.12
        pm = map ((Vector2d x 0.92) ^+^) (map ((0.8 !*^) . (rotate (-pi/4))) (fst playerModel))            



-- Draws the ship icons representing the remaining number of lives in the top right corner
drawLives ::
    GameState -> IO () -- The resulting state
drawLives state = do
    let score = gs_score state
    let lives = gs_lives state
    let time = gs_time state
    let aliveState = (ps_aliveState . gs_playerState) state

    mapM_ (drawLife aliveState time lives) [1..lives]

    return ()


-- Displays the current state of the game graphically
draw ::
    GameState -> IO () -- The resulting state
draw gs@(GameState playerState particles polygonParticles bullets asteroids time _ score lives _ _) = do
    clear [ColorBuffer]

    drawText 0.1 (Vector2d (-0.92) 0.88) ("Score: " ++ (show . round $ score))
    -- drawText 0.05 (Vector2d (-0.9) (-0.9)) ("Fps: " ++ (show (1.0/(gs_time gs - gs_prevTime gs))))

    drawLives gs

    let aliveState = ps_aliveState playerState

    case aliveState of
        Alive -> do
            drawObject $ Object playerModel (ps_direction playerState) (ps_position playerState)
            drawDuplicateObjects $ Object playerModel (ps_direction playerState) (ps_position playerState)
        Exploding since -> do
            when (time - since > 1.0) $ do
                when (lives > 0) $ do
                    centerText 0.1 (Vector2d (-0.3) 0.0) (Vector2d 0.3 0.0) "Press enter to respawn"
        GameOver since -> do
            centerText 0.15 (Vector2d (-0.3) 0.15) (Vector2d 0.3 0.0) "Game Over"
            centerText 0.09 (Vector2d (-0.3) (-0.15)) (Vector2d 0.3 0.0) "Press enter to restart"
        Winning since -> do 
            drawObject $ Object playerModel (ps_direction playerState) (ps_position playerState)
            drawDuplicateObjects $ Object playerModel (ps_direction playerState) (ps_position playerState)
            when (time - since > 0.5) $ do
                centerText 0.15 (Vector2d (-0.3) 0.15) (Vector2d 0.3 0.0) "Level complete"
                centerText 0.09 (Vector2d (-0.3) (-0.15)) (Vector2d 0.3 0.0) "Press enter to continue"
        _ -> do
            return ()
        
    drawParticles time particles

    forM_ bullets $ \(Bullet pos dir vel _) -> do
        drawObject (Object bulletModel dir pos)
        drawDuplicateObjects (Object bulletModel dir pos)

    forM_ polygonParticles $ \(PolygonParticle vert vel angVel life) -> do
        let t = abs (life - time)
        let c = min 1.0 t ** 1.2
        drawPolygon darkGray (gray c) vert

    forM_ asteroids $ \(Asteroid dir vel vert _) -> do
        drawPolygon darkGray white vert
        drawduplicateAsteroid vert
