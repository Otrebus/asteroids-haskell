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


-- Draws an object consisting of a set of convex interior polygons and a perimeter polygon
drawFigure ::
    GL.Color4 Float ->        -- The foreground color
    GL.Color4 Float ->        -- The interior color
    (Vertices, [Vertices]) -> -- The figures to draw (perimeter, interior polygons)
    IO ()
drawFigure fore back (verts, tris) = do
    GL.color back
    forM_ tris (\tri -> renderPrimitive TriangleFan $ mapM (vertex . toVertex) tri)

    GL.color fore
    renderPrimitive LineLoop $ mapM (vertex . toVertex) verts
    return ()


-- Draws an object as it toroidally wraps around the edges of the screen
drawDuplicateFigures ::
    GL.Color4 Float ->        -- The foreground color
    GL.Color4 Float ->        -- The interior color
    (Vertices, [Vertices]) -> -- The figures to draw (perimeter, interior polygons)
    IO ()
drawDuplicateFigures fore back (vectors, tris) = do
    let (Vector2d x y) = polyCentroid vectors

    let a = 2*(-signum x) :: Float
    let b = 2*(-signum y) :: Float

    let wrap = [Vector2d a 0.0, Vector2d 0.0 b, Vector2d a 0.0, Vector2d a b]

    forM_ wrap $ \v -> do
        drawFigure fore back (map (^+^ v) vectors, map (map (^+^ v)) tris)


-- Turns an object with its position and orientation into a pair of interior polygons and
-- perimeter polygon
transform :: Object -> (Vertices, [Vertices])
transform (Object (lineVertices, triangles) (Vector2d a b) pos) =
    let mat = Matrix2d b a (-a) b
        tris = map (\v -> (map ((pos ^+^) . ((#*^) mat))) v) triangles
        ys = map ((pos ^+^) . ((#*^) mat)) lineVertices

    in (ys, tris)


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


-- Draws the animation of the ship icon moving from the top right to the ship position in the game
-- as the player is respawning
drawLife ::
    AliveState -> -- Whether the player is currently being respawned or playing
    Time ->      --  The current time
    Int ->       --  The maximum number of lives
    Int ->       --  The current number of lives
    IO ()
drawLife (Respawning since) time maxLife life = drawFigure white black (pm, [pm])
    where
        x = 0.92 - ((realToFrac life - 1.0))*0.12
        t = 2.0*(time - since)
        startAng = pi/4 - angle startDir

        movePos = interpolate (Vector2d x 0.92) (Vector2d 0 0) t
        moveRot = -pi/4 + (startAng+pi/4)*t
        moveScale = 0.8 + (1.0-0.8)*t

        (pos, rot, scale) = if maxLife == life then (movePos, moveRot, moveScale) else ((Vector2d x 0.92), -pi/4, 0.8)
        pm = map (pos ^+^) (map ((scale !*^) . (rotate (rot))) (fst playerModel))
    
drawLife aliveState time max life = drawFigure white black (pm, [pm])
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
    GameState -> -- The state of the game
    IO ()
draw gs@(GameState playerState particles polygonParticles bullets asteroids time _ score lives _ _) = do
    clear [ColorBuffer]

    drawText 0.1 (Vector2d (-0.92) 0.88) ("Score: " ++ (show . round $ score))
    -- drawText 0.05 (Vector2d (-0.9) (-0.9)) ("Fps: " ++ (show (1.0/(gs_time gs - gs_prevTime gs))))

    drawLives gs

    let aliveState = ps_aliveState playerState

    case aliveState of
        Alive -> do
            drawWrap $ transform $ Object playerModel (ps_direction playerState) (ps_position playerState)
        Exploding since -> do
            when (time - since > 1.0) $ do
                when (lives > 0) $ do
                    centerText 0.1 (Vector2d (-0.3) 0.0) (Vector2d 0.3 0.0) "Press enter to respawn"
        GameOver since -> do
            centerText 0.15 (Vector2d (-0.3) 0.15) (Vector2d 0.3 0.0) "Game Over"
            centerText 0.09 (Vector2d (-0.3) (-0.15)) (Vector2d 0.3 0.0) "Press enter to restart"
        Winning since -> do 
            drawWrap $ transform $ Object playerModel (ps_direction playerState) (ps_position playerState)
            when (time - since > 0.5) $ do
                centerText 0.15 (Vector2d (-0.3) 0.15) (Vector2d 0.3 0.0) "Level complete"
                centerText 0.09 (Vector2d (-0.3) (-0.15)) (Vector2d 0.3 0.0) "Press enter to continue"
        _ -> do
            return ()
        
    drawParticles time particles

    forM_ bullets $ \(Bullet pos dir vel _) -> do
        drawWrap $ transform $ Object bulletModel dir pos

    forM_ polygonParticles $ \(PolygonParticle vert vel angVel life) -> do
        let t = abs (life - time)
        let c = min 1.0 t ** 1.2
        drawFigure (gray c) black (vert, [vert])

    forM_ asteroids $ \(Asteroid dir vel vert _) -> do
        drawWrap (vert, [vert])

    where
        drawWrap obj = do
            (drawFigure white darkGray) obj
            (drawDuplicateFigures white darkGray) obj
