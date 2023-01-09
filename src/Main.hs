-- Code written by Leo Vainio

module Main where

import Control.Monad.IO.Class
import UI.NCurses


-- Holds information about the state of the game
data GameState = 
    Game { 
             player1pos :: (Integer, Integer)
           , player2pos :: (Integer, Integer)
           , player1dir :: Direction
           , player2dir :: Direction
           , xMax :: Integer
           , yMax :: Integer
           , player1snake :: [(Integer, Integer)]
           , player2snake :: [(Integer, Integer)]}
  | GameOver

-- Directions that players can head
data Direction = DirUp | DirDown | DirRight | DirLeft deriving (Eq)



-- Initiate curses and initiate the game
main :: IO ()
main = do
  runCurses $ do
    setEcho False
    setCursorMode CursorInvisible
    window <- defaultWindow
    (rows, cols) <- screenSize
    let startState = Game (20, 10) (cols-20, rows-10) DirDown DirUp (cols-1) (rows-1) [(20, 10)] [(cols-20, rows-10)]
    drawBorderWalls window startState
    mainLoop window startState

-- This function draws the borders of the game represented with '#'
drawBorderWalls :: Window -> GameState -> Curses ()
drawBorderWalls window s = do
    updateWindow window $ do
        moveCursor 0 0
        drawString (replicate (fromIntegral (xMax s)) '#')
        moveCursor (yMax s) 0
        drawString (replicate (fromIntegral (xMax s)) '#')
    render
    drawSide window (xMax s) (yMax s)
    
-- helper to drawBorderWalls to draw the side walls
drawSide :: Window -> Integer -> Integer -> Curses ()
drawSide window xLim y = do
    updateWindow window $ do
        moveCursor y 0
        drawString "#"
        moveCursor y (xLim-1)
        drawString "#"       
    if (y > 0)
        then drawSide window xLim (y-1)
        else render
             
-- Gameover loop: When a player has hit a wall this prints game over to the screen
mainLoop :: Window -> GameState -> Curses ()
mainLoop window GameOver = do
    (rows, cols) <- screenSize

    updateWindow window $ do
        moveCursor (rows-3) (cols `div` 2 - 4)   -- Print game over at bottom center of screen
        drawString "Game Over"
        moveCursor (rows-2) (cols `div` 2 - 8)   -- Print prompt at bottom center of screen 
        drawString "press 'q' to quit"
    render

    keyEvent <- getEvent window Nothing
    case keyEvent of
        Just e ->
            if (e == EventCharacter 'q')
                then return ()
                else mainLoop window GameOver

-- In game: the games main loop. Handles player movement, key input, collisions
mainLoop window s = do

    -- Draw a wall at players previous position
    updateWindow window $ do
        moveCursor (y1) (x1)  
        drawString "#"
        moveCursor (y2) (x2) 
        drawString "#"
    render

    -- Draw the players head in the new position
    drawHead window (x1New, y1New) (player1dir s)
    drawHead window (x2New, y2New) (player2dir s) 

    -- Check if any player has collided with a wall
    if (hitWall s)
        then mainLoop window GameOver

        -- Check for player input and change direction if a movement key has been pressed 
        else do
            keyEvent <- getEvent window (Just 100) 
            case keyEvent of
                Just e ->
                    -- Player 1 keys 'wasd'
                    if (e == EventCharacter 'a')
                        then mainLoop window (Game (x1New, y1New) (x2New, y2New) DirLeft dir2 (xMax s) (yMax s) ((x1,y1):(player1snake s)) ((x2, y2):(player2snake s)))
                    else if (e == EventCharacter 'd')
                        then mainLoop window (Game (x1New, y1New) (x2New, y2New) DirRight dir2 (xMax s) (yMax s) ((x1,y1):(player1snake s)) ((x2, y2):(player2snake s)))
                    else if (e == EventCharacter 'w')
                        then mainLoop window (Game (x1New, y1New) (x2New, y2New) DirUp dir2 (xMax s) (yMax s) ((x1,y1):(player1snake s)) ((x2, y2):(player2snake s)))
                    else if (e == EventCharacter 's')
                        then mainLoop window (Game (x1New, y1New) (x2New, y2New) DirDown dir2 (xMax s) (yMax s) ((x1,y1):(player1snake s)) ((x2, y2):(player2snake s)))

                    -- Player 2 keys 'ijkl'
                    else if (e == EventCharacter 'j')
                        then mainLoop window (Game (x1New, y1New) (x2New, y2New) dir1 DirLeft (xMax s) (yMax s) ((x1,y1):(player1snake s)) ((x2, y2):(player2snake s)))
                    else if (e == EventCharacter 'l')
                        then mainLoop window (Game (x1New, y1New) (x2New, y2New) dir1 DirRight (xMax s) (yMax s) ((x1,y1):(player1snake s)) ((x2, y2):(player2snake s)))
                    else if (e == EventCharacter 'i')
                        then mainLoop window (Game (x1New, y1New) (x2New, y2New) dir1 DirUp (xMax s) (yMax s) ((x1,y1):(player1snake s)) ((x2, y2):(player2snake s)))
                    else if (e == EventCharacter 'k')
                        then mainLoop window (Game (x1New, y1New) (x2New, y2New) dir1 DirDown (xMax s) (yMax s) ((x1,y1):(player1snake s)) ((x2, y2):(player2snake s)))

                    -- Other key pressed: make no changes to direction but update state
                    else mainLoop window (Game (x1New, y1New) (x2New, y2New) dir1 dir2 (xMax s) (yMax s) ((x1,y1):(player1snake s)) ((x2, y2):(player2snake s)))
                -- No key pressed: make no changes to direction but update state
                Nothing -> 
                    mainLoop window (Game (x1New, y1New) (x2New, y2New) dir1 dir2 (xMax s) (yMax s) ((x1,y1):(player1snake s)) ((x2, y2):(player2snake s)))

    where
        (x1,y1) = player1pos s
        (x2,y2) = player2pos s
        (x1New, y1New) = newPosition (x1, y1) (player1dir s)
        (x2New, y2New) = newPosition (x2, y2) (player2dir s)
        dir1 = player1dir s
        dir2 = player2dir s

-- Returns true if a snake has hit either the border walls or one of the players walls
hitWall :: GameState -> Bool
hitWall s = 
    if (x1 == xLim || x1 == 1 || x2 == xLim || x2 == 1)
        then True
    else if (y1 == yLim || y1 == 1 || y2 == yLim || y2 == 1)
        then True
    else if (elem (x1New, y1New) (player1snake s) || elem (x1New, y1New) (player2snake s) || elem (x2New, y2New) (player1snake s) || elem (x2New, y2New) (player2snake s))
         then True
    else False

    where
        (x1, y1) = player1pos s
        (x2, y2) = player2pos s
        xLim = (xMax s) - 2
        yLim = (yMax s) - 1
        (x1New, y1New) = newPosition (x1, y1) (player1dir s)
        (x2New, y2New) = newPosition (x2, y2) (player2dir s)

-- Returns the new position of the players head dependant on its direction and current position
newPosition :: (Integer, Integer) -> Direction -> (Integer, Integer)
newPosition (x, y) dir =
    if dir == DirUp
        then (x, y-1)
    else if dir == DirDown
        then (x, y+1)
    else if dir == DirRight
        then (x+1, y)
    else (x-1, y)

-- Draws the head of the player in its new position, character used is dependant on players direction
drawHead :: Window -> (Integer, Integer) -> Direction -> Curses ()
drawHead window (x, y) dir = do
    updateWindow window $ do
        if dir == DirUp
            then do
                moveCursor (y) (x)
                drawString "^"
        else if dir == DirDown
            then do
                moveCursor (y) (x)
                drawString "v"
        else if dir == DirRight
            then do
                moveCursor (y) (x)
                drawString ">"
        else if dir == DirLeft
            then do
                moveCursor (y) (x)
                drawString "<"
        else return ()
    render



