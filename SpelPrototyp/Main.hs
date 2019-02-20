

module Main(main) where
import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Data.Color (makeColor, red)
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort
--import Graphics.UI.GLUT
import Control.Exception
import Data.Array

import Graphics.Gloss.Data.Picture
import System.Random

--  A data structure to hold the state of the Pong game.
data CandyGame = Game
  { --playerLoc :: (Float, Float)
    squareLoc :: (Float, Float) 
  , player1 :: Float
  , playerMove :: Float
  , player2 :: Float} deriving Show

-- Saved values --
width, height, offset :: Int


width = 500
height = 500
offset = 100




---------------------------- Mainfunktionen ----------------------------------------------------
{-- | Play a game in a window.
play :: Display -- ^ Window to draw game in.
     -> Color   -- ^ Background color.
     -> Int     -- ^ Number of simulation steps per second of real time.
     -> a       -- ^ The initial game state.
     -> (a -> Picture)       -- ^ A function to render the world a picture.
     -> (Event -> a -> a)    -- ^ A function to handle input events.
     -> (Float -> a -> a)    -- ^ A function to step the world one iteration.
     -> IO ()

-}

main :: IO ()
main = play window background fps initialState render handleKeys (const id) -- Löst så att rutan inte flyttas med (const id)

------------------------ Playfunktionens argument ----------------------------------------------
window :: Display
window = InWindow "CrushTheCandy" (width, height) (offset, offset)

background :: Color
background = black

fps :: Int
fps = 60

--  Initialize the game with this game state.
initialState :: CandyGame
initialState = Game { --playerLoc = ((-200),200)
                      squareLoc = ((-200),200)
                    , playerMove  = 0
                    , player1 = 40
                    , player2 = -80}

--  Draw a candy game state (convert it to a picture).
render :: CandyGame ->  Picture
render game = pictures ((paintRectangles (squareLocations 5 (-200,200))) ++ [mkMarker rose $ squareLoc game]) -- Konkatinerade rutnätet med Eriks ruta

--  Respond to key events.
handleKeys :: Event -> CandyGame -> CandyGame

-- For an 's' keypress, reset the ball to the center.
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) game = moveSquare 0 100 game
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) game = moveSquare 0 (-100) game
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) game = moveSquare (-100) 0 game
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) game = moveSquare 100 0 game
  -- game { playerMove = 1 }
handleKeys _ game = game

-------------------------------------------------------------------------------------------------


moveSquare :: Float -> Float
            -> CandyGame -- The initial game state
            -> CandyGame -- A new game state with an updated square position
moveSquare moveX moveY game = game { squareLoc = (x', y') }
  where
    -- Old locations and velocities.
    (x, y) = squareLoc game

    -- New locations. -- Nu kan man inte gå ut ur fönstret.
    x' | x >= (200) && moveX >= 0 = x
       | x >= (200) && moveX < 0 = x + moveX
       | x <= (-200) && moveX <= 0 = x
       | x <= (-200) && moveX > 0 = x + moveX
       | otherwise = x + moveX
   -- x' = x - move
    y' | y >= (200) && moveY >= 0 = y
       | y >= (200) && moveY < 0 = y + moveY
       | y <= (-200) && moveY <= 0 = y
       | y <= (-200) && moveY > 0 = y + moveY
       | otherwise = y + moveY






-- Do nothing for all other events.




  
mkMarker :: Color -> (Float, Float) -> Picture
mkMarker col (x,y) = pictures
  [ translate x y $ color white $ lineLoop $ rectanglePath 100 100]

paddleColor = light (light blue)




--main = (InWindow "Nice Window" (200, 200) (10, 10)) white (Circle 80)
--rectangleWire :: Float -> Float -> Picture
--rectangleWire sizeX sizeY = lineLoop $ rectanglePath 100 100
rectangleWire1 =  translate (-200) 200 $ lineLoop $ rectanglePath 100 100
rectangleWire2 =  rectangleWire 200 100


paintRectangles :: [(Float,Float)] -> [Picture]
paintRectangles [] = []
paintRectangles ((a,b):xs) = [Color red $ translate a b $ lineLoop $ rectanglePath 100 100] ++
                             paintRectangles xs
{-
PRE: Börjar på (-200,200)
-}
squareLocations :: Float -> (Float, Float)-> [(Float,Float)]
squareLocations 0  (a,b)
  | a > 200 && b < (-200) = []
  | otherwise = squareLocations 5 ((-200), b-100)
 
squareLocations int (a,b) = [(a,b)] ++ squareLocations (int-1) (a+100,b)














{-
window = InWindow "Functional" (640, 480) (100, 100)

backgroundColor = makeColor 0 0 0 255

gameAsPicture _ = Blank

transformGame _ game = game

main = IO ()
main = play window backgroundColor 30 initialGame gameAsPicture transformGame (const id)


data Player = Player deriving (Eq, Show)
data State = Running | GameOver (Maybe Player)

type Board = Array (Int,Int) Cell

data Game =  Game {gameBoard :: Board
                  , gamePlayer :: Player
                  , gameState :: State
                  } deriving (Eq, Show)



n :: int
n = 5

initialGame = Game { gameBoard = array indexRange $ zip (range indexRange) (cycle [Empty])
                   , gamePlayer = Player
                   , gameState = Running
                   }
      where indexRange = ((0,0) (n - 1, n - 1))

-}






{-
-- | Run a finite-time-step simulation in a window.
simulate :: Display -- ^ How to display the game.
         -> Color   -- ^ Background color.
         -> Int     -- ^ Number of simulation steps to take per second of real time.
         -> CandyGame       -- ^ The initial game state. 
         -> (CandyGame -> Picture) -- ^ A function to render the game state to a picture. 
         -> (ViewPort -> Float -> CandyGame -> CandyGame) -- ^ A function to step the game once. 
        -> IO ()

-- | Number of frames to show per second.
fps :: Int
fps = 30



-- | Update the game by moving the marker.
-- Ignore the ViewPort argument.
update :: ViewPort -> Float -> CandyGame -> CandyGame
update _ seconds =  moveplayer seconds




 




main = IO ()
main = play
    (InWindow "Candy-Crush" (500,500) location)
    white
    20
    renderBoard


location = (710, 290)
windowSize = (500,500)

-- | The starting state for the game.
initialState :: CandyGame
initialState = Game
  { playerLoc = ((-200), 200)}

updateGameState :: 



-- Data describing the state of the game. 
data CandyGame = Game
  { playerLoc :: (Float, Float) } deriving Show  -- player (x, y) location.

squares = 5


main :: IO ()
main = display (InWindow "Hello World" windowSize location)
               (makeColor 0.9 0.9 0.9 1)
               (Pictures $ (paintRectangles (squareLocations 5 ((-200),200))) ++ candyMarker)


paintRectangles :: [(Float,Float)] -> [Picture]
paintRectangles [] = []
paintRectangles ((a,b):xs) = [Color red $ translate a b $ lineLoop $ rectanglePath 100 100] ++
                             paintRectangles xs
{-
PRE: Börjar på (-200,200)
-}
squareLocations :: Float -> (Float, Float)-> [(Float,Float)]
squareLocations 0  (a,b)
  | a > 200 && b < (-200) = []
  | otherwise = squareLocations 5 ((-200), b-100)
 
squareLocations int (a,b) = [(a,b)] ++ squareLocations (int-1) (a+100,b)





--main = (InWindow "Nice Window" (200, 200) (10, 10)) white (Circle 80)
--rectangleWire :: Float -> Float -> Picture
--rectangleWire sizeX sizeY = lineLoop $ rectanglePath 100 100
rectangleWire1 =  translate (-200) 200 $ lineLoop $ rectanglePath 100 100
rectangleWire2 =   rectangleWire 200 100


--candyMarker :: (Float, Float)
candyMarker = [Color white $ translate 200 (-200) $ lineLoop $ rectanglePath 100 100]
-- For an 's' keypress, reset the ball to the center.


handleKeys (EventKey (Char 's') _ _ _) game =
  game { playerLoc = (100, (-200)) }


-}
