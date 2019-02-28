module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.Color (makeColor, red)
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort
--import Graphics.UI.GLUT
import Control.Exception
import Data.Array

import Graphics.Gloss.Data.Picture




width, height, offset :: Int
width = 500
height = 500
offset = 100

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

background :: Color
background = black

{-
-- Run a finite-time-step simulation in a window.
simulate :: Display -- ^ How to display the game.
         -> Color   -- ^ Background color.
         -> Int     -- ^ Number of simulation steps to take per second of real time.
         -> CandyGame       -- ^ The initial game state. 
         -> (CandyGame -> Picture) -- ^ A function to render the game state to a picture. 
         -> (ViewPort -> Float -> CandyGame -> CandyGame) -- ^ A function to step the game once. 
        -> IO ()
-}
fps :: Int
fps = 60

main :: IO ()
main = play window background fps initialState render handleKeys update

moveSquare :: Float    -- ^ The number of seconds since last update
         -> CandyGame -- ^ The initial game state
         -> CandyGame -- ^ A new game state with an updated ball position


moveSquare seconds game = game { squareLoc = (x', y') }
  where
    -- Old locations and velocities.
    (x, y) = squareLoc game
    (vx, vy) = squareVel game

    -- New locations.
    x' = x + vx * seconds
    y' = y + vy * seconds

update :: Float -> CandyGame -> CandyGame 
update = moveSquare

{-
-- | Play a game in a window.
play :: Display -- ^ Window to draw game in.
     -> Color   -- ^ Background color.
     -> Int     -- ^ Number of simulation steps per second of real time.
     -> a       -- ^ The initial game state.
     -> (a -> Picture)       -- ^ A function to render the world a picture.
     -> (Event -> a -> a)    -- ^ A function to handle input events.
     -> (Float -> a -> a)    -- ^ A function to step the world one iteration.
     -> IO ()

-}
-- | Respond to key events.
handleKeys :: Event -> CandyGame -> CandyGame

-- For an 's' keypress, reset the ball to the center.
handleKeys (EventKey (Char 's') _ _ _) game =
  game { player1 = 100 }

-- Do nothing for all other events.
handleKeys _ game = game

drawing :: Picture
drawing = pictures
  [translate 30 50 $ color paddleColor $ rectangleSolid 10 50]
  where
    squareColor = red


--  A data structure to hold the state of the Pong game.
data CandyGame = Game
  { --playerLoc :: (Float, Float)
    squareLoc :: (Float, Float) 
  , squareVel :: (Float, Float)
  , player1 :: Float
  , player2 :: Float} deriving Show

--  Draw a pong game state (convert it to a picture).
render :: CandyGame -> Picture
render game = pictures [mkPaddle rose 120 $ player1 game,
                        mkPaddle orange (-120) $ player2 game]
  
mkPaddle :: Color -> Float -> Float -> Picture
mkPaddle col x y = pictures
  [ translate x y $ color col $ rectangleSolid 26 86
  , translate x y $ color paddleColor $ rectangleSolid 20 80
  ]

paddleColor = light (light blue)

--  Initialize the game with this game state.
initialState :: CandyGame
initialState = Game { --playerLoc = ((-200),200)
                      squareLoc = ((-100),200)
                    , player1 = 40
                    , player2 = -80}



--main = (InWindow "Nice Window" (200, 200) (10, 10)) white (Circle 80)
--rectangleWire :: Float -> Float -> Picture
--rectangleWire sizeX sizeY = lineLoop $ rectanglePath 100 100
rectangleWire1 =  translate (-200) 200 $ lineLoop $ rectanglePath 100 100
rectangleWire2 =   rectangleWire 200 100


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
