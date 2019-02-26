module Main(main) where
import System.IO.Unsafe  -- be careful! 
import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Data.Color (makeColor, red)
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort
import Control.Exception
import Data.Array
import Graphics.Gloss.Data.Picture
import System.Random
import Control.Monad




type Candy = (((Float,Float),Int),Color)
           -- (Float, Float) is the candy's coordinates
           -- Int is the 'spot' of the candy on the playing field
           -- Color is the candy's color
           

data CandyGame = Game
  { --playerLoc :: (Float, Float)
    squareLoc :: (Float, Float),
    listCoord :: Float
  } deriving Show

-- In order to get random candies we chose a random element from a list.
-- Saved values --
width, height, offset :: Int
width = 501 -- Ändrade till 501 eftersom ramen runt är 1 px bred
height = 501
offset = 100

--PRE: boxes >= 5 or the game will crash

boxes :: Float
boxes = 7

boxesInt :: Int
boxesInt = 7

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
window =  FullScreen--InWindow "CrushTheCandy" (width, height) (offset, offset)

background :: Color
background = black

fps :: Int
fps = 60

--  Initialize the game with this game state.
initialState :: CandyGame
initialState = Game { --playerLoc = ((-200),200)
                      squareLoc = ((-((boxes*50)-50)),((boxes*50)-50)),
                      listCoord = 0
                    }


--  Draw a candy game state (convert it to a picture).
render :: CandyGame ->  Picture
render game = pictures ((paintRectangles (squareLocations boxes (-200,200))) ++ [mkMarker rose $ squareLoc game] ++ (paintCandy (candyLocations boxes (-200,200)) (unsafePerformIO (c (boxesInt*boxesInt)) ))) -- Konkatinerade rutnätet med Eriks ruta

--  Respond to key events.
handleKeys :: Event -> CandyGame -> CandyGame

-- For the different keypress.
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) game = moveSquare 0 100 game
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) game = moveSquare 0 (-100) game
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) game = moveSquare (-100) 0 game
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) game = moveSquare 100 0 game
handleKeys _ game = game

-------------------------------------------------------------------------------------------------


moveSquare :: Float -> Float
            -> CandyGame -- The initial game state
            -> CandyGame -- A new game state with an updated square position
moveSquare moveX moveY game = game { squareLoc = (x', y'), listCoord = z' }
  where
    -- Old locations and velocities.
    (x, y) = squareLoc game
    z = trace ("z = " ++ (show z)) $ listCoord game

    -- New locations. -- Nu kan man inte gå ut ur fönstret.
    x' | x >= ((boxes * 50)-50) && moveX >= 0 = x
       | x >= ((boxes * 50)-50) && moveX < 0 = x + moveX
       | x <= (-((boxes * 50)-50)) && moveX <= 0 = x
       | x <= (-((boxes * 50)-50)) && moveX > 0 = x + moveX
       | otherwise = x + moveX
   -- x' = x - move
    y' | y >= ((boxes * 50)-50) && moveY >= 0 = y
       | y >= ((boxes * 50)-50) && moveY < 0 = y + moveY
       | y <= (-((boxes * 50)-50)) && moveY <= 0 = y
       | y <= (-((boxes * 50)-50)) && moveY > 0 = y + moveY
       | otherwise = y + moveY

    z' | x' > x = z + 1
       | x' < x = z - 1
       | y' > y = z + boxes
       | y' < y = z - boxes
       | otherwise  = z

-- tar 0, lista med colors, candyLocations (boxes (200,-200) [(((200,-200),0),white)]

createCandy :: Int -> [Color] -> [(Float,Float)] -> [Candy]
createCandy _ _ [] = []
--createCandy _ _ _  = []
createCandy int colors positions =
           [(((head positions),(int+1)),(head colors))] ++ createCandy (int+1) (tail colors) (tail positions) 


   --    | a > 200 && b < (-((boxes*100)-400)) = []
   --    | otherwise = [(((a,-200),(acc+1)),] ++ createCandy....

render' :: [Candy] -> Picture
render' candies = undefined



paintCandy :: [(Float,Float)] -> [Int] -> [Picture]
paintCandy [] col = []
paintCandy ((a,b):xs) col = [Color (getColor(head(col))) $ translate a b $ rectangleSolid 50 50] ++ (paintCandy  xs (tail(col)))


 
candyLocations :: Float -> (Float, Float)-> [(Float,Float)]
candyLocations 0  (a,b)
  | a > 200 && b < (-((boxes*100)-400)) = []
  | otherwise = candyLocations boxes ((-200), b-100)
 
candyLocations int (a,b) = [(a-(((boxes*100)-500) / 2),b+(((boxes*100)-500) / 2))] ++ candyLocations (int-1) (a+100,b)


-- Do nothing for all other events.  
mkMarker :: Color -> (Float, Float) -> Picture
mkMarker col (x,y) = pictures
  [ translate x y $ color white $ lineLoop $ rectanglePath 100 100]

paddleColor = light (light blue)


c :: Int -> IO([Int])
c n = replicateM n $ randomRIO (1,4)

recur :: [Int] -> [Color]
recur [] = []
recur list = [getColor (head list)] ++ recur (tail list)


getColor :: Int -> Color
getColor n | n == 1 = red
           | n == 2 = white
           | n == 3 = blue
           | otherwise = dark green



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
  | a > 200 && b < (-((boxes*100)-400)) = []
  | otherwise = squareLocations boxes ((-200), b-100)
 
squareLocations int (a,b) = [(a-(((boxes*100)-500) / 2),b+(((boxes*100)-500) / 2))] ++ squareLocations (int-1) (a+100,b)














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



