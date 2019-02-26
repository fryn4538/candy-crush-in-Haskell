
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
           
--data CandyBank = CandyBank 
--  {  
--    candyBank :: (((Float,Float),Int),Color)
--  } deriving Show

data Player = Player
  {  squareLoc :: (Float, Float),
     playerColor :: Color,
     squareIndex :: Float,
     candyBank :: [Candy],
     moveState :: Bool,
     gameState :: Int
  } deriving Show

-- In order to get random candies we chose a random element from a list.
-- Saved values --
width, height, offset :: Int
width = 501 -- Ändrade till 501 eftersom ramen runt är 1 px bred
height = 501
offset = 100

--PRE: boxes >= 5 or the game will crash

boxes :: Float
boxes = 5

boxesInt :: Int
boxesInt = 5

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
main = play
       window
       background
       fps
       initialState
       render
       handleKeys
       (const id) -- Löst så att rutan inte flyttas med (const id)
------------------------ Playfunktionens argument ----------------------------------------------
window :: Display
window =  FullScreen--InWindow "CrushTheCandy" (width, height) (offset, offset)

background :: Color
background = black

fps :: Int
fps = 60

--  Initialize the game with this game state.
initialState :: Player
initialState =  Player {
                       squareLoc = (((-((boxes*50)-50)),((boxes*50)-50))),
                       squareIndex = 1,
                       playerColor = white,
                       candyBank = checkHorizontalRows (createCandy 0 (randColorGen (unsafePerformIO (randListGen (boxesInt*boxesInt)))) (candyLocations boxes ((-200),200))),
                       moveState = False,
                       gameState = 2
                       }

--initialCandy :: CandyBank
--initialCandy = CandyBank {
--                     candyBank = createCandy 0 (randColorGen (unsafePerformIO (randL--istGen (boxesInt*boxesInt)))) (candyLocations boxes ((-200),200))  }

--  Draw a candy game state (convert it to a picture).
render :: Player -> Picture
render player
   | (gameState player) == 1 = pictures ([mkText] ++ (paintButtons (-300,-200) 5) ++ [(Color red $ translate 0 (-200) $ rectangleSolid 100 100)] ++ (squareTxt (-350,-300) 5))
   | (gameState player) == 2 = pictures ((paintRectangles (squareLocations boxes (-200,200))) ++ [mkMarker player $  squareLoc player] ++ (paintCandy $ candyBank player))


--  Draw a candy game state (convert it to a picture).
--render :: Player ->  Picture
--render player = pictures ((paintRectangles (squareLocations boxes (-200,200))) ++ [mkMarker rose $  squareLoc player] ++ (paintCandy $ createCandy 0 (randColorGen (unsafePerformIO (randListGen (boxesInt*boxesInt)))) (candyLocations boxes ((-200),200))))

--  Respond to key events.
handleKeys :: Event -> Player -> Player
--handleKeys (EventKey (Char '3') Down _ _) player = player { candyBank =  removeCandy (candyBank player)}
handleKeys (EventKey (Char '2') Down _ _) player = player {gameState = 2}
handleKeys (EventKey (Char '1') Down _ _) player = player {gameState = 1}


-- Lagt in gaurds för att vi ska kunna se om spelaren vill flytta eller
-- Upp = squareIndex - boxesInt
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) player
  | moveState player && (verifyMoveCandy 0 100 (squareIndex player))  = trace ("Your move is made") $ player { playerColor = white, moveState = False, candyBank = (moveCandy (squareIndex player) "Up" (candyBank player))}
  | otherwise = moveSquare 0 100 player

-- down = squareIndex - boxesInt
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) player
  | moveState player && (verifyMoveCandy 0 (-100) (squareIndex player)) = trace ("Your move is made") $ player { playerColor = white, moveState = False, candyBank = (moveCandy (squareIndex player) "Down" (candyBank player))}
  | otherwise = moveSquare 0 (-100) player

-- left = squareIndex - 1 
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) player
  | moveState player && (verifyMoveCandy (-100) 0 (squareIndex player))  = trace ("Your move is made") $ player { playerColor = white, moveState = False, candyBank = (moveCandy (squareIndex player) "Left" (candyBank player)) }
  | otherwise = moveSquare (-100) 0 player

-- left = squareIndex + 1
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) player
  | moveState player && (verifyMoveCandy 100 0 (squareIndex player))  = trace ("Your move is made") $  player { playerColor = white, moveState = False, candyBank = (moveCandy (squareIndex player) "Right" (candyBank player))}
  | otherwise = moveSquare 100 0 player
  
handleKeys (EventKey (SpecialKey KeyEnter) Down _ _) player
  | moveState player = player { playerColor = white, moveState = False}
  | otherwise = player {playerColor = violet, moveState = True}
  
handleKeys _ player = player


verifyMoveCandy :: Float -> Float -> Float -> Bool
verifyMoveCandy moveX moveY index   | mod (floor index) boxesInt == 1 && moveX < 0 =  False
                                    | mod (floor index) boxesInt == 0 && moveX > 0 =  False
                                    | index <= boxes && moveY > 0 = False
                                    | index >= ((boxes*boxes)-boxes) && moveY < 0 =  False
                                    | otherwise = True



moveCandy :: Float -> String -> [Candy] -> [Candy]
moveCandy index "Right" candyList =  checkHorizontalRows (playAux (floor (index)) (snd(candyList !! floor(index))) (snd(candyList !! (floor (index-1)))) "Right" candyList) 
moveCandy index "Left" candyList =   checkHorizontalRows (playAux (floor (index)) (snd(candyList !! floor(index-2))) (snd(candyList !! (floor (index-1)))) "Left" candyList) 
moveCandy index "Up" candyList =   checkHorizontalRows (playAux (floor index) (snd(candyList !! floor(index-(boxes+1)))) (snd(candyList !! (floor (index-1)))) "Up" candyList) 
moveCandy index "Down" candyList =   checkHorizontalRows (playAux (floor index) (snd(candyList !! floor(index+(boxes-1)))) (snd(candyList !! (floor (index-1)))) "Down" candyList) 


{-
handlePlayMove :: Float ->  Player -> Player
handlePlayMove index player = player { playerColor = violet} 
--playAux (floor(index)) (candyBank player) 
      -}                               --(snd ((candyBank player) !! (floor(index)))) }
                              
-- Returns same candy list with index ind color change
playAux :: Int -> Color -> Color -> String -> [Candy] -> [Candy]
playAux ind _ _ _[] = [] 
playAux ind color color2 direction ((((a,b),int),col):xs) 
       | int == ind =  [(((a,b),int),color)] ++ playAux ind color color2 direction xs
       | int == (ind-1) && direction == "Left" =  [(((a,b),int),color2)] ++ playAux ind color color2 "Empty" xs
       | int == (ind+1) && direction == "Right" =  [(((a,b),int),color2)] ++ playAux ind color color2 "Empty" xs
       | int == (ind-(boxesInt)) && direction == "Up"=  [(((a,b),int),color2)] ++ playAux ind color color2 "Empty" xs
       | int == (ind+(boxesInt)) && direction == "Down"=  [(((a,b),int),color2)] ++ playAux ind color color2 "Empty" xs
       | otherwise  = [(((a,b),int),col)] ++ playAux ind color color2 direction xs
-- snd(fst(fst (candybank !! index)))

--Takes the current gamestate and returns a new gamestate where candies that have been placed in a row are removed.
{-
checkIfInRow :: [Candy] -> Int -> [Candy]
checkIfInRow (listHead:[]) _ =  
checkIfInRow (listHead:listTail) acc
                                 | snd(listHead) == snd(head(listTail)) = checkIfInRow listTail (acc+1)
                                 | snd(listHead) /= snd(head(listTail)) && acc >= 3 = trace ("You got 3 in a row") $ checkIfInRow listTail 0
                                 | otherwise = checkIfInRow listTail 0
-}

checkHorizontalRows :: [Candy] -> [Candy]
checkHorizontalRows list = toColor (checkHorizontalRowsAux 1 1 0 [] list list)

toColor :: [Candy] -> [Candy]
toColor list = toColorAux [] list

toColorAux :: [Candy] -> [Candy] -> [Candy]
toColorAux list [] = list
toColorAux list ((((a,b),index),color):xs)
--  | color == black && index <= boxesInt = toColorAux ((take (index-1) list) ++ [(((a,b),index),(getColor (unsafePerformIO $ randomRIO (1,4))))] ++ (drop index list))
  | color == black && index <= boxesInt = trace ("Ändrar färg") $ toColorAux list ([(((a,b),index),(getColor (unsafePerformIO $ randomRIO (1,4))))] ++ xs)
  | color == black = trace ("Har inte färg") $ toColorAux [] (moveCandy (fromIntegral index) "Up" (list ++ ((((a,b),index),color):xs)))
  | otherwise = trace ("Har färg") $ toColorAux (list ++ [(((a,b),index),color)]) xs
  
  
--[([Float],Int)] Is a list of tuples containing a startIndex and how many candies come after it.
checkHorizontalRowsAux :: Float -> Float -> Int -> [(Float,Int)] -> [Candy] -> [Candy] -> [Candy]
checkHorizontalRowsAux index startIndex counter listOfRows unchanged ((((a,b),int),col):xs)
     --  | (int `mod` boxesInt) == 1 = checkIfInRows (index+1) startIndex 0 (((startIndex),(counter+1)):listOfRows) unchanged xs
      | index == (boxes*boxes) =  trace ("Calling makeblack with = " ++ show (reverse(listOfRows))) $ makeBlack (reverse(listOfRows)) unchanged
      | col == snd(head(xs)) && (int `mod` boxesInt) /= 0 &&  counter == 0 = checkHorizontalRowsAux (index+1) index (counter+1) listOfRows unchanged xs
      | col == snd(head(xs)) && (int `mod` boxesInt) /= 0 &&  counter > 0 = checkHorizontalRowsAux (index+1) startIndex (counter+1) listOfRows unchanged xs
      | col == snd(head(xs)) && (int `mod` boxesInt) == 0 &&  counter > 1 = checkHorizontalRowsAux (index+1) index 0 (((startIndex),(counter+1)):listOfRows) unchanged xs
      | col /= snd(head(xs)) && counter > 1 = trace ("Color changed will be " ++ show col) $ checkHorizontalRowsAux (index+1) index 0 (((startIndex),(counter+1)):listOfRows) unchanged xs
      | col /= snd(head(xs)) && counter == 0 = checkHorizontalRowsAux (index+1) startIndex 0 listOfRows unchanged xs
      | otherwise = checkHorizontalRowsAux (index+1) startIndex 0 listOfRows unchanged xs


--checkVerticalRowsAux :: Float -> Float -> Int -> [(Float,Int)] -> [Candy] -> [Candy] -> [Candy]
--checkVerticalRowsAux index startIndex counter listOfRows unchanged ((((a,b),int),col):xs)

makeBlack :: [(Float,Int)] -> [Candy] -> [Candy]
makeBlack _ [] = []
makeBlack [] ((((a,b),int),col):xs) = [] ++ ((((a,b),int),col):xs)
makeBlack ((startPoint,inRow):tail) ((((a,b),int),col):xs) -- = trace ("listOfRows = " ++ show listOfRows ) $ unchanged
     | ((startPoint,inRow):tail) == [] = xs
     | int >= (floor startPoint)  && int < ((floor startPoint) + inRow) = [(((a,b),int),black)] ++ makeBlack ((startPoint,inRow):tail) xs
     | int == ((floor startPoint) + inRow) = makeBlack tail ((((a,b),int),col):xs)
     | otherwise  = [(((a,b),int),col)] ++ makeBlack ((startPoint,inRow):tail) xs




removeCandy :: [Candy] -> [Candy]
removeCandy [] = []
removeCandy  ((((a,b),int),col):xs)
     | col ==  black && int <= boxesInt =trace ("make candy green on index: " ++ show int) ((((a,b),int),(dark green)):xs) ++ removeCandy xs
      -- col == black && int > boxesInt && (verifyMoveCandy 0 100 (fromIntegral int)) = trace ("moving candy at index: " ++ show int) $ (moveCandy (fromIntegral int) "Up" ((((a,b),int),col):xs))
      -- col == black = (moveCandy (fromIntegral int) "Up" ((((a,b),int),col):xs)) ++ removeCandy xs
     | otherwise = trace ("Ignore candy at index: " ++ show int) [(((a,b),int),col)] ++ removeCandy xs

{-
removeCandyAux :: [Candy] -> Float -> [Candy]
removeCandyAux ((((a,b),int),black):xs) index = (moveCandy (fromIntegral ) "Up" ((((a,b),int),col):xs)) 
-}
{-
checkIfInRows ::Float -> Int -> [Float] -> [Candy] -> [Candy] -> [Candy]
checkIfInRows index counter candiesInRow unchanged ((((a,b),int),col):xs:xss)
      | (int `mod` boxesInt) == 1 && index /= 1 && counter /= 0 = trace ("Counter = " ++ show counter ) $ checkIfInRows index 0 candiesInRow unchanged ((((a,b),int),col):xs:xss)
      | (xss) == [] = trace ("You reached step 1.3    'calling makeCandyBlack with'" ++ show candiesInRow) $  makeCandyBlack unchanged (reverse(candiesInRow))
      | col == snd(xs) && col == snd(head(xss)) && counter == 0 && (int `mod` boxesInt) <= 3 && (int `mod` boxesInt) /= 0  = trace ("this works 1, mod, int, boxes = " ++ show ((int `mod` boxesInt), (int, boxesInt))) $ checkIfInRows (index+2) (counter+2) ((index+2):(index+1):index:candiesInRow) unchanged xss
      | col == snd(xs) && col /= snd(head(xss)) &&  counter == 0 = trace ("this works 4") $ checkIfInRows (index+2) 0 candiesInRow unchanged xss
      | col == snd(xs) && counter < 5 && counter > 0 = trace ("this works 5") $ checkIfInRows (index+1) (counter+1) ((index+1):candiesInRow) unchanged xss
      | col == snd(xs) && counter /= 0 = trace ("this works 2") $ checkIfInRows (index+1) (counter+1) ((index+1):candiesInRow) unchanged (xs:xss)
      | col /= snd(xs) = trace ("Counter = " ++ show counter ) $ checkIfInRows (index+1) 0 candiesInRow unchanged (xs:xss)
      | otherwise = trace ("this works 3") $ unchanged





makeCandyBlack ::[Candy] -> [Float] -> [Candy]
makeCandyBlack ((((a,b),int),col):xs) [] = trace ("HEEEEEYYYYEYYEYEYEYYEYEYEYEY") $ [] ++ ((((a,b),int),col):xs)
makeCandyBlack  ((((a,b),int),col):xs)  candiesInRow
     | candiesInRow == [] = xs
     | int == floor(head(candiesInRow)) = trace ("made candy black" ++ show (int, candiesInRow)) $ [(((a,b),int),black)] ++ makeCandyBlack xs (tail(candiesInRow))
     | otherwise  = [(((a,b),int),col)] ++ makeCandyBlack xs candiesInRow


     | int == floor(head(candiesInRow)) && col /= black = trace ("You reached step 2.1") $ makeCandyBlack candyList ((((a,b),int),black):xs) candiesInRow
     | int == floor(head(candiesInRow)) && col == black = trace ("You reached step 2.2   'calling makeCandyBlack with'" ++ show candiesInRow) $ makeCandyBlack candyList xs (tail(candiesInRow))
     | int /= floor(head(candiesInRow)) = trace ("You reached step 2.3") $ makeCandyBlack candyList xs candiesInRow
     -}
--int == ind =  [(((a,b),int),color)] ++ playAux ind color color2 direction xs
--trace ("You reached") $

        
-------------------------------------------------------------------------------------------------


moveSquare :: Float
            -> Float
            -> Player -- The initial game state
            -> Player -- A new game state with an updated square position
moveSquare moveX moveY player = trace ("z' = " ++ show z') $ player { squareLoc = (x', y'), squareIndex =  z', moveState = False, playerColor = white}
  where
    -- Old locations and velocities.
    
    (x, y) = squareLoc player
    z = squareIndex player
    bank = candyBank player
    

    z' | y >= ((boxes * 50)-50) && moveY > 0 = z
       | y <= ((-(boxes * 50)+50)) && moveY < 0 = z
       | x >= ((boxes * 50)-50) && moveX > 0 = z
       | x <= ((-(boxes * 50)+50)) && moveX < 0 = z
       | moveX /= 0 = (z + (moveX/100))
       | moveY /= 0 = (z + (((moveY/100)*boxes))*(-1))
       | otherwise = z
       
    -- New locations. -- Nu kan man inte gå ut ur fönstret.
    x' | x >= ((boxes * 50)-50) && moveX >= 0 = x
       | x >= ((boxes * 50)-50) && moveX < 0 = updateLocationX bank (floor(z')-1)
       | x <= (-((boxes * 50)-50)) && moveX <= 0 = x
       | x <= (-((boxes * 50)-50)) && moveX > 0 = updateLocationX  bank (floor(z')-1)
   --    | x == 1000 = selectCandy 
       | otherwise = updateLocationX bank (floor(z')-1)
       
   -- x' = x - move
    y' | y >= ((boxes * 50)-50) && moveY >= 0 = y
       | y >= ((boxes * 50)-50) && moveY < 0 = updateLocationY bank (floor(z')-1)
       | y <= (-((boxes * 50)-50)) && moveY <= 0 = y
       | y <= (-((boxes * 50)-50)) && moveY > 0 = updateLocationY bank (floor(z')-1)
       | otherwise = updateLocationY bank (floor(z')-1)


--moveSquareAux :: Candy -> Player
--moveSquareAux (((a,b),int),col) = Player {squareLoc = (a,b), squareIndex = int}
updateLocationX :: [Candy] -> Int -> Float
updateLocationX bank z = fst(fst(fst( bank !! z)))

updateLocationY :: [Candy] -> Int -> Float
updateLocationY bank z = snd(fst(fst( bank !! z)))

--moveSquareAux :: Candy -> Player
--moveSquareAux (((a,b),int),col) = Player {squareLoc = (a,b), squareIndex = int}
--type Candy = (((Float,Float),Int),Color)
--updateLocation
    

-- tar 0, lista med colors, candyLocations (boxes (200,-200) [(((200,-200),0),white)]

createCandy :: Int -> [Color] -> [(Float,Float)] -> [Candy]
createCandy _ _ [] = []
createCandy int colors positions =
           [(((head positions),(int+1)),(head colors))] ++ createCandy (int+1) (tail colors) (tail positions) 


   --    | a > 200 && b < (-((boxes*100)-400)) = []
   --    | otherwise = [(((a,-200),(acc+1)),] ++ createCandy....



--type Candy = (((Float,Float),Int),Color)
paintCandy :: [Candy] -> [Picture]
paintCandy [] = []
paintCandy ((((a,b),int),col):xs) = [Color col $ translate a b $ rectangleSolid 50 50] ++ (paintCandy  xs)
{-

paintCandy :: [(Float,Float)] -> [Int] -> [Picture]
paintCandy [] col = []
paintCandy ((a,b):xs) col = [Color (getColor(head(col))) $ translate a b $ rectangleSolid 50 50] ++ (paintCandy  xs (tail(col)))


-}


candyLocations :: Float -> (Float, Float)-> [(Float,Float)]
candyLocations 0  (a,b)
  | a > 200 && b < (-((boxes*100)-400)) = []
  | otherwise = candyLocations boxes ((-200), b-100)
 
candyLocations int (a,b) = [(a-(((boxes*100)-500) / 2),b+(((boxes*100)-500) / 2))] ++ candyLocations (int-1) (a+100,b)


-- Do nothing for all other events.  
mkMarker :: Player -> (Float, Float) -> Picture
mkMarker player (x,y) = pictures
  [ translate x y $ color (playerColor player) $ lineLoop $ rectanglePath 100 100]

--mkMarker :: Color -> (Float, Float) -> Picture
--mkMarker col (x,y) = pictures
  --[ translate x y $ color white $ lineLoop $ rectanglePath 100 100]


randListGen :: Int -> IO([Int])
randListGen n = replicateM n $ randomRIO (1,4)

randColorGen :: [Int] -> [Color]
randColorGen [] = []
randColorGen list = [getColor (head list)] ++ randColorGen (tail list)


getColor :: Int -> Color
getColor n
  | n == 9 = black
  | n == 1 = red
  | n == 2 = white
  | n == 3 = blue
  | otherwise = yellow



--main = (InWindow "Nice Window" (200, 200) (10, 10)) white (Circle 80)
--rectangleWire :: Float -> Float -> Picture
--rectangleWire sizeX sizeY = lineLoop $ rectanglePath 100 100
rectangleWire1 =  translate (-200) 200 $ lineLoop $ rectanglePath 100 100
rectangleWire2 =  rectangleWire 200 100


paintRectangles :: [(Float,Float)] -> [Picture]
paintRectangles [] = []
paintRectangles ((a,b):xs) = [Color red $ translate a b $ lineLoop $ rectanglePath 100 100] ++  paintRectangles xs

{-
PRE: Börjar på (-200,200)
-}
squareLocations :: Float -> (Float, Float)-> [(Float,Float)]
squareLocations 0  (a,b)
  | a > 200 && b < (-((boxes*100)-400)) = []
  | otherwise = squareLocations boxes ((-200), b-100)
 
squareLocations int (a,b) = [(a-(((boxes*100)-500) / 2),b+(((boxes*100)-500) / 2))] ++ squareLocations (int-1) (a+100,b)




{-

-}
paintButtons :: (Float, Float) -> Int -> [Picture]
paintButtons (a, b) counter
  | counter <= 0 = []
  | otherwise = [Color green $ translate a b $ rectangleSolid 100 100] ++ paintButtons (a+150, b) (counter-1)

mkText :: Picture
mkText = (Color green $ translate (-525) 200 $  text "Crush the Candy")


squareTxt ::  (Float, Float) -> Int -> [Picture]
squareTxt (x,y) a
  | a >= 9 = [(Color white $ translate x y $ text (show a ++ "x" ++ show a))]
  | otherwise = [(Color white $ translate x y $ text (show a ++ "x" ++ show a))] ++ squareTxt (x+150,y) (a+1)