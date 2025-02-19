-- By Fredrik Yngve, August Bredberg, Erik Junghahn and Markus Winberg
module Main where
import System.IO.Unsafe
import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Picture
import System.Random
import Control.Monad
import Test.HUnit
import Lib


{-
 -- (Float, Float) is the candy's coordinates
 -- Int is the 'spot' of the candy on the playing field
 -- Color is the candy's color
-}
type Candy = (((Float,Float),Int),Color)
          
         
{- Description of every attribute

     squareLoc : The current coordinates of the player, in the form of two floats.
     playerColor :: Represents the color of the player.
     squareIndex :: Represents the current index of the player (squareIndex = 1 means that the player is currently sat on the top left square).
     candyBank :: Represents the current constellation of candies. In other words, this is a list of every candy on screen, in order.
     moveState :: Either True or False. While true, the player is in a moveState. While in a moveState, the players next move switches two candies (if move is valid).
     gameState :: Represents what the function render renders on screen. 1 = startScreen, 2 = game, 3 = gameover.
     colorBank :: 10 000 random colors, generated at the start of the game. When new candies are generated after a valid move, their colors are chosen from the first elements of colorBank.
     score :: The players current score. Increases by the number of candies crushed every move. 
     time :: How much time left until gameover.

  INVARIANTS
     squareLoc :: Within the borders of the gameboard
     playerColor :: Either white or violet
     squareIndex :: Somewhere between 1 and gridsize
     candyBank :: Length is gridsize
     moveState :: True or False
     gameState :: 1, 2 or 3
     colorBank :: Length never exceeds 10 000
     score :: >= 0
     time :: >= 0

-}
data Player = Player
  {  squareLoc :: (Float, Float),
     playerColor :: Color,
     squareIndex :: Float,
     candyBank :: [Candy],
     moveState :: Bool,
     gameState :: Int,
     colorBank :: [Color],
     score :: Int,
     time :: Int
  } deriving Show





{- boxes
   Holds the number of rows on the gameboard.
   RETURNS: the number of rows on the gameboard.
   EXAMPLES: boxes -> 8
-}
boxes :: Float
boxes = 8

{- boxesInt
   Converts boxes into an Int.
   RETURNS: boxes as Int.
   EXAMPLES: boxesInt -> 8
-}
boxesInt :: Int
boxesInt = round boxes


{- main
   A gameloop that updates the game after every move.
   Returns: This function returns all the different values given by its called functions. (Windows, background, etc.)
-}
main :: IO ()
main = play
       window
       background
       fps
       initState
       render
       handleKeys
       timer
       
{- window
   Tells the main function to run in fullscreen.
   RETURNS: how the main function shoud display its output.
   EXAMPLES: window -> Fullscreen
-}
window :: Display
window = FullScreen

{- background
   Tells the main function the color of the background.
   RETURNS: the background color.
   EXAMPLES: background -> RGBA 0.0 0.0 0.0 1.0
-}
background :: Color
background = black

{- fps
   Tells the main function what fps it should operate at.
   RETURNS: the fps.
   EXAMPLES: fps -> 1
-}
fps :: Int
fps = 1

--  Initialize the game with this game state.
{- initState
   Tells the main function the initial state of the game.
   RETURNS: the initial state of the game.
   SIDE EFFECT: Gives the attributes in Player their initial values.
   Examples initState -> Player {squareLoc = (-350.0,350.0), squareIndex = 1.0, playerColor = RGBA 1.0 1.0 1.0 1.0, colorBank = [RGBA 1.0 0.0 0.0 1.0..RGBA 1.0 0.0 1.0 1.0], candyBank = [(((100.0,100.0),1),RGBA 0.0 1.0 0.0 1.0)..(((100.0,100.0),64),RGBA 0.0 1.0 0.0 1.0)], moveState = False, gameState = 2, score = 0, time = 120}
-}
initState :: Player
initState =  Player {squareLoc   = (((-((boxes*50)-50)),((boxes*50)-50))),
                     squareIndex = 1,
                     playerColor = white,
                     colorBank   = (randColorGen (unsafePerformIO (randListGen (10000)))),
                     candyBank   = refill (checkRows (createCandy 0 (randColorGen (unsafePerformIO (randListGen (boxesInt*boxesInt)))) (candyLocations boxes ((-200),200))) 12) (randColorGen (unsafePerformIO (randListGen (100)))),        
                     moveState   = False,
                     gameState   = 1,
                     score       = 0,
                     time        = 120}

{- render player
   Generates the graphics of the game.
   RETURNS: A Picture of the current gamestate with values extracted from player.
-}
render :: Player -> Picture
render player
   | (gameState player) == 1 = pictures ([(Color green $ translate (-380) 0 $ text ("Candy Break"))] ++
                                         [(Color green $ translate (-260) (-200) $ scale 0.4 0.4 $ text ("Press ENTER to play"))])

   | (gameState player) == 2 = pictures ((paintRectangles (squareLocations boxes (-200,200))) ++
                                         [mkMarker player $  squareLoc player] ++ (paintCandy $ candyBank player) ++
                                         [Color green $ (translate (-700) 0 $ scoreDisp player)] ++
                                         [Color green $ translate 500 0 $ showTime player] ++
                                         [(Color green $ translate (-190) (-450) $ scale 0.3 0.3 $ text ("Press M for menu"))])

   | (gameState player) == 3 = pictures ([(Color green $ translate (-580) 0 $ text ("Your score was: " ++ show (score player)))] ++
                                         [(Color green $ translate (-340) (-200) $ scale 0.4 0.4 $ text ("Press ENTER to play again"))] ++
                                         [(Color green $ translate (-240) (-300) $ scale 0.4 0.4 $ text ("Press ESC to exit"))])

{- HandleKeys Event
     Receives a keystate and performs a functioncall depending on what key is pressed in what way.
     RETURNS: a new gamestate depending on wich key is pressed.
     Keyup: Returns a gamestate where player position in candyList is changed to be n less where n is the width of the playing field.
     KeyDown: Returns a gamestate where player position in candyList is changed to be n more where n is the width of the playing field.
     KeyRight: Returns a gamestate where player position in candyList is changed to be 1 more.
     KeyLeft: Returns a gamestate where player position in candyList is changed to be 1 less.
     SIDE EFFECTS: Updates the gamestate to a new gamestate that is printed out on the screen where the values of various player attributes may have changed depending on the key pressed.
     EXAMPLES: handleKeys (EventKey (Char 'm') Down _ _) player == player {gameState = 1, score = 0, time = 120}
-}   
handleKeys :: Event -> Player -> Player
handleKeys (EventKey (Char 'm') Down _ _) player = player {gameState = 1, score = 0, time = 120}

handleKeys (EventKey (SpecialKey KeyUp) Down _ _) player
  | (gameState player) == 1 = player
  | moveState player && (verifyMoveCandy 0 100 (squareIndex player)) && not(verifySwapCandy (squareIndex player) (candyBank player) "Up")  = player { playerColor = white, colorBank = (drop 10 (colorBank player)), moveState = False, candyBank = refill (moveCandy (squareIndex player) "Up" (candyBank player)) (colorBank player), score = ((score player) + (countBlack (moveCandy (squareIndex player) "Up" (candyBank player)) 0))}
  | otherwise = moveSquare 0 100 player

handleKeys (EventKey (SpecialKey KeyDown) Down _ _) player
  | (gameState player) == 1 = player
  | moveState player && (verifyMoveCandy 0 (-100) (squareIndex player)) && not(verifySwapCandy (squareIndex player) (candyBank player) "Down")  = player { playerColor = white, colorBank = (drop 10 (colorBank player)), moveState = False, candyBank = refill (moveCandy (squareIndex player) "Down" (candyBank player)) (colorBank player), score = ((score player) + (countBlack (moveCandy (squareIndex player) "Down" (candyBank player)) 0))}
  | otherwise = moveSquare 0 (-100) player

handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) player
  | (gameState player) == 1 = player
  | moveState player && (verifyMoveCandy (-100) 0 (squareIndex player)) && not(verifySwapCandy (squareIndex player) (candyBank player) "Left")  = player { playerColor = white, colorBank = (drop 10 (colorBank player)), moveState = False, candyBank = refill (moveCandy (squareIndex player) "Left" (candyBank player)) (colorBank player), score = ((score player) + (countBlack (moveCandy (squareIndex player) "Left" (candyBank player)) 0))}
  | otherwise = moveSquare (-100) 0 player

handleKeys (EventKey (SpecialKey KeyRight) Down _ _) player
  | (gameState player) == 1 = player
  | moveState player && (verifyMoveCandy 100 0 (squareIndex player)) && not(verifySwapCandy (squareIndex player) (candyBank player) "Right")  = player { playerColor = white, colorBank = (drop 10 (colorBank player)), moveState = False, candyBank = refill (moveCandy (squareIndex player) "Right" (candyBank player)) (colorBank player), score = ((score player) + (countBlack (moveCandy (squareIndex player) "Right" (candyBank player)) 0))}
  | otherwise = moveSquare 100 0 player
  
handleKeys (EventKey (SpecialKey KeyEnter) Down _ _) player
  | (gameState player) == 1 || (gameState player) == 3 = player {squareLoc = (((-((boxes*50)-50)),((boxes*50)-50))),
                                         squareIndex = 1,
                                         playerColor = white,
                                          colorBank = (randColorGen (unsafePerformIO (randListGen (10000)))),
                                          moveState = False,
                                          gameState = 2,
                                          score = 0,
                                          time = 120}
  | moveState player = player { playerColor = white, moveState = False}
  | otherwise = player {playerColor = violet, moveState = True}
  
handleKeys _ player = player


 {- verifyMoveCandy moveX moveY index
     If a player tries to swap a candy this function checks wether or not there is a candy to swap with). If we are located on the upper row of the gameboard and try swap up it returns false.
     RETURNS: A Boolean, true or False
     EXAMPLES: 
     verifyMoveCandy 100 0 1  = False
     verifyMoveCandy 100 0 10 = True
  -}
verifyMoveCandy :: Float -> Float -> Float -> Bool
verifyMoveCandy moveX moveY index   | mod (floor index) boxesInt == 1 && moveX < 0 =  False
                                    | mod (floor index) boxesInt == 0 && moveX > 0 =  False
                                    | index <= boxes && moveY > 0 = False
                                    | index > ((boxes*boxes)-boxes) && moveY < 0 =  False
                                    | otherwise = True

 {- verifySwapCandy index candyList string
     If a player tries to swap a candy, this function checks wether or not the swap results in at least one row with 3 or more candies of the same type.
     RETURNS: A Boolean, True or False
     EXAMPLES:
     verifySwapCandy 10 (candyBank player) "Right" == False (This result is given if the moveCandy function finds any rows of 3 or more candies. Otherwise True)
  -}
                                   
verifySwapCandy :: Float -> [Candy] -> String -> Bool
verifySwapCandy index candyList "Up" = moveCandy index "Up" candyList == (moveCandyAux (floor index) (snd(candyList !! floor(index-(boxes+1)))) (snd(candyList !! (floor (index-1)))) "Up" candyList) 
verifySwapCandy index candyList "Down" = moveCandy index "Down" candyList == (moveCandyAux (floor index) (snd(candyList !! floor(index+(boxes-1)))) (snd(candyList !! (floor (index-1)))) "Down" candyList) 
verifySwapCandy index candyList "Right" = moveCandy index "Right" candyList == (moveCandyAux (floor (index)) (snd(candyList !! floor(index))) (snd(candyList !! (floor (index-1)))) "Right" candyList) 
verifySwapCandy index candyList "Left" = moveCandy index "Left" candyList  == (moveCandyAux (floor (index)) (snd(candyList !! floor(index-2))) (snd(candyList !! (floor (index-1)))) "Left" candyList) 


 {- checkRows list n
     Recursively calls the function checkHorizontalRows n times in order to make all possible created rows black
     PRE: n > -1
     RETURNS: a new list of Candy where candies in rows of 3 or more have their colors changed to black.
     EXAMPLES: checkRows [green, green, green, red, blue, green, pink, pink, green] 2 == [black, black, black, red, blue, black, pink, pink, black] 
  -}
checkRows :: [Candy] -> Int -> [Candy]
checkRows list n
       | n > 0 =  checkRows (checkHorizontalRows 1 1 0 [] list list) (n-1)
       | otherwise = checkHorizontalRows 1 1 0 [] list list

--[((Float,Int),String)] Is a list of tuples containing a startIndex and how many candies come after it. The string tells if the row is vertical or horizontal.
{-  checkHorizontalRows index startIndex counter listOfRows unchanged candyList
     Checks if there are any candies in a horizontal row and calls checkVerticalRows with all rows found.
     VARIANT: index
     RETURNS: A new list of candies with one row of candies made black.
     EXAMPLES:
checkHorizontalRows 1 1 0 [] [green, green, green, red, blue, green, pink, pink, green] [green, green, green, red, blue, green, pink, pink, green] 2 == [black, black, black, red, blue, green, pink, pink, green]
-}       
checkHorizontalRows :: Float -> Float -> Int -> [((Float,Int),String)] -> [Candy] -> [Candy] -> [Candy]
checkHorizontalRows index startIndex counter listOfRows unchanged ((((a,b),candyIndex),color):xs)
      | index == (boxes*boxes) && color == snd((((a,b),candyIndex),color)) && (candyIndex `mod` boxesInt) == 0 &&  counter > 1 && color /= black  = checkVerticalRows 1 1 0 1 (((startIndex,(counter+1)),"H"):listOfRows) unchanged
      | index == (boxes*boxes) = checkVerticalRows 1 1 0 1 listOfRows unchanged
      | color == snd(head(xs)) && (candyIndex `mod` boxesInt) /= 0 &&  counter == 0 = checkHorizontalRows (index+1) index (counter+1) listOfRows unchanged xs
      | color == snd(head(xs)) && (candyIndex `mod` boxesInt) /= 0 &&  counter > 0 && color /= black = checkHorizontalRows (index+1) startIndex (counter+1) listOfRows unchanged xs
      | color == snd(head(xs)) && (candyIndex `mod` boxesInt) == 0 &&  counter > 1 && color /= black = checkHorizontalRows (index+1) index 0 (((startIndex,(counter+1)),"H"):listOfRows) unchanged xs
      | color /= snd(head(xs)) && counter > 1 = checkHorizontalRows (index+1) index 0 ((((startIndex),(counter+1)),"H"):listOfRows) unchanged xs
      | color /= snd(head(xs)) && counter == 0 = checkHorizontalRows (index+1) startIndex 0 listOfRows unchanged xs
      | otherwise = checkHorizontalRows (index+1) startIndex 0 listOfRows unchanged xs


{-  checkVerticalRows index startIndex counter row listOfRows unchanged 
     Checks if there are any candies in a vertical row and calls makeBlackV with all rows found in both checkHorizontalRows and checkVerticalRows.
     VARIANT: index
     RETURNS: A new list of candies where the first row of candies that is found is made black.
     EXAMPLES:
checkVerticalRows 1 1 0 1 [((1.0,3),"H")] [green, green, green, red, blue, red, pink, pink, green] == [black, black, black, red, blue, green, pink, pink, green] 
  -}

checkVerticalRows :: Float -> Float -> Int -> Float -> [((Float,Int),String)] -> [Candy]  -> [Candy]
checkVerticalRows index startIndex counter row listOfRows unchanged
      | index == (boxes*boxes) && snd(unchanged !! (floor (index-(boxes+1)))) == snd(unchanged !! ((floor (index-1)))) && counter > 1  =  makeBlackV (((startIndex,(counter+1)),"V"):listOfRows) 0 unchanged
      | index == (boxes*boxes) = makeBlackV listOfRows 0 unchanged 
      | indexCheck && counter <= 1 = checkVerticalRows (row+1) startIndex 0 (row+1) listOfRows unchanged
      | indexCheck && counter > 1  = checkVerticalRows (row+1) startIndex 0 (row+1) (((startIndex,(counter+1)),"V"):listOfRows) unchanged
      | colorCheck && counter == 0 && blackCheck = checkVerticalRows (index+boxes) index (counter+1) row listOfRows unchanged
      | colorCheck && counter >  0 && blackCheck = checkVerticalRows (index+boxes) startIndex (counter+1) row listOfRows unchanged
      |snd(unchanged !! (floor (index-1))) /= snd(unchanged !! ((floor (index-1)) + boxesInt)) && counter > 1 =checkVerticalRows (index+boxes) 0 0 row (((startIndex,(counter+1)),"V"):listOfRows) unchanged
      | snd(unchanged !! (floor (index-1))) /= snd(unchanged !! ((floor (index-1)) + boxesInt)) && counter <= 1 =checkVerticalRows (index+boxes) 0 0 row listOfRows unchanged
      | otherwise = checkVerticalRows (index+boxes) 0 0 row listOfRows unchanged
      where
        indexCheck = (index > (boxes*(boxes-1)))
        colorCheck = (snd(unchanged !! (floor (index-1))) == snd(unchanged !! ((floor (index-1)) + boxesInt)))
        blackCheck = (snd(unchanged !! (floor (index-1))) /= black)


{- moveCandy index string candyList
    The function swaps the color of a candy with the color of an adjacent candy depending on the value of the given string. And with the help of checkRows figures out if any new rows have appeared.
    RETURNS:  A new list of candies where each row of 3 or more of the same candies have their color set to black.
    EXAMPLES: moveCandy 5 "Up" [green, blue, green, blue, green, green, pink, pink, green] == [black, black, black, blue, blue, green, pink, pink, green]
-}
moveCandy :: Float -> String -> [Candy] -> [Candy]
moveCandy index "Right" candyList =  checkRows(moveCandyAux (floor (index)) (snd(candyList !! floor(index))) (snd(candyList !! (floor (index-1)))) "Right" candyList) 4
moveCandy index "Left" candyList =  checkRows(moveCandyAux (floor (index)) (snd(candyList !! floor(index-2))) (snd(candyList !! (floor (index-1)))) "Left" candyList) 4
moveCandy index "Up" candyList =  checkRows(moveCandyAux (floor index) (snd(candyList !! floor(index-(boxes+1)))) (snd(candyList !! (floor (index-1)))) "Up" candyList) 4
moveCandy index "Down" candyList =  checkRows(moveCandyAux (floor index) (snd(candyList !! floor(index+(boxes-1)))) (snd(candyList !! (floor (index-1)))) "Down" candyList) 4


{- moveCandyAux index color color2 direction candyList
   Performs the swap between the colors of two adjacent candies.
   RETURNS: A new candylist where the color value at index a and b have switched.
   VARIANT: length candyList
   EXAMPLES: moveCandyAux 5 green red "Up" [green, blue, green, blue, green, green, pink, pink, green] == [black, black, black, blue, blue, green, pink, pink, green]
-}
moveCandyAux :: Int -> Color -> Color -> String -> [Candy] -> [Candy]
moveCandyAux ind _ _ _[] = [] 
moveCandyAux ind color1 color2 direction ((((a,b),candyIndex),col):xs) 
       | candyIndex == ind =  [(((a,b),candyIndex),color1)] ++ moveCandyAux ind color1 color2 direction xs
       | candyIndex == (ind-1) && direction == "Left" =  [(((a,b),candyIndex),color2)] ++ moveCandyAux ind color1 color2 "Empty" xs
       | candyIndex == (ind+1) && direction == "Right" =  [(((a,b),candyIndex),color2)] ++ moveCandyAux ind color1 color2 "Empty" xs
       | candyIndex == (ind-(boxesInt)) && direction == "Up"=  [(((a,b),candyIndex),color2)] ++ moveCandyAux ind color1 color2 "Empty" xs
       | candyIndex == (ind+(boxesInt)) && direction == "Down"=  [(((a,b),candyIndex),color2)] ++ moveCandyAux ind color1 color2 "Empty" xs
       | otherwise  = [(((a,b),candyIndex),col)] ++ moveCandyAux ind color1 color2 direction xs





{- refill list colorBank
   moves blacked out spots to the top and fills them with colors from the colorBank
   RETURNS: A new candylist where all the blacked out spots have been moved to the top and filled with colors
   EXAMPLES: mkAllCol [green, green, yellow, red, blue, pink, black, black, black] colorBank = [pink, red, green, green, green, yellow, red, blue, pink]
-}


                     
refill :: [Candy] -> [Color] -> [Candy]
refill list colorBank = refillAux list [] colorBank

{- refillAux list temp colors
   moves blacked out spots to the top and fills them with colors
   VARIANT: length list
   RETURNS: A new candylist where all the blacked out spots have been moved to the top and filled with colors
   EXAMPLES: refillAux [green, green, yellow, red, blue, pink, black, black, black] [] [pink, blue, blue] = [pink, blue, blue, green, green, yellow, red, blue, pink]
-}

refillAux :: [Candy] -> [Candy]-> [Color] -> [Candy]
refillAux [] baseList _ = baseList
refillAux list baseList colors
  | snd (head list) == black =  refillAux (checkRows (recolor (moveBlack (checkRows (baseList ++ list) 1)) colors) 1) [] (tail colors)
  | otherwise = refillAux (tail list) (baseList ++ [(head list)]) colors

{- recolor list colors
   Recolors all blacked out candies
   VARIANT: length list
   RETURNS: A new candylist where all the blacked out spots have been filled with colored candies.
   EXAMPLES: recolor [black, black, black, red, blue, pink, red, green, red] colorBank = [pink, blue, blue, red, blue, pink, red, green, red]
-}

recolor :: [Candy] -> [Color] ->[Candy]
recolor [] _ = [] 
recolor ((((a,b),int),col):xs) (c:cs)
  | col == black = [(((a,b),int),c)] ++ recolor xs cs
  | otherwise = [(((a,b),int),col)] ++ recolor xs (c:cs)

{- moveBlack list
    Calls moveBlackAux.
    RETURNS: A new candylist with all black squares moved to the top row as far as possible.              .
    EXAMPLE: moveBlack [red, green, pink, black, black, black, blue, blue, yellow] = [black, black, black, red, green, pink, blue, blue, yellow]
-}

moveBlack :: [Candy] -> [Candy]
moveBlack list = (moveBlackAux list (boxesInt) (boxesInt + 1))

{- moveBlackAux list n m
   Moves black squares to the top
   VARIANT: n
   RETURNS: A new candylist with all black squares moved to the top row as far as possible.              .
   EXAMPLE: moveBlackAux 3 4 [red, green, pink, black, black, black, blue, blue, yellow] = [black, black, black, red, green, pink, blue, blue, yellow] 
-}
moveBlackAux :: [Candy] -> Int -> Int -> [Candy]
moveBlackAux list n m
  | n <= 0 =  list
  | m > ((boxesInt * boxesInt)) = moveBlackAux list (n-1) (boxesInt + 1)
  | (snd (list !! (m-1))) == black = trace ("m" ++ show m ++ " n" ++ show n) $ moveBlackAux (moveCandy (fromIntegral m) "Up" list) n (m+1)
  | otherwise = trace ("m" ++ show m ++ " n" ++ show n) $  moveBlackAux list n (m+1)

{- makeBlackV rows counter list
   Makes specified vertical row black.
   VARIANT: (length list)
   RETURNS: A new candylist with the row blacked out.
   EXAMPLE: makeBlackV [((1.0,3),"V")] 0 [red, green, pink, red, blue, red, red, blue, yellow] == [black, green, pink, black, blue, red, black, blue, yellow]
-}
makeBlackV :: [((Float,Int),String)] -> Int -> [Candy] -> [Candy]
makeBlackV _ _ [] = []
makeBlackV [] _ ((((a,b),int),col):xs) = ((((a,b),int),col):xs)
makeBlackV (((startPoint,inRow),"H"):tail) counter ((((a,b),int),col):xs) = makeBlackH (((startPoint,inRow),"H"):tail) ((((a,b),int),col):xs)
makeBlackV (((startPoint,inRow),"V"):tail) counter ((((a,b),int),col):xs)
    | counter >= inRow = makeBlackV tail 0 ((((a,b),int),col):xs)
    | int == ((floor startPoint) + (boxesInt * counter)) = [(((a,b),int),black)] ++ makeBlackV (((startPoint,inRow),"V"):tail) (counter+1) xs
    | otherwise  = [(((a,b),int),col)] ++ makeBlackV (((startPoint,inRow),"V"):tail) counter xs

{- makeBlackH rows list
   Makes specified horizontal row black.
   VARIANT: length list
   RETURNS: A new candylist with the row blacked out.
   EXAMPLE: makeBlackV [((1.0,3),"H")] [green, green, green, red, blue, red, red, blue, yellow] == [black, black, black, red, blue, red, red, blue, yellow]    
-}
makeBlackH :: [((Float,Int),String)] -> [Candy] -> [Candy]
makeBlackH _ [] = []
makeBlackH [] ((((a,b),int),col):xs) = [] ++ ((((a,b),int),col):xs)
makeBlackH (((startPoint,inRow),"V"):tail)  ((((a,b),int),col):xs) = trace ("V is first in list") $ makeBlackV (((startPoint,inRow),"V"):tail) 0 ((((a,b),int),col):xs)
makeBlackH (((startPoint,inRow),"H"):tail)  ((((a,b),int),col):xs) -- = trace ("listOfRows = " ++ show listOfRows ) $ unchanged
     | (((startPoint,inRow),"H"):tail) == [] = xs
     | int >= (floor startPoint)  && int < ((floor startPoint) + inRow) = [(((a,b),int),black)] ++ makeBlackH (((startPoint,inRow),"H"):tail) xs
     | int == ((floor startPoint) + inRow) = makeBlackH tail ((((a,b),int),col):xs)
     | otherwise  = [(((a,b),int),col)] ++ makeBlackH (((startPoint,inRow),"H"):tail) xs

{- moveSquare moveX moveY player
    Moves the players selected square. 
    RETURNS: A new location for the player.
    SIDE EFFECTS: creates a new gamestate with the new location for squareIndex that is then diplayed by main.
    EXAMPLE: moveSquare 100 0 (Player {squareLoc = (100,100),
                                  squareIndex = 1,                                    ---> 2
                                  playerColor = white,
                                  colorBank = (randColorGen (unsafePerformIO (randListGen (10000)))),
                                  candyBank =  [(((100,100),1),black)],         
                                  moveState = False,
                                  gameState = 2,
                                  score = 0})                  
-}
moveSquare :: Float
            -> Float
            -> Player -- The previous game state
            -> Player -- A new game state with an updated square position
moveSquare moveX moveY player = trace ("z' = " ++ show z') $ player { squareLoc = (x', y'), squareIndex =  z', moveState = False, playerColor = white}
  where
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

    x' | x >= ((boxes * 50)-50) && moveX >= 0 = x
       | x >= ((boxes * 50)-50) && moveX < 0 = updateLocationX bank (floor(z')-1)
       | x <= (-((boxes * 50)-50)) && moveX <= 0 = x
       | x <= (-((boxes * 50)-50)) && moveX > 0 = updateLocationX  bank (floor(z')-1)
       | otherwise = updateLocationX bank (floor(z')-1)
       

    y' | y >= ((boxes * 50)-50) && moveY >= 0 = y
       | y >= ((boxes * 50)-50) && moveY < 0 = updateLocationY bank (floor(z')-1)
       | y <= (-((boxes * 50)-50)) && moveY <= 0 = y
       | y <= (-((boxes * 50)-50)) && moveY > 0 = updateLocationY bank (floor(z')-1)
       | otherwise = updateLocationY bank (floor(z')-1)

{- updateLocationX bank z
   changes the x value of players location 
   RETURNS: a new location
   EXAMPLE: updateLocationX [(((-200,200),1),black), (((-100,200),2),red), (((0,200),3),blue), (((-100,200),4),yellow)] 2 -> 0.0
-}

updateLocationX :: [Candy] -> Int -> Float
updateLocationX bank z = fst(fst(fst( bank !! z)))

{- updateLocationY bank z
   changes the y value of players location
   RETURNS: a new location
   EXAMPLE: updateLocationY [(((-200,200),1),black), (((-100,200),2),red), (((0,200),3),blue), (((100,200),4),yellow)] 2 -> 200.0
-}
updateLocationY :: [Candy] -> Int -> Float
updateLocationY bank z = snd(fst(fst( bank !! z)))

{- createCandy int colors positions
   Populates the grid with colored squares.
   VARIANT: length positions
   RETURNS: A candylist
   EXAMPLE: createCandy 0 [red, blue, green, blue, blue, pink, yellow, yellow, red] ??? = [red, blue, green, blue, blue, pink, yellow, yellow, red] 
-}
createCandy :: Int -> [Color] -> [(Float,Float)] -> [Candy]
createCandy _ _ [] = []
createCandy int colors positions =
           [(((head positions),(int+1)),(head colors))] ++ createCandy (int+1) (tail colors) (tail positions) 



{- paintCandy list
    takes a candylist and generates colored squares
    RETURNS: a picture with the colored squares drawn out.
    VARIANT: length list
-}
paintCandy :: [Candy] -> [Picture]
paintCandy [] = []
paintCandy ((((a,b),int),col):xs) = [Color col $ translate a b $ rectangleSolid 50 50] ++ (paintCandy  xs)


{- candyLocations int (a,b)
    Computes the coordinates of each created square so the candies line up no matter what size the board is.
    RETURNS: A list of tuples containing the x- and y-coordinates of each candy.
    EXAMPLE: candyLocations 2 ((-200),200) == [((-200),200), ((-100),200), ((-200),100), ((-100),100)]
    VARIANT: num or a && b
-}
candyLocations :: Float -> (Float, Float)-> [(Float,Float)]
candyLocations 0  (a,b)
  | a > 350 && b < (-((boxes*100)-400)) = []
  | otherwise = candyLocations boxes ((-200), b-100)
candyLocations num (a,b) = [(a-(((boxes*100)-500) / 2),b+(((boxes*100)-500) / 2))] ++ candyLocations (num-1) (a+100,b)


{- mkMarker player (x,y)
    creates the marker that is used by the player to choose candies. Size, color and coordinates of this square is defined here.
    RETURNS: A Picture of a square with the size 100x100 at coordinates (x,y)
    EXAMPLE: mkMarker (Player {squareLoc = (100,100),          --->  (200,200)
                                  squareIndex = 1,
                                  playerColor = white,
                                  colorBank = (randColorGen (unsafePerformIO (randListGen (10000)))),
                                  candyBank =  [(((100,100),1),black)],         
                                  moveState = False,
                                  gameState = 2,
                                  score = 0})    (200,200)
-}
-- Do nothing for all other events.  
mkMarker :: Player -> (Float, Float) -> Picture
mkMarker player (x,y) = pictures
  [ translate x y $ color (playerColor player) $ lineLoop $ rectanglePath 100 100]

{- randListGen n
    Creates a list of random integers n long.
    RETURNS: a list of n random integers.
    EXAMPLE: randListGen 9 = [1, 6, 4, 3, 7, 3, 3, 4, 5]
-}
randListGen :: Int -> IO([Int])
randListGen n = replicateM n $ randomRIO (1,7)

{- randColorGen list
    Turns a list of integers into a list of colors.
    RETURNS: a list of colors.
    EXAMPLE: randColorGen [3, 7, 6, 1, 7, 3, 4, 1, 3] = [blue, yellow, dark green, red, yellow, blue, dark (dark violet), red, blue]
    VARIANT: length list
-}
randColorGen :: [Int] -> [Color]
randColorGen [] = []
randColorGen list = [getColor (head list)] ++ randColorGen (tail list)

{- getColor Int
    Turns an integer into a color.
    RETURNS: Returns a color.
    EXAMPLE: getColor 2 = white
-}
getColor :: Int -> Color
getColor n | n == 1 = red
           | n == 2 = white
           | n == 3 = (greyN 0.2)
           | n == 4 = dark (dark violet)
           | n == 5 = rose
           | n == 6 = dark green
           | otherwise = yellow



{-  paintRectangles list
    creates pictures that make up a grid of red rectangles
    RETURNS: A list of boxes^2 red rectangles.
    EXAMPLE: paintRectangles [((-200),200), ((-100),200)] == [Color (RGBA 1.0 0.0 0.0 1.0) (Translate (-200.0) 200.0 (Line [(-50.0,-50.0),(-50.0,50.0),(50.0,50.0),(50.0,-50.0),(-50.0,-50.0)])),Color (RGBA 1.0 0.0 0.0 1.0) (Translate (-100.0) 200.0 (Line [(-50.0,-50.0),(-50.0,50.0),(50.0,50.0),(50.0,-50.0),(-50.0,-50.0)]))]
    VARIANT: length list
-}


paintRectangles :: [(Float,Float)] -> [Picture]
paintRectangles [] = []
paintRectangles ((a,b):xs) = [Color red $ translate a b $ lineLoop $ rectanglePath 100 100] ++  paintRectangles xs

{- squareLocations num
Calculates the positions of the squares on the board and puts them in a list.
PRE: tuple of coordinates must be (-200,200)
VARIANT: a & b
RETURNS: a list of tuples containing each rectangles x- and y-coordinates
EXAMPLE: squareLocations 5 ((-200),200) == 
[(-200.0,200.0),(-100.0,200.0),(0.0,200.0),(100.0,200.0),(200.0,200.0),(-200.0,100.0),(-100.0,100.0),(0.0,100.0),(100.0,100.0),(200.0,100.0),(-200.0,0.0),(-100.0,0.0),(0.0,0.0),(100.0,0.0),(200.0,0.0),(-200.0,-100.0),(-100.0,-100.0),(0.0,-100.0),(100.0,-100.0),(200.0,-100.0),(-200.0,-200.0),(-100.0,-200.0),(0.0,-200.0),(100.0,-200.0),(200.0,-200.0)]

-}
squareLocations :: Float -> (Float, Float)-> [(Float,Float)]
squareLocations 0  (a,b)
  | a > 200 && b < (-((boxes*100)-400)) = []
  | otherwise = squareLocations boxes ((-200), b-100)
 
squareLocations num (a,b) = [(a-(((boxes*100)-500) / 2),b+(((boxes*100)-500) / 2))] ++ squareLocations (num-1) (a+100,b)



{- scoreDisp player
   Converts the score attribute in player into a Picture instead of a Int. 
   RETURNS: A Picture of the score in player.
   EXAMPLES: updateScore (Player {squareLoc = (100,100),
                                  squareIndex = 1,
                                  playerColor = white,
                                  colorBank = (randColorGen (unsafePerformIO (randListGen (10000)))),
                                  candyBank =  [(((100,100),1),black)],         
                                  moveState = False,
                                  gameState = 2,
                                  score = 1})                  --> Text "1"
-}
scoreDisp :: Player -> Picture
scoreDisp player = text (show (score player))


{- updateScore player
   Updates the amount of completed moves by 1.
   RETURNS: A player with the attribute score increased by 1.
   EXAMPLES: updateScore (Player {squareLoc = (100,100),
                                  squareIndex = 1,
                                  playerColor = white,
                                  colorBank = (randColorGen (unsafePerformIO (randListGen (10000)))),
                                  candyBank =  [(((100,100),1),black)],         
                                  moveState = False,
                                  gameState = 2,
                                  score = 0})                  --> 1
-}
updateScore :: Player -> Int
updateScore player = (score player) + 1

{- timer num player
   A countdown timer when the player is in the game. When the countdown hits 0 the game is over.
   Pre: True
   RETURNS: an updated player with the time-attribute equal to 1 less than before the function was called.
   VARIANT: num
   EXAMPLES: timer 1 (Player {squareLoc = (100,100), squareIndex = 1, playerColor = white, colorBank = [],candyBank = [], moveState = False, gameState = 2, score = 0, time = 120})
             ->
             Player {squareLoc = (100.0,100.0), playerColor = RGBA 1.0 1.0 1.0 1.0, squareIndex = 1.0, candyBank = [], moveState = False, gameState = 2, colorBank = [], score = 0, time = 119}
-}
timer :: Float -> Player -> Player
timer num player
  | (gameState player == 1) || (gameState player == 3) = player
  | (time player) <= 0 = player {gameState = 3}
  | otherwise = player {time = ((time player)-1)}

{- showTime player
   Converts time attribute in Player to a Picture.
   PRE: True
   RETURNS: the time attribute in player as a Picture.
   EXAMPLES: showTime (Player {squareLoc   = (100,100), squareIndex = 1, playerColor = white, colorBank = [],candyBank = [], moveState = False, gameState = 1, score = 0, time = 120}) -> Text "120"
-}
showTime :: Player -> Picture
showTime player = text (show (time player))

{- countBlack list n
   Counts the amount of black candies in a list of candies.
   PRE: countBlack is called with n = 0.
   RETURNS: n when when every element in list has been checkes.
   VARIANT: list
   EXAMPLES: countBlack [[(((-350.0,350.0),1),dark (dark violet)),(((-250.0,350.0),2),red),(((-150.0,350.0),3),yellow),(((-50.0,350.0),4),rose),(((50.0,350.0),5),(greyN 0.2)),(((150.0,350.0),6),dark green),(((250.0,350.0),7),red),(((350.0,350.0),8),dark (dark violet)),(((-350.0,250.0),9),rose),(((-250.0,250.0),10),(greyN 0.2)),(((-150.0,250.0),11),black),(((-50.0,250.0),12),black),(((50.0,250.0),13),black),(((150.0,250.0),14),dark (dark violet)),(((250.0,250.0),15),yellow)] 0 -> 3 
-}
countBlack :: [Candy] -> Int -> Int
countBlack [] n = n
countBlack (x:xs) n
  | snd x == black = countBlack xs (n+1)
  | otherwise = countBlack xs n


-----------------------------------------------------------------------------------------------------------------------

rT = runTestTT $ TestList [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, test11, test12, test13, test14, test15, test16, test17, test18, test19, test20, test21, test22, test23, test24, test25, test26, test28, test29, test30, test31, test32, test33, test34, test35, test36, test37, test38, test39, test40]

test1 = TestCase $ assertEqual "boxes" 8 (boxes)
test2 = TestCase $ assertEqual "boxesInt" 8 (boxesInt)
test3 = TestCase $ assertEqual "window" FullScreen (window)
test4 = TestCase $ assertEqual "background" black (background)
test5 = TestCase $ assertEqual "fps" 1 (fps)
test6 = TestCase $ assertEqual "paintCandy1" ([Color black (Translate 100.0 100.0 (Polygon [(-25.0,-25.0),(-25.0,25.0),(25.0,25.0),(25.0,-25.0)]))]) (paintCandy [(((100,100),1),black)])
test7 = TestCase $ assertEqual "paintCandy2" ([Color (black) (Translate 100.0 100.0 (Polygon [(-25.0,-25.0),(-25.0,25.0),(25.0,25.0),(25.0,-25.0)])),Color (red) (Translate 100.0 0.0 (Polygon [(-25.0,-25.0),(-25.0,25.0),(25.0,25.0),(25.0,-25.0)]))]) (paintCandy [(((100,100),1),black),(((100,0),2),red)])
test8 =  TestCase $ assertEqual "updateLocationX" (150.0) (updateLocationX testList1 5)
test9 =  TestCase $ assertEqual "updateLocationX" (-350.0) (updateLocationX testList1 0)
test10 = TestCase $ assertEqual "updateLocationX" (350.0) (updateLocationX testList1 ((length testList1)-1))
test11 = TestCase $ assertEqual "VerifyDownMove" False (verifyMoveCandy 0 (-100) (boxes*boxes))
test12 = TestCase $ assertEqual "VerifyRightMove" True (verifyMoveCandy 100 0 (3))
test13 = TestCase $ assertEqual "VerifySwapCandy" True (verifySwapCandy 3 testList1 "Right" )
test14 = TestCase $ assertEqual "VerifySwapCandy" False (verifySwapCandy 11 testList1 "Left" )
test15 = TestCase $ assertEqual "checkRows" testList1 (checkRows testList1 1 )
test16 = TestCase $ assertEqual "checkRows" testList5 (checkRows testList2 1 )
test17 = TestCase $ assertEqual "checkHorizontalRows" testList3 (checkHorizontalRows 1 1 0 [] testList6 testList6 )
test18 = TestCase $ assertEqual "checkVerticalRows" testList4 (checkVerticalRows 1 1 0 1 [] testList2 )
test19 = TestCase $ assertEqual "refill"  testList8 (refill testList3 [dark green, red, white, white])
test20 = TestCase $ assertEqual "refillAux"  testList8 (refillAux testList3 [] [dark green, red, white, white])
test21 = TestCase $ assertEqual "recolor"  testList8 (recolor testList3 [dark green, red, white, white, red])
-- klara
test22 = TestCase $ assertEqual "updateLocationY1" (100.0) (updateLocationY testList 8)
test23 = TestCase $ assertEqual "updateLocationY2" (-200.0) (updateLocationY testList ((length testList)-1))
test24 = TestCase $ assertEqual "updateLocationY3" (200.0) (updateLocationY testList 0)
test25 = TestCase $ assertEqual "moveBlack" testList10 (moveBlack testList9)
test26 = TestCase $ assertEqual "moveBlack" testList10 (moveBlackAux testList9 8 9)

-- MoveSquare cant be tested since it requires a complete playing field of at least 5*5. When that criteria is met the output is way too big to test.

test27 = TestCase $ assertEqual "for createCandy [1,2,3] ," ([(((1.0,2.0),5),red),(((3.0,4.0),6),red),(((5.0,6.0),7),blue),(((7.0,8.0),8),blue)]) (createCandy 4 [red,red,blue,blue] [(1,2),(3,4),(5,6),(7,8)])
test28 = TestCase $ assertEqual "for createCandy [1,2,3] ," ([(((1.0,2.0),5),white),(((3.0,4.0),6),white),(((5.0,6.0),7),blue),(((7.0,8.0),8),blue)]) (createCandy 4 [white,white,blue,blue] [(1,2),(3,4),(5,6),(7,8)])

test29 = TestCase $ assertEqual "for candyLocations 8 ((-200),200) ," [(-350.0,350.0),(-250.0,350.0),(-150.0,350.0),(-50.0,350.0),(50.0,350.0),(150.0,350.0),(250.0,350.0),(350.0,350.0),(-350.0,250.0),(-250.0,250.0),(-150.0,250.0),(-50.0,250.0),(50.0,250.0),(150.0,250.0),(250.0,250.0),(350.0,250.0),(-350.0,150.0),(-250.0,150.0),(-150.0,150.0),(-50.0,150.0),(50.0,150.0),(150.0,150.0),(250.0,150.0),(350.0,150.0),(-350.0,50.0),(-250.0,50.0),(-150.0,50.0),(-50.0,50.0),(50.0,50.0),(150.0,50.0),(250.0,50.0),(350.0,50.0),(-350.0,-50.0),(-250.0,-50.0),(-150.0,-50.0),(-50.0,-50.0),(50.0,-50.0),(150.0,-50.0),(250.0,-50.0),(350.0,-50.0),(-350.0,-150.0),(-250.0,-150.0),(-150.0,-150.0),(-50.0,-150.0),(50.0,-150.0),(150.0,-150.0),(250.0,-150.0),(350.0,-150.0),(-350.0,-250.0),(-250.0,-250.0),(-150.0,-250.0),(-50.0,-250.0),(50.0,-250.0),(150.0,-250.0),(250.0,-250.0),(350.0,-250.0),(-350.0,-350.0),(-250.0,-350.0),(-150.0,-350.0),(-50.0,-350.0),(50.0,-350.0),(150.0,-350.0),(250.0,-350.0),(350.0,-350.0)] (candyLocations 8 ((-200),200))

test30 = TestCase $ assertEqual "for mkMarker initState (0,0) ," (Pictures [Translate 0.0 0.0 (Color (white) (Line [(-50.0,-50.0),(-50.0,50.0),(50.0,50.0),(50.0,-50.0),(-50.0,-50.0)]))])    (mkMarker initState (0,0))
test31 = TestCase $ assertEqual "for mkMarker initState (0,0) ," (Pictures [Translate 100.0 0.0 (Color (white) (Line [(-50.0,-50.0),(-50.0,50.0),(50.0,50.0),(50.0,-50.0),(-50.0,-50.0)]))])    (mkMarker initState (100,0))

test32 = TestCase $ assertEqual "for randColorGen [1,2,3] ," [red, white, (greyN 0.2)] (randColorGen [1,2,3])
test33 = TestCase $ assertEqual "for randColorGen [4,5,6] ," [dark (dark violet), rose, dark green] (randColorGen [4,5,6])

test34 = TestCase $ assertEqual "for getColor 3 ," (greyN 0.2) (getColor 3)
test35 = TestCase $ assertEqual "for getColor 2 ," (white) (getColor 2)
test36 = TestCase $ assertEqual "for getColor 1 ," (red) (getColor 1)

test37 = TestCase $ assertEqual "for squareLocations 8 ((-200),200)," [(-350.0,350.0),(-250.0,350.0),(-150.0,350.0),(-50.0,350.0),(50.0,350.0),(150.0,350.0),(250.0,350.0),(350.0,350.0),(-350.0,250.0),(-250.0,250.0),(-150.0,250.0),(-50.0,250.0),(50.0,250.0),(150.0,250.0),(250.0,250.0),(350.0,250.0),(-350.0,150.0),(-250.0,150.0),(-150.0,150.0),(-50.0,150.0),(50.0,150.0),(150.0,150.0),(250.0,150.0),(350.0,150.0),(-350.0,50.0),(-250.0,50.0),(-150.0,50.0),(-50.0,50.0),(50.0,50.0),(150.0,50.0),(250.0,50.0),(350.0,50.0),(-350.0,-50.0),(-250.0,-50.0),(-150.0,-50.0),(-50.0,-50.0),(50.0,-50.0),(150.0,-50.0),(250.0,-50.0),(350.0,-50.0),(-350.0,-150.0),(-250.0,-150.0),(-150.0,-150.0),(-50.0,-150.0),(50.0,-150.0),(150.0,-150.0),(250.0,-150.0),(350.0,-150.0),(-350.0,-250.0),(-250.0,-250.0),(-150.0,-250.0),(-50.0,-250.0),(50.0,-250.0),(150.0,-250.0),(250.0,-250.0),(350.0,-250.0),(-350.0,-350.0),(-250.0,-350.0),(-150.0,-350.0),(-50.0,-350.0),(50.0,-350.0),(150.0,-350.0),(250.0,-350.0),(350.0,-350.0)] (squareLocations 8 ((-200),200))

test38 = TestCase $ assertEqual "for updateScore initState ," 1 (updateScore initState)

test39 = TestCase $ assertEqual "showTime" (Text "119") (showTime (Player {squareLoc = (100.0,100.0), playerColor = white, squareIndex = 1.0, candyBank = [], moveState = False, gameState = 2, colorBank = [], score = 0, time = 119}))

test40 = TestCase $ assertEqual "countBlack" 3 (countBlack testList9 0)




testlist = testList :: [Candy]
testList = [(((-200.0,200.0),1),green),(((-100.0,200.0),2),white),(((0.0,200.0),3),green),(((100.0,200.0),4),green),(((200.0,200.0),5),red),(((-200.0,100.0),6),green),(((-100.0,100.0),7),blue),(((0.0,100.0),8),white),(((100.0,100.0),9),white),(((200.0,100.0),10),blue),(((-200.0,0.0),11),white),(((-100.0,0.0),12),red),(((0.0,0.0),13),green),(((100.0,0.0),14),green),(((200.0,0.0),15),white),(((-200.0,-100.0),16),blue),(((-100.0,-100.0),17),red),(((0.0,-100.0),18),red),(((100.0,-100.0),19),white),(((200.0,-100.0),20),white),(((-200.0,-200.0),21),blue),(((-100.0,-200.0),22),blue),(((0.0,-200.0),23),white),(((100.0,-200.0),24),blue),(((200.0,-200.0),25),blue)]

testList1 = [(((-350.0,350.0),1),dark (dark violet)),(((-250.0,350.0),2),red),(((-150.0,350.0),3),yellow),(((-50.0,350.0),4),rose),(((50.0,350.0),5),(greyN 0.2)),(((150.0,350.0),6),dark green),(((250.0,350.0),7),red),(((350.0,350.0),8),dark (dark violet)),(((-350.0,250.0),9),rose),(((-250.0,250.0),10),(greyN 0.2)),(((-150.0,250.0),11),red),(((-50.0,250.0),12),yellow),(((50.0,250.0),13),white),(((150.0,250.0),14),dark (dark violet)),(((250.0,250.0),15),yellow),(((350.0,250.0),16),white),(((-350.0,150.0),17),(greyN 0.2)),(((-250.0,150.0),18),red),(((-150.0,150.0),19),(greyN 0.2)),(((-50.0,150.0),20),white),(((50.0,150.0),21),yellow),(((150.0,150.0),22),red),(((250.0,150.0),23),dark (dark violet)),(((350.0,150.0),24),dark green),(((-350.0,50.0),25),rose),(((-250.0,50.0),26),(greyN 0.2)),(((-150.0,50.0),27),yellow),(((-50.0,50.0),28),(greyN 0.2)),(((50.0,50.0),29),dark green),(((150.0,50.0),30),white),(((250.0,50.0),31),red),(((350.0,50.0),32),rose),(((-350.0,-50.0),33),white),(((-250.0,-50.0),34),dark (dark violet)),(((-150.0,-50.0),35),red),(((-50.0,-50.0),36),dark (dark violet)),(((50.0,-50.0),37),red),(((150.0,-50.0),38),dark (dark violet)),(((250.0,-50.0),39),yellow),(((350.0,-50.0),40),dark green),(((-350.0,-150.0),41),(greyN 0.2)),(((-250.0,-150.0),42),red),(((-150.0,-150.0),43),(greyN 0.2)),(((-50.0,-150.0),44),red),(((50.0,-150.0),45),dark (dark violet)),(((150.0,-150.0),46),white),(((250.0,-150.0),47),dark (dark violet)),(((350.0,-150.0),48),dark green),(((-350.0,-250.0),49),red),(((-250.0,-250.0),50),(greyN 0.2)),(((-150.0,-250.0),51),dark (dark violet)),(((-50.0,-250.0),52),white),(((50.0,-250.0),53),dark (dark violet)),(((150.0,-250.0),54),white),(((250.0,-250.0),55),dark green),(((350.0,-250.0),56),white),(((-350.0,-350.0),57),dark (dark violet)),(((-250.0,-350.0),58),(greyN 0.2)),(((-150.0,-350.0),59),red),(((-50.0,-350.0),60),dark (dark violet)),(((50.0,-350.0),61),dark green),(((150.0,-350.0),62),dark green),(((250.0,-350.0),63),red),(((350.0,-350.0),64),dark green)]

testList2 = [(((-350.0,350.0),1),dark (dark violet)),(((-250.0,350.0),2),red),(((-150.0,350.0),3),yellow),(((-50.0,350.0),4),rose),(((50.0,350.0),5),(greyN 0.2)),(((150.0,350.0),6),dark green),(((250.0,350.0),7),red),(((350.0,350.0),8),dark (dark violet)),(((-350.0,250.0),9),rose),(((-250.0,250.0),10),red),(((-150.0,250.0),11),(greyN 0.2)),(((-50.0,250.0),12),yellow),(((50.0,250.0),13),white),(((150.0,250.0),14),dark (dark violet)),(((250.0,250.0),15),yellow),(((350.0,250.0),16),white),(((-350.0,150.0),17),(greyN 0.2)),(((-250.0,150.0),18),red),(((-150.0,150.0),19),(greyN 0.2)),(((-50.0,150.0),20),white),(((50.0,150.0),21),yellow),(((150.0,150.0),22),red),(((250.0,150.0),23),dark (dark violet)),(((350.0,150.0),24),dark green),(((-350.0,50.0),25),rose),(((-250.0,50.0),26),(greyN 0.2)),(((-150.0,50.0),27),yellow),(((-50.0,50.0),28),(greyN 0.2)),(((50.0,50.0),29),dark green),(((150.0,50.0),30),white),(((250.0,50.0),31),red),(((350.0,50.0),32),rose),(((-350.0,-50.0),33),white),(((-250.0,-50.0),34),dark (dark violet)),(((-150.0,-50.0),35),red),(((-50.0,-50.0),36),dark (dark violet)),(((50.0,-50.0),37),red),(((150.0,-50.0),38),dark (dark violet)),(((250.0,-50.0),39),yellow),(((350.0,-50.0),40),dark green),(((-350.0,-150.0),41),(greyN 0.2)),(((-250.0,-150.0),42),red),(((-150.0,-150.0),43),(greyN 0.2)),(((-50.0,-150.0),44),red),(((50.0,-150.0),45),dark (dark violet)),(((150.0,-150.0),46),white),(((250.0,-150.0),47),dark (dark violet)),(((350.0,-150.0),48),dark green),(((-350.0,-250.0),49),red),(((-250.0,-250.0),50),(greyN 0.2)),(((-150.0,-250.0),51),dark (dark violet)),(((-50.0,-250.0),52),white),(((50.0,-250.0),53),dark (dark violet)),(((150.0,-250.0),54),white),(((250.0,-250.0),55),dark green),(((350.0,-250.0),56),white),(((-350.0,-350.0),57),dark (dark violet)),(((-250.0,-350.0),58),(greyN 0.2)),(((-150.0,-350.0),59),red),(((-50.0,-350.0),60),dark (dark violet)),(((50.0,-350.0),61),dark green),(((150.0,-350.0),62),dark green),(((250.0,-350.0),63),red),(((350.0,-350.0),64),dark green)]

testList3 = [(((-350.0,350.0),1),black),(((-250.0,350.0),2),black),(((-150.0,350.0),3),black)]++ (drop 3 testList6)

testList4 = [(((-350.0,350.0),1),dark (dark violet)),(((-250.0,350.0),2),black),(((-150.0,350.0),3),yellow),(((-50.0,350.0),4),rose),(((50.0,350.0),5),(greyN 0.2)),(((150.0,350.0),6),dark green),(((250.0,350.0),7),red),(((350.0,350.0),8),dark (dark violet)),(((-350.0,250.0),9),rose),(((-250.0,250.0),10),black),(((-150.0,250.0),11),(greyN 0.2)),(((-50.0,250.0),12),yellow),(((50.0,250.0),13),white),(((150.0,250.0),14),dark (dark violet)),(((250.0,250.0),15),yellow),(((350.0,250.0),16),white),(((-350.0,150.0),17),(greyN 0.2)),(((-250.0,150.0),18),black),(((-150.0,150.0),19),(greyN 0.2)),(((-50.0,150.0),20),white),(((50.0,150.0),21),yellow),(((150.0,150.0),22),red),(((250.0,150.0),23),dark (dark violet)),(((350.0,150.0),24),dark green),(((-350.0,50.0),25),rose),(((-250.0,50.0),26),(greyN 0.2)),(((-150.0,50.0),27),yellow),(((-50.0,50.0),28),(greyN 0.2)),(((50.0,50.0),29),dark green),(((150.0,50.0),30),white),(((250.0,50.0),31),red),(((350.0,50.0),32),rose),(((-350.0,-50.0),33),white),(((-250.0,-50.0),34),dark (dark violet)),(((-150.0,-50.0),35),red),(((-50.0,-50.0),36),dark (dark violet)),(((50.0,-50.0),37),red),(((150.0,-50.0),38),dark (dark violet)),(((250.0,-50.0),39),yellow),(((350.0,-50.0),40),dark green),(((-350.0,-150.0),41),(greyN 0.2)),(((-250.0,-150.0),42),red),(((-150.0,-150.0),43),(greyN 0.2)),(((-50.0,-150.0),44),red),(((50.0,-150.0),45),dark (dark violet)),(((150.0,-150.0),46),white),(((250.0,-150.0),47),dark (dark violet)),(((350.0,-150.0),48),dark green),(((-350.0,-250.0),49),red),(((-250.0,-250.0),50),(greyN 0.2)),(((-150.0,-250.0),51),dark (dark violet)),(((-50.0,-250.0),52),white),(((50.0,-250.0),53),dark (dark violet)),(((150.0,-250.0),54),white),(((250.0,-250.0),55),dark green),(((350.0,-250.0),56),white),(((-350.0,-350.0),57),dark (dark violet)),(((-250.0,-350.0),58),(greyN 0.2)),(((-150.0,-350.0),59),red),(((-50.0,-350.0),60),dark (dark violet)),(((50.0,-350.0),61),dark green),(((150.0,-350.0),62),dark green),(((250.0,-350.0),63),red),(((350.0,-350.0),64),dark green)]

testList5 = [(((-350.0,350.0),1),dark (dark violet)),(((-250.0,350.0),2),black),(((-150.0,350.0),3),yellow),(((-50.0,350.0),4),rose),(((50.0,350.0),5),(greyN 0.2)),(((150.0,350.0),6),dark green),(((250.0,350.0),7),red),(((350.0,350.0),8),dark (dark violet)),(((-350.0,250.0),9),rose),(((-250.0,250.0),10),black),(((-150.0,250.0),11),(greyN 0.2)),(((-50.0,250.0),12),yellow),(((50.0,250.0),13),white),(((150.0,250.0),14),dark (dark violet)),(((250.0,250.0),15),yellow),(((350.0,250.0),16),white),(((-350.0,150.0),17),(greyN 0.2)),(((-250.0,150.0),18),black),(((-150.0,150.0),19),(greyN 0.2)),(((-50.0,150.0),20),white),(((50.0,150.0),21),yellow),(((150.0,150.0),22),red),(((250.0,150.0),23),dark (dark violet)),(((350.0,150.0),24),dark green),(((-350.0,50.0),25),rose),(((-250.0,50.0),26),(greyN 0.2)),(((-150.0,50.0),27),yellow),(((-50.0,50.0),28),(greyN 0.2)),(((50.0,50.0),29),dark green),(((150.0,50.0),30),white),(((250.0,50.0),31),red),(((350.0,50.0),32),rose),(((-350.0,-50.0),33),white),(((-250.0,-50.0),34),dark (dark violet)),(((-150.0,-50.0),35),red),(((-50.0,-50.0),36),dark (dark violet)),(((50.0,-50.0),37),red),(((150.0,-50.0),38),dark (dark violet)),(((250.0,-50.0),39),yellow),(((350.0,-50.0),40),dark green),(((-350.0,-150.0),41),(greyN 0.2)),(((-250.0,-150.0),42),red),(((-150.0,-150.0),43),(greyN 0.2)),(((-50.0,-150.0),44),red),(((50.0,-150.0),45),dark (dark violet)),(((150.0,-150.0),46),white),(((250.0,-150.0),47),dark (dark violet)),(((350.0,-150.0),48),dark green),(((-350.0,-250.0),49),red),(((-250.0,-250.0),50),(greyN 0.2)),(((-150.0,-250.0),51),dark (dark violet)),(((-50.0,-250.0),52),white),(((50.0,-250.0),53),dark (dark violet)),(((150.0,-250.0),54),white),(((250.0,-250.0),55),dark green),(((350.0,-250.0),56),white),(((-350.0,-350.0),57),dark (dark violet)),(((-250.0,-350.0),58),(greyN 0.2)),(((-150.0,-350.0),59),red),(((-50.0,-350.0),60),dark (dark violet)),(((50.0,-350.0),61),dark green),(((150.0,-350.0),62),dark green),(((250.0,-350.0),63),red),(((350.0,-350.0),64),dark green)]


testList6 = [(((-350.0,350.0),1),dark (dark violet)),(((-250.0,350.0),2),dark (dark violet)),(((-150.0,350.0),3),dark (dark violet)),(((-50.0,350.0),4),rose),(((50.0,350.0),5),(greyN 0.2)),(((150.0,350.0),6),dark green),(((250.0,350.0),7),red),(((350.0,350.0),8),dark (dark violet)),(((-350.0,250.0),9),rose),(((-250.0,250.0),10),(greyN 0.2)),(((-150.0,250.0),11),red),(((-50.0,250.0),12),yellow),(((50.0,250.0),13),white),(((150.0,250.0),14),dark (dark violet)),(((250.0,250.0),15),yellow),(((350.0,250.0),16),white),(((-350.0,150.0),17),(greyN 0.2)),(((-250.0,150.0),18),red),(((-150.0,150.0),19),(greyN 0.2)),(((-50.0,150.0),20),white),(((50.0,150.0),21),yellow),(((150.0,150.0),22),red),(((250.0,150.0),23),dark (dark violet)),(((350.0,150.0),24),dark green),(((-350.0,50.0),25),rose),(((-250.0,50.0),26),(greyN 0.2)),(((-150.0,50.0),27),yellow),(((-50.0,50.0),28),(greyN 0.2)),(((50.0,50.0),29),dark green),(((150.0,50.0),30),white),(((250.0,50.0),31),red),(((350.0,50.0),32),rose),(((-350.0,-50.0),33),white),(((-250.0,-50.0),34),dark (dark violet)),(((-150.0,-50.0),35),red),(((-50.0,-50.0),36),dark (dark violet)),(((50.0,-50.0),37),red),(((150.0,-50.0),38),dark (dark violet)),(((250.0,-50.0),39),yellow),(((350.0,-50.0),40),dark green),(((-350.0,-150.0),41),(greyN 0.2)),(((-250.0,-150.0),42),red),(((-150.0,-150.0),43),(greyN 0.2)),(((-50.0,-150.0),44),red),(((50.0,-150.0),45),dark (dark violet)),(((150.0,-150.0),46),white),(((250.0,-150.0),47),dark (dark violet)),(((350.0,-150.0),48),dark green),(((-350.0,-250.0),49),red),(((-250.0,-250.0),50),(greyN 0.2)),(((-150.0,-250.0),51),dark (dark violet)),(((-50.0,-250.0),52),white),(((50.0,-250.0),53),dark (dark violet)),(((150.0,-250.0),54),white),(((250.0,-250.0),55),dark green),(((350.0,-250.0),56),white),(((-350.0,-350.0),57),dark (dark violet)),(((-250.0,-350.0),58),(greyN 0.2)),(((-150.0,-350.0),59),red),(((-50.0,-350.0),60),dark (dark violet)),(((50.0,-350.0),61),dark green),(((150.0,-350.0),62),dark green),(((250.0,-350.0),63),red),(((350.0,-350.0),64),dark green)]

testList8 = [(((-350.0,350.0),1), dark green),(((-250.0,350.0),2),red),(((-150.0,350.0),3),white),(((-50.0,350.0),4),rose),(((50.0,350.0),5),(greyN 0.2)),(((150.0,350.0),6),dark green),(((250.0,350.0),7),red),(((350.0,350.0),8),dark (dark violet)),(((-350.0,250.0),9),rose),(((-250.0,250.0),10),(greyN 0.2)),(((-150.0,250.0),11),red),(((-50.0,250.0),12),yellow),(((50.0,250.0),13),white),(((150.0,250.0),14),dark (dark violet)),(((250.0,250.0),15),yellow),(((350.0,250.0),16),white),(((-350.0,150.0),17),(greyN 0.2)),(((-250.0,150.0),18),red),(((-150.0,150.0),19),(greyN 0.2)),(((-50.0,150.0),20),white),(((50.0,150.0),21),yellow),(((150.0,150.0),22),red),(((250.0,150.0),23),dark (dark violet)),(((350.0,150.0),24),dark green),(((-350.0,50.0),25),rose),(((-250.0,50.0),26),(greyN 0.2)),(((-150.0,50.0),27),yellow),(((-50.0,50.0),28),(greyN 0.2)),(((50.0,50.0),29),dark green),(((150.0,50.0),30),white),(((250.0,50.0),31),red),(((350.0,50.0),32),rose),(((-350.0,-50.0),33),white),(((-250.0,-50.0),34),dark (dark violet)),(((-150.0,-50.0),35),red),(((-50.0,-50.0),36),dark (dark violet)),(((50.0,-50.0),37),red),(((150.0,-50.0),38),dark (dark violet)),(((250.0,-50.0),39),yellow),(((350.0,-50.0),40),dark green),(((-350.0,-150.0),41),(greyN 0.2)),(((-250.0,-150.0),42),red),(((-150.0,-150.0),43),(greyN 0.2)),(((-50.0,-150.0),44),red),(((50.0,-150.0),45),dark (dark violet)),(((150.0,-150.0),46),white),(((250.0,-150.0),47),dark (dark violet)),(((350.0,-150.0),48),dark green),(((-350.0,-250.0),49),red),(((-250.0,-250.0),50),(greyN 0.2)),(((-150.0,-250.0),51),dark (dark violet)),(((-50.0,-250.0),52),white),(((50.0,-250.0),53),dark (dark violet)),(((150.0,-250.0),54),white),(((250.0,-250.0),55),dark green),(((350.0,-250.0),56),white),(((-350.0,-350.0),57),dark (dark violet)),(((-250.0,-350.0),58),(greyN 0.2)),(((-150.0,-350.0),59),red),(((-50.0,-350.0),60),dark (dark violet)),(((50.0,-350.0),61),dark green),(((150.0,-350.0),62),dark green),(((250.0,-350.0),63),red),(((350.0,-350.0),64),dark green)]

testList9 = [(((-350.0,350.0),1),dark (dark violet)),(((-250.0,350.0),2),red),(((-150.0,350.0),3),yellow),(((-50.0,350.0),4),rose),(((50.0,350.0),5),(greyN 0.2)),(((150.0,350.0),6),dark green),(((250.0,350.0),7),red),(((350.0,350.0),8),dark (dark violet)),(((-350.0,250.0),9),rose),(((-250.0,250.0),10),(greyN 0.2)),(((-150.0,250.0),11),black),(((-50.0,250.0),12),black),(((50.0,250.0),13),black),(((150.0,250.0),14),dark (dark violet)),(((250.0,250.0),15),yellow),(((350.0,250.0),16),white),(((-350.0,150.0),17),(greyN 0.2)),(((-250.0,150.0),18),red),(((-150.0,150.0),19),(greyN 0.2)),(((-50.0,150.0),20),white),(((50.0,150.0),21),yellow),(((150.0,150.0),22),red),(((250.0,150.0),23),dark (dark violet)),(((350.0,150.0),24),dark green),(((-350.0,50.0),25),rose),(((-250.0,50.0),26),(greyN 0.2)),(((-150.0,50.0),27),yellow),(((-50.0,50.0),28),(greyN 0.2)),(((50.0,50.0),29),dark green),(((150.0,50.0),30),white),(((250.0,50.0),31),red),(((350.0,50.0),32),rose),(((-350.0,-50.0),33),white),(((-250.0,-50.0),34),dark (dark violet)),(((-150.0,-50.0),35),red),(((-50.0,-50.0),36),dark (dark violet)),(((50.0,-50.0),37),red),(((150.0,-50.0),38),dark (dark violet)),(((250.0,-50.0),39),yellow),(((350.0,-50.0),40),dark green),(((-350.0,-150.0),41),(greyN 0.2)),(((-250.0,-150.0),42),red),(((-150.0,-150.0),43),(greyN 0.2)),(((-50.0,-150.0),44),red),(((50.0,-150.0),45),dark (dark violet)),(((150.0,-150.0),46),white),(((250.0,-150.0),47),dark (dark violet)),(((350.0,-150.0),48),dark green),(((-350.0,-250.0),49),red),(((-250.0,-250.0),50),(greyN 0.2)),(((-150.0,-250.0),51),dark (dark violet)),(((-50.0,-250.0),52),white),(((50.0,-250.0),53),dark (dark violet)),(((150.0,-250.0),54),white),(((250.0,-250.0),55),dark green),(((350.0,-250.0),56),white),(((-350.0,-350.0),57),dark (dark violet)),(((-250.0,-350.0),58),(greyN 0.2)),(((-150.0,-350.0),59),red),(((-50.0,-350.0),60),dark (dark violet)),(((50.0,-350.0),61),dark green),(((150.0,-350.0),62),dark green),(((250.0,-350.0),63),red),(((350.0,-350.0),64),dark green)]
  
testList10 = [(((-350.0,350.0),1),dark (dark violet)),(((-250.0,350.0),2),red),(((-150.0,350.0),3),black),(((-50.0,350.0),4),black),(((50.0,350.0),5),black),(((150.0,350.0),6),dark green),(((250.0,350.0),7),red),(((350.0,350.0),8),dark (dark violet)),(((-350.0,250.0),9),rose),(((-250.0,250.0),10),(greyN 0.2)),(((-150.0,250.0),11),yellow),(((-50.0,250.0),12),rose),(((50.0,250.0),13),(greyN 0.2)),(((150.0,250.0),14),dark (dark violet)),(((250.0,250.0),15),yellow),(((350.0,250.0),16),white),(((-350.0,150.0),17),(greyN 0.2)),(((-250.0,150.0),18),red),(((-150.0,150.0),19),(greyN 0.2)),(((-50.0,150.0),20),white),(((50.0,150.0),21),yellow),(((150.0,150.0),22),red),(((250.0,150.0),23),dark (dark violet)),(((350.0,150.0),24),dark green),(((-350.0,50.0),25),rose),(((-250.0,50.0),26),(greyN 0.2)),(((-150.0,50.0),27),yellow),(((-50.0,50.0),28),(greyN 0.2)),(((50.0,50.0),29),dark green),(((150.0,50.0),30),white),(((250.0,50.0),31),red),(((350.0,50.0),32),rose),(((-350.0,-50.0),33),white),(((-250.0,-50.0),34),dark (dark violet)),(((-150.0,-50.0),35),red),(((-50.0,-50.0),36),dark (dark violet)),(((50.0,-50.0),37),red),(((150.0,-50.0),38),dark (dark violet)),(((250.0,-50.0),39),yellow),(((350.0,-50.0),40),dark green),(((-350.0,-150.0),41),(greyN 0.2)),(((-250.0,-150.0),42),red),(((-150.0,-150.0),43),(greyN 0.2)),(((-50.0,-150.0),44),red),(((50.0,-150.0),45),dark (dark violet)),(((150.0,-150.0),46),white),(((250.0,-150.0),47),dark (dark violet)),(((350.0,-150.0),48),dark green),(((-350.0,-250.0),49),red),(((-250.0,-250.0),50),(greyN 0.2)),(((-150.0,-250.0),51),dark (dark violet)),(((-50.0,-250.0),52),white),(((50.0,-250.0),53),dark (dark violet)),(((150.0,-250.0),54),white),(((250.0,-250.0),55),dark green),(((350.0,-250.0),56),white),(((-350.0,-350.0),57),dark (dark violet)),(((-250.0,-350.0),58),(greyN 0.2)),(((-150.0,-350.0),59),red),(((-50.0,-350.0),60),dark (dark violet)),(((50.0,-350.0),61),dark green),(((150.0,-350.0),62),dark green),(((250.0,-350.0),63),red),(((350.0,-350.0),64),dark green)]
--------------------------------------------------------------------------------------------------------------------------------------------------


