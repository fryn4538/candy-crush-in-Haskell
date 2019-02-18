module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.Color (makeColor, red)
import Graphics.Gloss.Data.ViewPort
--import Graphics.UI.GLUT
import Control.Exception

import Graphics.Gloss.Data.Picture

location = (710, 290)
windowSize = (500,500)

{-
main = IO ()
main = play
    (InWindow "Candy-Crush" (500,500) location)
    white
    20
    
    renderBoard
-}

squares = 5


main :: IO ()
main = display (InWindow "Hello World" windowSize location)
               (makeColor 0.9 0.9 0.9 1)
               (Pictures $ paintRectangles (squareLocations 5 ((-200),200)))


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
