{-
import Graphics.Gloss

type Position = (Int, Int)
type Candy = (Position, Int)
type Gameboard = [Candy]



main = display (InWindow "Nice Window" (200, 200) (10, 10)) white (Circle 80)

import Graphics.Gloss
main = display (InWindow "Nice Window" (200, 200) (10, 10)) white (Circle 80)
-}

-- GLUT.hs
module Main where

import Graphics.UI.GLUT (($=), getArgsAndInitialize, createWindow, displayCallback, mainLoop)

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Hello World"
  displayCallback $= return ()
