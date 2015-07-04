module Main where
import System.Random
import Control.Monad.Random
import Data.Functor

import System.Console.ANSI
import System.IO

type Rnd a = Rand StdGen a

type Coord = (Int, Int)

data World = World { wHero :: Coord }

main :: IO()
main = do
  hSetEcho stdin False
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  hideCursor
  setTitle "Scramble"
  gameLoop $ World (0, 0)

drawHero (heroX, heroY) = do
  clearScreen
  setCursorPosition heroY heroX
  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Blue ]
  putStr "@"

gameLoop world@(World hero) = do
  drawHero hero
  input <- getInput
  case input of
    KExit -> handleExit
    _    -> gameLoop $ handleDir world input

handleExit = do
  clearScreen
  setCursorPosition 0 0
  setSGR [ Reset ]
  showCursor
  putStrLn "Bye!"

data Input = KUp
           | KDown
           | KLeft
           | KRight
           | KExit
           deriving (Eq)

addC :: Coord -> Coord -> Coord
addC (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

dirToDiff d
  | d == KUp    = (0, -1)
  | d == KDown  = (0,  1)
  | d == KLeft  = (-1, 0)
  | d == KRight = (1,  0)
  | otherwise  = (0,  0)

-- Move hero to supplied direction, limitimg the hero's coordinates to range 0..80
handleDir :: World -> Input -> World
handleDir w@(World hero) input = w { wHero = (hConst heroX, hConst heroY) }
  where (heroX, heroY) = addC hero (dirToDiff input)
        hConst i       = max 0 (min i 80)

-- handleDir world@(World (heroX, heroY)) input = world { wHero = newCoord }
--   where newCoord = case input of
--                     KUp    -> (heroX, heroY - 1)
--                     KDown  -> (heroX, heroY + 1)
--                     KLeft  -> (heroX - 1, heroY)
--                     KRight -> (heroX + 1, heroY)

getInput :: IO Input
getInput = do
  char <- getChar
  case char of
    'q' -> return KExit
    'w' -> return KUp
    's' -> return KDown
    'a' -> return KLeft
    'd' -> return KRight
    _ -> getInput -- Recursing on invalid input
