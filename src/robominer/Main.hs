module Main where

import System.Random
import Control.Monad.Random

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.List as L
-- import Data.Sequence

import System.Console.ANSI
import System.IO

import qualified Control.Concurrent as CC

type Pos = (Int, Int)
type Size = Pos
type PosDelta = Pos
type Mines = Set Pos
type Layer a = Map Pos a
type Intel = Layer Int
data Cell = CMine
            | CUnknown
            | CFree
       deriving (Eq)
type Field = Layer Cell

genMine :: (RandomGen g) => (Int, Int) -> Rand g (Int, Int)
genMine (maxX, maxY) = do
    x <- getRandomR (0, maxX - 1)
    y <- getRandomR (0, maxY - 1)
    return (x, y)

genMines :: Size -> IO Mines
genMines size = do
      mines <- evalRandIO $ sequence $ replicate 10  $ genMine size
      return $ S.fromList mines

genField :: Size -> Field
genField (sx, sy) = M.fromList [((x,y), CUnknown) | x <- [0..(sx-1)], y <- [0..(sy-1)]]

renderCell :: Show a => Pos -> a -> IO ()
renderCell (x, y) c = do
               setCursorPosition y x
               putStr (show c)

renderLayer :: Show a => Map Pos a -> IO ()
renderLayer cells = mapM_ (uncurry renderCell) (M.toList cells)

renderColored :: [SGR] -> IO () -> IO ()
renderColored colorSetup action = do
  setSGR colorSetup
  action
  setSGR [Reset]

positionsToLayer :: (Show a) => (Pos -> a) -> Set Pos -> Layer a
positionsToLayer showFn = M.fromSet showFn

instance Show Cell where
  show CMine = "@"
  show CUnknown = "#"
  show CFree = " "

isGameComplete :: Mines -> Field -> Bool
isGameComplete mines field = S.size mines == L.length (filter (CUnknown==) (M.elems field))

step :: Mines -> Field -> Pos -> Maybe Field
step mines field pos
  | S.member pos mines = Nothing
  | otherwise = Just $ newField
  where newField :: Map Pos Cell
        newField = M.alter (const (Just CFree)) pos field

neighbourDeltas :: [PosDelta]
neighbourDeltas = [(x, y) | x <- d, y <- d, x /= 0 || y /= 0]
  where d = [-1, 0, 1]

shiftPos :: Pos -> PosDelta -> Pos
shiftPos (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

nearPositions :: Pos -> [Pos]
nearPositions pos = fmap (\d -> shiftPos pos d) neighbourDeltas

genIntel :: Mines -> Intel
genIntel mines = foldl updateNeighbours M.empty (S.toList mines)
  where updateNeighbours intel pos = foldl (\intl nPos -> M.alter updateCount nPos intl)
                                           intel (nearPositions pos)
        updateCount Nothing = Just 1
        updateCount (Just x) = Just (x + 1)

filterIntel :: Intel -> Field -> Intel
filterIntel intel field = M.filterWithKey (\pos cnt -> (M.lookup pos field) == (Just CFree)) intel


chooseProbePosition :: Field -> Intel -> Pos
chooseProbePosition field intel = head $ fmap fst reordered
  where
  
    -- TODO Correct this !!!!!!!!!!!!!!!!!!!!!!!!!! first should test neighbours of empty slots

    unknowns = M.toList $ M.filter (CUnknown==) field
    splitted = L.partition
      (\(pos, c) -> case M.lookup pos intel of
                      Nothing -> True
                      Just 0 -> True
                      Just _ -> False)
      unknowns
    reordered = (fst splitted) ++ (snd splitted)




moveCursorBelow :: Size -> IO ()
moveCursorBelow boardSize = setCursorPosition (snd boardSize) 0

showStatus :: Size -> String -> IO ()
showStatus fieldSize message = do
    moveCursorBelow fieldSize
    putStrLn message

renderBoard :: Field -> Intel -> Mines -> IO ()
renderBoard field intel mines =
  let minesColor = [SetConsoleIntensity BoldIntensity,
                    SetColor Foreground Vivid Red]
      fieldColor = [SetColor Foreground Dull White]
  in do
    renderColored fieldColor $ renderLayer field
    renderColored minesColor $ renderLayer $ positionsToLayer (const CMine) mines
    renderLayer intel

gameStep :: Size -> Mines -> Field -> IO ()
gameStep fieldSize mines field =
  let intl = filterIntel (genIntel mines) field
      probePos = (chooseProbePosition field intl)
      newField = step mines field probePos
      showFinalStatus message = do
        showStatus fieldSize message
        return ()
  in do
    -- CC.threadDelay 300000
    renderBoard field intl mines
    moveCursorBelow fieldSize -- Move it away so it does not obstruct cells
    case newField of
      Nothing -> showFinalStatus ("Tripped on mine at " ++ (show probePos))
      Just f ->
        if isGameComplete mines f
          then showFinalStatus "Done"
          else gameStep fieldSize mines f

main :: IO ()
main =
  let dims = (20, 10)
  in do
    clearScreen
    mines <- genMines dims
    gameStep dims mines (genField dims)