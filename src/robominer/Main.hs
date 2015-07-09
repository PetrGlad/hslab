module Main where

import System.Random
import Control.Monad.Random

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.MultiMap as Mm
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Tuple as Tu
-- import Data.Sequence

import qualified Control.Exception as CE

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

genMines :: Size -> Int -> IO Mines
genMines size cnt = do
      mines <- evalRandIO $ sequence $ replicate cnt  $ genMine size
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
  where d = [-1..1]

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

-- The cell is already known as not containing mine
isFreeCell :: Field -> Pos -> Bool
isFreeCell field pos = (M.lookup pos field) == (Just CFree)

filterLayer :: Layer a -> Field -> Layer a
filterLayer intel field = M.filterWithKey (\pos cnt -> isFreeCell field pos) intel

-----------------------------------------------------------

chooseProbePosition :: Size -> Field -> Intel -> Pos
chooseProbePosition fieldSize field intel = choice
  where
    -- Fallback implementation
    unknowns = M.toList $ M.filter (CUnknown==) field
    choice0 = head $ fmap fst unknowns

    -- TODO Implement
    intelRel = intelMatrix fieldSize (enumPositions fieldSize)
    edgeMinesMatrix = probableMinesMatrix (visibleIntelMatrix intelRel field) field
    neighbourToMine = groupByNeighbour edgeMinesMatrix
    mineToNeighbour = groupByFirst edgeMinesMatrix
    choice = if Mm.null neighbourToMine
             then choice0
              else (head . snd . head . M.toList . Mm.toMap) neighbourToMine -- Stub -- TODO Implement
--              else
--                 consistentCombinations mineToNeighbour neighbourToMine intel

    -- 1 Calc envelope (one side is opened - connected side is in unknown)
    -- 2 Choose cell with fewest connections
    -- 3 select a combination
    -- 4 follow connections propagating with reqstrictions until no more connections or no appropriate combination
    -- 5 If all is satisfied - select cell& exit. Otherwise choose new combination
    -- 6 Try next cell in (2)

--     splitted = L.partition
--       (\(pos, c) -> case M.lookup pos intel of
--                       Nothing -> True
--                       Just 0 -> True
--                       Just _ -> False)
--       unknowns
--     reordered = (fst splitted) ++ (snd splitted)

type CellPair = (Pos, Pos)

-- Defines index order of positions.
enumPositions :: Size -> [Pos]
enumPositions (width, height) = [(col, row) | col <- [0..(width-1)], row <- [0..(height-1)]]

inRange :: Int -> Int -> Int -> Bool
inRange lo hi x = x >= lo && x < hi

inBoard :: Size -> Pos -> Bool
inBoard (w, h) (x,y) = (inRange 0 w x) && (inRange 0 h y)

-- Produces (cell, neighbour) relations
intelMatrix :: Size -> [Pos] -> [CellPair]
intelMatrix s ps = concatMap (\p -> fmap (\n -> (p, n))
                                         (filter (inBoard s) (nearPositions p)))
                                  ps

-- Limit relations to ones we can reason about on given field
-- (i.e. a relation neighbour is visible/uncovered)
visibleIntelMatrix :: [CellPair] -> Field -> [CellPair]
visibleIntelMatrix pairs field = filter ((isFreeCell field) . snd) pairs

probableMinesMatrix :: [CellPair] -> Field -> [CellPair]
probableMinesMatrix pairs field = filter (not . (isFreeCell field) . fst) pairs

groupByFirst :: Ord a => [(a, b)] -> Mm.MultiMap a b
groupByFirst = foldl (\mm (k,v) -> Mm.insert k v mm) Mm.empty

groupByNeighbour :: [CellPair] -> Mm.MultiMap Pos Pos
groupByNeighbour = groupByFirst . (map Tu.swap)

getIntel :: Intel -> Pos -> Int
getIntel i p = case M.lookup p i of
                 Just x -> x
                 Nothing -> 0

-- (Map MinePosition [Neighbours]) -> Neighbour/intel cell -> list of other Neighbour/intel cells affected
getLinkedNeighbours :: Mm.MultiMap Pos Pos -> Pos -> [Pos]
getLinkedNeighbours mineToNeighbour pos = Mm.lookup mineToNeighbour

-- pos->True mine, pos->False no mine
-- (Fixes free cells in combination as well as taken ones
-- since any changes to chosen set will make it inconsistent.
-- So we do not change existing layout during test)
type MineLayout = Map Pos Bool

-- Every program should ensure this.
justIsTrue x = case x of
                 Just _ -> True
                 Nothing -> False

-- Find some mine combinations that are consistent with known Intel
-- (Map Neighbour [Mine positions]) -> intel -> consistent mine placements
consistentCombinations :: Mm.MultiMap Pos Pos -> Mm.MultiMap Pos Pos -> Intel -> [Set Pos]
consistentCombinations mineToNeighbour neighbourToMine intel =
  let
      -- We cannot enumerate all combinations except with smallest cases. Using heuristics.
      -- TODO Try cells with least number of combinations first (sort by asc choiceCount)
      -- orderedRels = L.sortBy (\x y -> compare (snd y) (snd x)) $ Mm.toList neighbourToMine

      -- (A) TODO Calculate all linked neighbourhood then try to satisfy it
      -- -- (Doing this in 2 separate steps would simplify this function)
      -- TODO Then we could use:
      -- -- 1. List of linked neighbours.
      -- -- 2. Key set of accumulated MineLayout - to see which positions are already taken while generating new layouts.
      -- -- 3. neighbourToMine - to generate consistent layouts together with 2.
      -- TODO If after picking linked neighbours remain some more then repeat procedure (A)
      -- TODO Move constrainedCombos to top level?
      constrainedCombos :: Mm.Multimap Pos Pos -> Intel -> MineLayout -> [Pos] -> Maybe MineLayout
      -- Seed testPositions with single item list. testPositions - neigbours to be satisfied by intel
      constrainedCombos nToM intel currentLayout testPositions@(testPos : tps) =
        -- Generate only those combinations that are consistent in this position.
        if isFeasible
          -- TODO Let's try returning single layout first. Then we could rewrite this to find all suitable.
          then L.find justIsTrue $ fmap testCombo tpCombinations
          else Nothing
        where
          linkedMinePoss = (M.lookup testPos nToM)
          tpIntel = getIntel intel testPos
          -- Find related mine positions that are already in layout by splitting into (taken, available)
          lmp = L.partition (\p -> M.member p currentLayout) linkedMinePoss
          takenCount = L.foldl (\x -> M.findWithDefault 0 x currentLayout) 0 (fst lmp)
          isFeasible = (tpIntel - takenCount) <= (L.length (snd lmp))
          tpCombinations = combinations (tpIntel - takenCount) (snd lmp)

          linkedNeighbours = S.fromList $ concatMap getLinkedNeighbours linkedMinePoss
          -- In case of cycle we should just get empty combo and move on
          -- (if this does not work - find other test to avoid already visited neighbours)
          -- Recursive call returning some consistent layout:
          testCombo posLayout = constrainedCombos nToM intel
                        (M.union currentLayout posLayout)
                        (L.concat linkedNeighbours testPositions)

  in constrainedCombos orderedRels S.empty

-- permutations n = sequence . replicate n

-- http://stackoverflow.com/a/22577148/117220
combinations :: Int -> [a] -> [[a]]
combinations k xs = combinations' (length xs) k xs
  where combinations' n k' l@(y:ys)
          | k' == 0   = [[]]
          | k' >= n   = [l]
          | null l    = []
          | otherwise = map (y :) (combinations' (n - 1) (k' - 1) ys) ++ combinations' (n - 1) k' ys

-- Binomial coefficient
-- http://stackoverflow.com/a/6806997/117220
choiceCount :: Int -> Int -> Int
choiceCount n 0 = 1
choiceCount 0 k = 0
choiceCount n k = chooseCount (n-1) (k-1) * n `div` k

-- It would be better to have this in multimap lib
instance (Show a, Show b) => Show (Mm.MultiMap a b) where
  show = show . Mm.toMap

-----------------------------------------------------------

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
  let intel = filterLayer (genIntel mines) field
      probePos = (chooseProbePosition fieldSize field intel)
      newField = step mines field probePos
      showFinalStatus message = do
        showStatus fieldSize message
        return ()
  in do
    -- CC.threadDelay 300000
    clearScreen
    renderBoard field intel mines
    moveCursorBelow fieldSize -- Move it away so it does not obstruct cells

    -------------------------
    let intelRel = intelMatrix fieldSize (enumPositions fieldSize)
    let vim = (visibleIntelMatrix intelRel field)
    putStrLn $ show $ vim
    let edgeRelations = groupByNeighbour $ probableMinesMatrix vim field
    putStrLn $ show $ edgeRelations
    putStrLn $ show $ consistentCombinations edgeRelations intel
    -------------------------

    c <- getChar

    case newField of
      Nothing -> showFinalStatus ("Tripped on mine at " ++ (show probePos))
      Just f ->
        if isGameComplete mines f
          then showFinalStatus "Done"
          else gameStep fieldSize mines f

main :: IO ()
main =
  let dims = (8, 4)
  in do
    clearScreen
    mines <- genMines dims 5
    gameStep dims mines (genField dims)
