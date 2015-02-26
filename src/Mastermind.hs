{-# OPTIONS_GHC -Wall #-}
module Mastermind where
-- import Control.Exception (assert)

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches a b = length $ filter (\ (x, y) -> x == y) $ zip a b

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors code = map (\ c -> length $ filter ((==) c) code) colors

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches a b = sum $ map (uncurry min) $ zip (countColors a) (countColors b)

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secret guess = let exactCount = (exactMatches secret guess) in
                       Move guess exactCount ((matches secret guess) - exactCount)

isConsistent :: Move -> Code -> Bool
isConsistent move @ (Move mCode _ _) code = move == (getMove code mCode)

filterCodes :: Move -> [Code] -> [Code]
filterCodes move codes = filter (isConsistent move) codes

allCodes :: Int -> [Code]
allCodes 0 = []
allCodes 1 = map (:[]) colors
allCodes len = concatMap (\l -> map (++ l) (allCodes 1)) (allCodes (len - 1))

-- Exercise 7 -----------------------------------------

solveWithOptions :: Code -> [Move] -> [Code] -> [Move]
solveWithOptions _code _tries [] = error "Cannot solve (are inital options exhaustive?)"
solveWithOptions code moves (try : remainingOptions)
  | code == try = newMoves
  | otherwise = solveWithOptions code newMoves $ filterCodes move remainingOptions
  where move = getMove code try
        newMoves = move : moves

solve :: Code -> [Move]
solve code = reverse $ solveWithOptions code [] $ allCodes $ length code

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined


-- Tests

runTest :: (Int, [Bool])
runTest = let results = [
                exactMatches [Red, Blue, Green, Yellow] [Blue, Green, Yellow, Red] == 0,
                exactMatches [Red, Blue, Green, Yellow] [Red, Purple, Green, Orange] == 2,
                countColors [Red, Blue, Yellow, Purple] == [1, 0, 1, 1, 0, 1],
                countColors [Green, Blue, Green, Orange] == [0, 2, 1, 0, 1, 0],
                matches [Red, Blue, Yellow, Orange] [Red, Orange, Orange, Blue] == 3,
                getMove [Red, Blue, Yellow, Orange] [Red, Orange, Orange, Blue]
                        == Move [Red, Orange, Orange, Blue] 1 2,
                isConsistent (Move [Red, Red, Blue, Green] 1 1) [Red, Blue, Yellow, Purple] == True,
                isConsistent (Move [Red, Red, Blue, Green] 1 1) [Red, Blue, Red, Purple] == False,
                solve [Green, Green] == [Move [Red,Red] 0 0,Move [Green,Green] 2 0],
                solve [Green, Blue] == [Move [Red,Red] 0 0,Move [Green,Green] 1 0,Move [Blue,Green] 0 2,Move [Green,Blue] 2 0]]
          in (length $ filter not results, results)
