{-# LANGUAGE MonadComprehensions, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module HW07 where

import Prelude hiding (mapM)
import Cards

import Control.Monad hiding (mapM, liftM)
import Control.Monad.Random
import Data.Functor
import Data.Monoid
import Data.Vector (Vector, cons, (!), (!?), (//))
import System.Random

import qualified Data.Vector as V


-- Exercise 1 -----------------------------------------

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f v = v >>= (\x -> return $ f x)

swapV' :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV' i j v = (v !? i) >>= (\a -> (v !? j) >>= (\b -> Just $ v // [(i, b), (j,a)]))

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV i j v = do
  a <- (v !? i)
  b <- (v !? j)
  return $ v // [(i, b), (j,a)]

-- Exercise 2 -----------------------------------------

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
-- XXX Foldr could be slow here (compared to foldl), used to keep resulting elements in same order
-- It turns out that I reinvented Control.Monad.sequence :)
mapM' f xs = foldr (\mb mbs -> (mb >>= (\b -> (mbs >>= (\bs -> return (b : bs))))))
                  (return [])
                  (map f xs)

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f xs = foldr (\mb mbs -> do
                     b <- mb
                     bs <- mbs
                     return $ b : bs)
                  (return [])
                  (map f xs)

getElts :: [Int] -> Vector a -> Maybe [a]
getElts ixs v = mapM ((!?) v) ixs

-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a)
randomElt v = (getRandomR (0, (V.length v) - 1)) >>= (\i -> return $ v !? i)

-- Exercise 4 -----------------------------------------

randomVec :: Random a => Int -> Rnd (Vector a)
-- See also evalRandIO
randomVec n = (replicateM n getRandom) >>= (\xs -> return $ V.fromList xs)

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR n r = (replicateM n $ getRandomR r) >>= (\xs -> return $ V.fromList xs)

-- Exercise 5 -----------------------------------------

shuffle :: Vector a -> Rnd (Vector a)
shuffle = undefined

-- Exercise 6 -----------------------------------------

partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt = undefined

-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])

qsort :: Ord a => Vector a -> Vector a
qsort = undefined

-- Exercise 8 -----------------------------------------

qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR = undefined

-- Exercise 9 -----------------------------------------

-- Selection
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select = undefined

-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = undefined

newDeck :: Rnd Deck
newDeck =  undefined

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard = undefined

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards = undefined

-- Exercise 13 ----------------------------------------

data State = State { money :: Int, deck :: Deck }

repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
  putStrLn "Would you like to play (y/n)?"
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with \ESC[32m$"
           ++ show money ++ "\ESC[0m"
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got \ESC[32m$"
                      ++ show money ++ "\ESC[0m"
          play = do
            putStrLn "How much do you want to bet?"
            amt <- read <$> getLine
            if amt < 1 || amt > money
            then play
            else do
              case getCards 2 deck of
                Just ([c1, c2], d) -> do
                  putStrLn $ "You got:\n" ++ show c1
                  putStrLn $ "I got:\n" ++ show c2
                  case () of
                    _ | c1 >  c2  -> repl $ State (money + amt) d
                      | c1 <  c2  -> repl $ State (money - amt) d
                      | otherwise -> war s{deck = d} amt
                _ -> deckEmpty
          war (State m d) amt = do
            putStrLn "War!"
            case getCards 6 d of
              Just ([c11, c21, c12, c22, c13, c23], d') -> do
                putStrLn $ "You got\n" ++ ([c11, c12, c13] >>= show)
                putStrLn $ "I got\n" ++ ([c21, c22, c23] >>= show)
                case () of
                  _ | c13 > c23 -> repl $ State (m + amt) d'
                    | c13 < c23 -> repl $ State (m - amt) d'
                    | otherwise -> war (State m d') amt
              _ -> deckEmpty 

main :: IO ()
main = evalRandIO newDeck >>= repl . State 100


-- Tests

runTest :: (Int, [Bool])
runTest = let results = [
                (swapV 1 2 (V.fromList [1::Int, 2, 3])) == Just (V.fromList [1, 3, 2]),
                (swapV 2 1 (V.fromList [1::Int, 2, 3])) == Just (V.fromList [1, 3, 2]),
                (swapV 2 20 (V.fromList [1::Int, 2, 3])) == Nothing,
                (swapV 10 0 (V.fromList [1::Int, 2, 3])) == Nothing,
                mapM Just [(1::Int)..10] == Just [1..10],
                getElts [1, 3] (V.fromList [(0::Int)..9]) == Just [1, 3],
                getElts [21, 3] (V.fromList [(0::Int)..9]) == Nothing]
          in (length $ filter not results, results)

