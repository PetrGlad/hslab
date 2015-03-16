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

swapVunsafe :: Int -> Int -> Vector a -> Vector a
swapVunsafe i j v = v // [(i, (v!j)), (j,(v!i))]

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
shuffle v = foldM (\w i -> getRandomR (0, (i-1)) >>= (\j -> return $ swapVunsafe i j w))
            v
            (reverse [1..((V.length v) - 1)]) -- (going from 1 to n - 2 would be as good)

-- Exercise 6 -----------------------------------------

partitionAt :: Ord a => Vector a -> Int -> (Vector a, Vector a, Vector a)
-- XXX Could be faster, using simplest algorithm for now
-- Note signature differs from one in
partitionAt v i = let x = v ! i
                  in (V.filter ((>) x) v,
                      V.filter ((==) x) v,
                      V.filter ((<) x) v)

-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])

qsort :: Ord a => Vector a -> Vector a
qsort v
  | (V.length v) == 0 = V.empty
  | otherwise = let (l, x, r) = partitionAt v 0
                in V.concat [(qsort l), x, (qsort r)]

-- Exercise 8 -----------------------------------------

qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR v
  | (V.length v) == 0 = return V.empty
  | otherwise = getRandomR (0, (V.length v) - 1)
                >>= (\at -> let (l, x, r) = (partitionAt v at)
                            in return $  (qsort l) <> x <> (qsort r))

-- Exercise 9 -----------------------------------------

-- Selection
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
-- Rank is 0 based
select i v
  | (V.length v) == 0 = return Nothing
  | (V.length v) <= i = return Nothing -- Optimization, not required
  | otherwise = do at <- getRandomR (0, (V.length v) - 1)
                   recur (partitionAt v at)
  where recur (l, x, r)
          | i < (V.length l) = (select i l)
          | i < (V.length l) + (V.length x) = return $ Just (x ! 0)
          | otherwise = (select (i - (V.length l) - (V.length x)) r)

-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = do
           l <- labels
           s <- suits
           return $ Card l s

newDeck :: Rnd Deck
newDeck = shuffle allCards

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard d
    | V.length d == 0 = Nothing
    | otherwise = Just (V.head d, V.tail d)

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
                getElts [21, 3] (V.fromList [(0::Int)..9]) == Nothing,
                partitionAt (V.fromList [5::Int, 2, 8, 3, 6, 1]) 3 ==
                  (V.fromList [2, 1], V.singleton 3, V.fromList [5, 8, 6]),
                partitionAt (V.fromList [1::Int, 6, 4, 7, 2, 4]) 2 ==
                  (V.fromList [1, 2], V.fromList [4, 4], V.fromList [6, 7, 4])]
          in (length $ filter not results, results)

