{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Data.List
import Data.Functor

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = (fib (n - 1)) + (fib (n - 2))

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 -----------------------------------------

fibN :: Integer -> [Integer]
fibN 0 = 1 : []
fibN 1 = 1 : fibN 0
fibN n = let fs @ (f1 : f2 : _) = fibN (n - 1)
         in (f1 + f2) : fs

fibs2 :: [Integer]
fibs2 = 1 : 1 : (zipWith (+) (tail fibs2) fibs2)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons v vs) = v : (streamToList vs)

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap f (Cons v vs) = Cons (f v) (fmap f vs)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat v = Cons v (sRepeat v)

sIterate :: (a -> a) -> a -> Stream a
sIterate f x = Cons x (sIterate f (f x))

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons v vs) w = Cons v (sInterleave w vs)

sTake :: Int -> Stream a -> [a]
sTake n s = take n $ streamToList s

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate ((+) 1) 0

ruler :: Stream Integer
ruler = r 0
        where r n = sInterleave (sRepeat n) (r (n+1))

-- Exercise 7 -----------------------------------------

randN :: Int -> Int
randN n = mod (1103515245 * n + 12345) 2147483648

-- | Implementation of C rand
rand :: Int -> Stream Int
rand seed = sIterate randN seed

-- Exercise 8 -----------------------------------------

{- Total Memory in use: 205 MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: ??? MB -}
minMax0 :: [Int] -> Maybe (Int, Int)
minMax0 [] = Nothing
minMax0 [v] = Just (v, v)
minMax0 (x : xs) = let (Just (mn, mx)) = minMax0 xs
                  in (Just (min x mn, max x mx))

minMax1 :: [Int] -> Maybe (Int, Int)
minMax1 [] = Nothing
minMax1 (x : xs) = Just $ foldl (\(mn, mx) v -> (min v mn, max v mx))
                           (x, x) xs

minMaxD :: (Int, Int) -> [Int] -> (Int, Int)
minMaxD d [] = d
minMaxD (mn, mx) (x : xs) = minMaxD (min x mn, max x mx) xs

minMax2 :: [Int] -> Maybe (Int, Int)
minMax2 [] = Nothing
minMax2 (x : xs) = Just $ minMaxD (x, x) xs

-- ghc HW06.hs -rtsopts -main-is HW06
-- HW06 +RTS -s
-- HW06 +RTS -h -i0.001
-- hp2ps -c HW06.hp

-- Trying to make it even faster by removing lazyness
minMaxItr :: Int -> (Int -> Int) -> Int -> Int -> Int -> (Int, Int)
minMaxItr 0 _ _ mn mx = (mn, mx)
minMaxItr n itr prev !mn !mx =
  let x = itr prev
  in minMaxItr (n - 1) itr x (min x mn) (max x mx)

minMax3 :: Int -> (Int, Int)
minMax3 n = let r = 7666532
            in minMaxItr n randN r r r

main :: IO ()
----- main = print $ minMax2 $ sTake 1000000 $ rand 7666532
-- main = print $ minMax2 $ take 1000000 $ iterate randN 7666532
main = print $ minMax3 1000000

-- Exercise 10 ----------------------------------------

fastFib :: Int -> Integer
fastFib = undefined
