-- http://www.seas.upenn.edu/~cis194/lectures.html
-- 4th assignment
{-# OPTIONS_GHC -Wall #-}
-- {-# OPTIONS_GHC -W #-}
module Polynomial where

import Data.List

newtype Poly a = P [a]

padWith :: a -> [a] -> [a] -> [(a, a)]
padWith val a b = take (max (length a) (length b))
                        $ zip (a ++ (repeat val)) (b ++ (repeat val))

instance (Num a, Eq a) => Eq (Poly a) where
  (P x1) == (P x2) = all (uncurry (==)) $ padWith 0 x1 x2

coeffStr :: (Show a, Eq a, Num a) => a -> String
coeffStr 1 = ""
coeffStr (-1) = "-"
coeffStr c = show c

showMember :: (Show a, Eq a, Num a) => a -> Int -> String
showMember 0 _ = ""
showMember c 0 = show c
showMember c 1 = (coeffStr c) ++ "x"
showMember c p = (coeffStr c) ++ "x^" ++ (show p)

showP :: (Num a, Eq a, Show a) => [a] -> String
showP [] = "0"
showP [c] = show c
showP cs = intercalate " + " $ filter (not . null) memberStrs
               where mapMember (pow, lst) c = (pow + 1, (showMember c pow) : lst)
                     (_, memberStrs) = foldl mapMember (0, []) cs

instance (Num a, Eq a, Show a) => Show (Poly a) where
  show (P cs) = showP cs

withPowers :: [a] -> [(Integer, a)]
withPowers = zip [0..]

plusC :: Num a => [a] -> [a] -> [a]
plusC q y = map (uncurry (+)) $ padWith 0 q y

type RowMatrix a = [(a, [a])]

toMatrix :: Num a => [a] -> [a] -> RowMatrix a
toMatrix q y = map (\a -> (a, y)) q

multiplyC_slice :: Num a => Int -> RowMatrix a -> ([a], RowMatrix a)
multiplyC_slice n m = let (toMap, toKeep) = splitAt n m
                          splitted = map (\(a, bs) -> (map ((*) a) $ take 1 bs, (a, drop 1 bs))) toMap
                      in (concat $ map fst splitted, map snd splitted ++ toKeep)

-- Multiply polynomial coefficients
multiplyC :: Num a => [a] -> [a] -> [a]
multiplyC q y = reverse
                $ snd $ foldl nextCoeff (toMatrix q y, []) [1..((length q) + (length y) - 1)]
                where nextCoeff (m, prod) p =
                        let (slice, newM) = multiplyC_slice p m
                        in (newM, (sum slice) : prod)

-- Negate polynomial coefficients -- TODO Use multiplication (default impl) instead?
negateC :: Num a => [a] -> [a]
negateC = map (* (-1))

-- TODO Is there a way to not repeat polynomial desctructure and construction?
instance Num a => Num (Poly a) where
  (P q) + (P y) = P $ plusC q y
  (P q) * (P y) = P $ multiplyC q y
  negate (P q) = P $ negateC q
  fromInteger q = P [fromInteger q]
  abs = undefined
  signum = undefined

applyP :: Num a => Poly a -> a -> a
applyP (P cs) q = sum $ map (\(p, c) -> c * q^p) $ withPowers cs

class Num a => Differentiable a where
  deriv :: a -> a
  nderiv :: Int -> a -> a

  nderiv 0 a = a
  nderiv n a = nderiv (n - 1) (deriv a)

instance Num a => Differentiable (Poly a) where
  deriv (P cs) = P $ map (\(p, c) -> c * (fromIntegral p)) $ drop 1 $ withPowers cs

x :: Num a => Poly a
x = P [0, 1]

runTest :: (Int, [Bool])
runTest = let results = [
                P [1::Int, 2] /= P [1, 2, 3],
                P [1::Int, 2] == P [1, 2, 0],
                P [2::Int, 1] == P [2, 1],
                P ([]::[Int]) == P [],

                show (P [0::Int, 1]) == "x",
                show (P [1::Int, 0, 0, 2]) == "2x^3 + 1",
                show (P [0::Int, -1, 2]) == "2x^2 + -x",

                P [5::Int, 0, 1] + P [1, 1, 2] == P [6, 1, 3],
                P [1::Int, 0, 1] + P [1, 1] == P [2, 1, 1],

                P [1::Int, 2] * P [6, 7] == P [6, 19, 14],
                P [1::Int, 1, 1] * P [2, 2] == P [2, 4, 4, 2],

                applyP (x^(2::Int) + 2*x + 1) (1::Int) == 4,
                applyP (x^(2::Int) + 2*x + 1) (2::Int) == 9,

                deriv (x^(2::Int) + 3*x + 5) == 2*(x::Poly Int) + 3]
          in (length $ filter not results, results)
