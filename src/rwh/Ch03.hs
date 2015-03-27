{-# LANGUAGE TemplateHaskell #-} -- For Test.QuickCheck.All
{-# OPTIONS_GHC -Wall #-}
module Ch03 where

import Data.Function
import Data.List
-- import Data.Map

import Test.QuickCheck
import qualified Test.QuickCheck.All ()

-- http://stackoverflow.com/questions/7108559/how-to-find-the-frequency-of-characters-in-a-string-in-haskell
frequencies :: Ord a => [a] -> [(a, Int)]
frequencies l = map (\x -> (head x, length x)) $ group $ sort l

sortByKey :: Ord b => (a -> b) -> [a] -> [a]
sortByKey key l = sortBy (on compare key) l


-- Write a function that computes the number of elements in a list.
-- To test it, ensure that it gives the same answers as the standard length function.
listLength :: [a] -> Int
listLength [] = 0
listLength (_:xs) = succ (listLength xs)

prop_listLength :: [a] -> Bool
prop_listLength l = (listLength l) == (length l)

-- Write a function that computes the mean of a list,
-- i.e. the sum of all elements in the list divided by its length.
average :: (Num a, Fractional a) => [a] -> a
average l = (sum l) / fromIntegral (length l)


-- Turn a list into a palindrome, i.e. it should read the same both backwards and forwards.
-- For example, given the list [1,2,3], your function should return [1,2,3,3,2,1].
palindrome :: [a] -> [a]
palindrome l = l ++ (reverse l)

prop_palindromeLen :: [a] -> Bool
prop_palindromeLen l = (length l) * 2 == length (palindrome l)

prop_palindromeChars :: (Eq a) => [a] -> Bool
prop_palindromeChars l = let p = (palindrome l)
                             half = div (length p) 2
                             pairs = zip (take half p) (take half (reverse p))
                         in all (uncurry (==)) pairs

-- Write a function that determines whether its input list is a palindrome
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome l = all (uncurry (==)) $ zip l (reverse l)

prop_isPalindrome :: (Eq a) => [a] -> Bool
prop_isPalindrome l = isPalindrome (palindrome l)

prop_isPalindrome0 :: Bool
prop_isPalindrome0 = isPalindrome ([] :: [Char])

prop_isPalindromeNot :: Bool
prop_isPalindromeNot = not (isPalindrome "abc")

-- Create a function that sorts a list of lists based on the length of each sublist.

sortByLength :: [[a]] -> [[a]]
sortByLength l = sortByKey length l

-- Define a function that joins a list of lists together using a separator value.
interpose :: a -> [a] -> [a]
interpose _ [] = []
interpose _ [x] = [x]
interpose sep (x:xs) = [x, sep] ++ (intersperse sep xs)

prop_interspersePose :: Eq a => a -> [a] -> Bool
prop_interspersePose s l = (interpose s l) == (intersperse s l)


-- Using the binary tree type that we defined earlier in this chapter,
-- write a function that will determine the height of the tree.
data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving (Show)

depth :: Tree a -> Int
depth Empty = 0
depth (Node _ l r) = 1 + (max (depth l) (depth r))

prop_depth :: Bool
prop_depth = and [
  2 == (depth $ Node "x" Empty (Node "y" Empty Empty)),
  1 == (depth $ Node "x" Empty Empty)]


-- Consider three two-dimensional points a, b, and c. If we look at the angle formed by the line segment
-- from a to b and the line segment from b to c, it either turns left, turns right,
-- or forms a straight line. Define a Direction data type that lets you represent these possibilities.
data Point a = Point a a
               deriving (Show, Eq)
data Direction = DStraight
               | DLeft
               | DRight
               deriving (Show, Eq)
data Vec a = Vec a a
             deriving (Show, Eq)

det2 :: Num a => Vec a -> Vec a -> a
det2 (Vec x1 y1) (Vec x2 y2) = x1 * y2 - y1 * x2

dotProd2 :: Num a => Vec a -> Vec a -> a
dotProd2 (Vec x1 y1) (Vec x2 y2) = x1 * x2 + y1 * y2

toVector :: Num a => Point a -> Point a -> Vec a
toVector (Point x1 y1) (Point x2 y2) = Vec (x2 - x1) (y2 - y1)

angleLike :: (Fractional a) => Vec a -> Vec a -> a
angleLike a b = (det2 a b) / (dotProd2 b b)

signumToDirection :: (Num a, Ord a) => a -> Direction
signumToDirection x
  | x > 0 = DLeft
  | x < 0 = DRight
  | otherwise = DStraight

-- Write a function that calculates the turn made by three 2D points and returns a Direction.
angleDirection :: (Num a, Ord a) => Point a -> Point a -> Point a -> Direction
angleDirection a b c = let ab = toVector a b
                           bc = toVector b c
                       in signumToDirection (det2 ab bc)

prop_angleDirection :: (Num a, Ord a) => a -> Bool
prop_angleDirection x = (signumToDirection x) == (angleDirection a b c)
                        where a = (Point 0 0)
                              b = (Point 1 0)
                              c = (Point 2 x)

prop_angleDirection2 :: (Num a, Ord a) => a -> Bool
prop_angleDirection2 x = (signumToDirection x) == (angleDirection a b c)
                        where a = (Point 0 0)
                              b = (Point 1 (-x))
                              c = (Point 2 0)

-- Define a function that takes a list of 2D points and computes the direction of each successive triple.
-- Given a list of points [a,b,c,d,e], it should begin by computing the turn made by [a,b,c],
-- then the turn made by [b,c,d], then [c,d,e]. Your function should return a list of Direction.
angleDirections :: (Num a, Ord a) => [Point a] -> [Direction]
angleDirections ps = zipWith3 angleDirection ps (drop 1 ps) (drop 2 ps)

prop_angleDirections_rights :: Int -> Bool
prop_angleDirections_rights n = all ((==) DRight) (angleDirections ps)
  where ps = take n (cycle [Point (0::Int) 0, Point 0 1, Point 1 1, Point 1 0])

-- Using the code from the preceding three exercises, implement Graham's scan algorithm
-- for the convex hull of a set of 2D points. You can find good description of what a convex hull is,
-- and how the Graham scan algorithm should work, on Wikipedia https://en.wikipedia.org/wiki/Graham_scan
convexHull :: (Fractional a, Ord a) => [Point a] -> [Point a]
-- XXX This can be optimized for speed.
convexHull [] = []
convexHull ps = let start = head $ sortByKey (\(Point x y) -> (x, y)) ps
                    ordered = sortByKey (\p -> angleLike (Vec 1 0) (toVector start p)) $ tail ps
                in grahamScan $ concat [ordered, [start]]

grahamScan :: (Fractional a, Ord a) => [Point a] -> [Point a]
grahamScan [] = []
grahamScan [a] = [a]
grahamScan [a, b] = [a, b]
grahamScan (a : b : c : ps) = a : (if DRight == (angleDirection a b c)
                                   then grahamScan $ c : ps
                                   else grahamScan $ b : c : ps)


-- Tests

runTests :: IO Bool
runTests = $quickCheckAll
