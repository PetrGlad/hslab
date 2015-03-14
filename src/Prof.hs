module Main where

lazySum :: Num a => [a] -> a
lazySum = go 0
  where go acc []     = acc
        go acc (x:xs) = go (x + acc) xs

main = print (lazySum [1..1000000])

