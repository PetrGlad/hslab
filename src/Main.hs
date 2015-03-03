{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)

-- primes :: [Integer]
-- primes = sieve [2..]
--         where sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p > 0]
silly :: IO ()
silly = do
    putStrLn "What's yout name?"
    name <- getLine
    putStrLn ("Hello, " ++ name ++ "!")

getWords :: IO [ByteString]
getWords = do
  ln <- BS.getLine
  return $ BS.split 32 ln -- 32 is the ASCII code for ' '

-- getWords' :: IO [ByteString]
-- getWords' = BS.split 32 <$> BS.getLine

main :: IO ()
main = do
  wds <- getWords
  print wds
  silly
