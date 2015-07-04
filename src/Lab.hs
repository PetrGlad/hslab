module Lab where

import Data.List
import Data.Char (digitToInt, isNumber)

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith piecePred [] = []
splitWith piecePred xs = let breakPred = (not . piecePred)
                             (piece, pieces) = (break breakPred (dropWhile breakPred xs))
                         in piece : (splitWith piecePred pieces)

firstWordInEachLine = (map (head . words) . lines)

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f z xs = (foldr step id xs) z
    where step x g a = g (f a x)

asInt_fold :: String -> Int
asInt_fold "" = error "Empty input"
asInt_fold cs = foldl step 0 cs
                where step v c = v * 10 + (digitToInt c)

type ErrorMessage = String
asInt_either :: String -> Either ErrorMessage Int
asInt_either "" = Left "Empty input"
asInt_either cs = foldl step (Right 0) cs
                where dtoi c
                        | isNumber c = Right $ digitToInt c
                        | otherwise = Left $ "'" ++ [c] ++ "' is not a number"
                      step (Right v) c =
                        case dtoi c of
                          Left e -> Left e
                          Right n -> Right $ v * 10 + n
                      step e@(Left _) _ = e

-- file: ch04/InteractWith.hs
-- Save this in a source file, e.g. Interact.hs
-- import System.Environment (getArgs)
--
-- interactWith function inputFile outputFile = do
--   input <- readFile inputFile
--   writeFile outputFile (function input)
--
-- main = mainWith myFunction
--   where mainWith function = do
--           args <- getArgs
--           case args of
--             [input,output] -> interactWith function input output
--             _ -> putStrLn "error: exactly two arguments needed"
--
--         myFunction = id