{-# OPTIONS_GHC -Wall #-}
-- {-# OverloadedStrings #-}
-- https://stackoverflow.com/questions/2086842/using-haskell-to-output-a-utf-8-encoded-bytestring
-- http://www.stephendiehl.com/what/
-- c3 = 'ダ'

module Main where
import Data.Map (Map)
import qualified Data.Map as Map



-- Split number into it's last digit and remainder: 1234 -> (123, 4)
splitLastDigit :: Integer -> Integer -> (Integer, Integer)
splitLastDigit x base = (div x base, mod x base)

-- Decimal dIgits of given number in reverse order
intToDigits :: Integer -> [Integer]
intToDigits x
  | x < 0 = undefined
  | x == 0  = []
  | otherwise = d : (intToDigits r)
                where (r, d) = splitLastDigit x 10

-- Double numbers at even positions (second, fourth, ...)
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x : []) = [x]
doubleEveryOther (x : y : xs) = x : (y * 2) : doubleEveryOther xs

luhnSum :: Integer -> Integer
luhnSum x = sum $ concatMap intToDigits $ doubleEveryOther $ intToDigits x

luhnIsValid :: Integer -> Bool
luhnIsValid x = mod (luhnSum x) 10 == 0

type Peg = String
type Move = (Peg, Peg)

-- Move disks from first peg to second
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _1 _2 _3 = []
hanoi 1 pa pb _ = [(pa, pb)]
hanoi x pa pb pc = (hanoi (x - 1) pa pc pb)
                   ++ (hanoi 1 pa pb pc)
                   ++ (hanoi (x - 1) pc pb pa)

just :: String -> Maybe a -> a
just message x = case x of
                    Just v -> v
                    Nothing -> error message

type PegState = [Integer]
type HanoiState = Map Peg PegState

validatePeg :: PegState -> PegState
validatePeg [] = []
validatePeg [x] = [x]
validatePeg (x : xs)
  | x > (head xs) = error "Peg state is invalid: bigger disk on top"
validatePeg state = state

hanoiInterpretMove :: HanoiState -> Move -> HanoiState
hanoiInterpretMove state (px, py) = Map.insert px rest
                                    $ Map.adjust (\l -> top : l) py state
                                    where (top : rest) = state Map.! px



initialHanoiState :: Integer -> String -> String -> String -> HanoiState
initialHanoiState n pa pb pc = Map.fromList [(pa, [1..n]), (pb, []), (pc, [])]

hanoiInterpret :: Integer -> String -> String -> String -> [Move] -> [HanoiState]
hanoiInterpret n pa pb pc moves = reverse $ foldl
                            (\ results move -> (hanoiInterpretMove (head results) move) : results)
                            [(initialHanoiState n pa pb pc)]
                            moves

hanoiShow :: Integer -> [HanoiState]
hanoiShow n = hanoiInterpret n pa pb pc $ hanoi n pa pb pc
              where pa = "a"
                    pb = "b"
                    pc = "c"


