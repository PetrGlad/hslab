-- http://www.seas.upenn.edu/~cis194/lectures.html
-- 3rd assignment

{-# OPTIONS_GHC -Wall #-}
module Interpreter where

-- import Data.Function
-- import qualified Data.Map as Map

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop =
    Plus
  | Minus
  | Times
  | Divide
  | Gt
  | Ge
  | Lt
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend state varName value = (\name -> if name == varName
                                     then value
                                     else state name)

empty :: State
empty = (\_ -> 0)

-- Exercise 2 -----------------------------------------

intToBool :: Int -> Bool
intToBool 0 = False
intToBool _ = True

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

binFnAs :: (a -> b -> c) -> (c -> d) -> (a -> b -> d)
binFnAs f r = (\ x y -> r $ f x y)

boolToIntF :: (Int -> Int -> Bool) -> (Int -> Int -> Int)
boolToIntF f = binFnAs f boolToInt

bopFunction :: Bop -> Int -> Int -> Int
bopFunction Plus = (+)
bopFunction Minus = (-)
bopFunction Times = (*)
bopFunction Divide = div
bopFunction Gt = boolToIntF (>)
bopFunction Ge  = boolToIntF (>=)
bopFunction Lt =  boolToIntF (<)
bopFunction Le =  boolToIntF (<=)
bopFunction Eql = boolToIntF (==)

evalE :: State -> Expression -> Int
evalE state (Var varName) = state varName
evalE state (Val intVal) = intVal
evalE state (Op exprA bop exprB) = bopFunction bop (evalE state exprA) (evalE state exprB)

-- Exercise 3 -----------------------------------------

data DietStatement =
    DAssign String Expression
  | DIf Expression DietStatement DietStatement
  | DWhile Expression DietStatement
  | DSequence DietStatement DietStatement
  | DSkip
  deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign name expr) = DAssign name expr
desugar (Incr name) =
  DAssign name $ Op (Var name) Plus (Val 1)
desugar (If condExpr thenStat elseStmt) =
  DIf condExpr (desugar thenStat) (desugar elseStmt)
desugar (While condExpr body) = DWhile condExpr (desugar body)
desugar (For setup loopCond incr body) =
  DSequence (desugar setup)
            $ DWhile loopCond
                   $ DSequence (desugar body)
                               (desugar incr)
desugar (Sequence statA statB) = DSequence (desugar statA) (desugar statB)
desugar Skip = DSkip

-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple state (DAssign name expr) = extend state name (evalE state expr)
evalSimple state (DIf expr thenStmt elseStmt) =
  if intToBool (evalE state expr)
    then (evalSimple state thenStmt)
    else (evalSimple state elseStmt)
evalSimple state whileStmt @ (DWhile expr body) =
  if intToBool (evalE state expr)
    then (evalSimple (evalSimple state body) whileStmt)
    else state
evalSimple state (DSequence stmtA stmtB) = evalSimple (evalSimple state stmtA) stmtB
evalSimple state DSkip = state

run :: State -> Statement -> State
run state stmt = evalSimple state (desugar stmt)

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

prog1 :: Statement
prog1 = (Assign "Out" (Var "In"))

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]


-- Tests

runTest :: (Int, [Bool])
runTest = let results = [
                (run (extend empty "In" 4) factorial) "Out" == 24,
                (run (extend empty "In" 0) factorial) "Out" == 1,
                (run (extend empty "A" 0) squareRoot) "B" == 0,
                (run (extend empty "A" 9) squareRoot) "B" == 3,
                (run (extend empty "In" 1) fibonacci) "Out" == 1,
                (run (extend empty "In" 4) fibonacci) "Out" == 5]
          in (length $ filter not results, results)
