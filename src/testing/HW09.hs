-- (For Autumn 2014 version of course)

-- {-# LANGUAGE MonadComprehensions, RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
module HW09 where

import Ring
import Test.QuickCheck
import qualified Test.QuickCheck.All as TQA
import Test.HUnit

import Control.Applicative
import Control.Monad
import qualified System.Random as Rand

instance Arbitrary Mod5 where
  -- Type annotation is not neccessary here, just for clarity.
  arbitrary = (arbitrary :: Gen Integer) >>= (\n -> return $ mkMod n)

instance Arbitrary Mat2x2 where
  -- Type annotation is not neccessary here, just for clarity.
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ MkMat a b c d

  shrink (MkMat a b c d) = map
    (\(a', b', c', d') -> MkMat a' b' c' d')
    (shrink (a,b,c,d))


--------
-- R is an abelian group under addition
--------

prop_plusIsAssociative :: (Ring a, Eq a) => a -> a -> a -> Bool
prop_plusIsAssociative a b c = (a `add` b) `add` c == a `add` (b `add` c)

prop_zeroIsAdditiveIdentity :: (Ring a, Eq a) => a -> Bool
prop_zeroIsAdditiveIdentity a = ((a `add` addId) == a) && (a == (a `add` addId))

prop_unaryMinusGivesInverse :: (Ring a, Eq a) => a -> Bool
prop_unaryMinusGivesInverse a = ((a `add` (addInv a)) == addId)

prop_plusIsCommutative :: (Ring a, Eq a) => a -> a -> Bool
prop_plusIsCommutative a b = a `add` b == b `add` a

--------
-- R is a monoid under multiplication
--------

prop_multipyIsAssociative :: (Ring a, Eq a) => a -> a -> a -> Bool
prop_multipyIsAssociative a b c = (a `mul` b) `mul` c == a `mul` (b `mul` c)

prop_unitIsMultiplicativeIdentity :: (Ring a, Eq a) => a -> Bool
prop_unitIsMultiplicativeIdentity a = (a `mul` mulId == a) && (mulId `mul` a == a)

--------
-- Multiplication distributes over addition:
--------

prop_leftDistributivity :: (Ring a, Eq a) => a -> a -> a -> Bool
prop_leftDistributivity a b c = a `mul` (b `add` c) == (a `mul` b) `add` (a `mul` c)

prop_rightDistributivity :: (Ring a, Eq a) => a -> a -> a -> Bool
prop_rightDistributivity a b c = (b `add` c) `mul` a == (b `mul` a) `add` (c `mul` a)

-- I do not know what's the 9-th property they mentioned. :(

-- See also https://hackage.haskell.org/package/QuickCheck-2.7.6/docs/Test-QuickCheck-All.html
--  http://tab.snarc.org/posts/haskell/2010-12-02-using_quickcheck.html , use .&&. here?
prop_Zahlring :: (Ring a, Eq a) => a -> a -> a -> Property
prop_Zahlring a b c = conjoin [
    prop_plusIsAssociative a b c,
    prop_zeroIsAdditiveIdentity a,
    prop_unaryMinusGivesInverse a,
    prop_plusIsCommutative a b,
    prop_multipyIsAssociative a b c,
    prop_unitIsMultiplicativeIdentity a,
    prop_leftDistributivity a b c,
    prop_rightDistributivity a b c]

prop_matrix2x2 :: Mat2x2 -> Mat2x2 -> Mat2x2 -> Property
prop_matrix2x2 = prop_Zahlring

prop_mod5 :: Mod5 -> Mod5 -> Mod5 -> Property
prop_mod5 = prop_Zahlring

prop_integer :: Integer -> Integer -> Integer -> Property
prop_integer = prop_Zahlring

-- !!! Bool ring is not correct: Addition inverse does not hold
-- prop_bool :: Bool -> Bool -> Bool -> Property
-- prop_bool = prop_Zahlring

runTests = $quickCheckAll

-- ------------------------------------------------------
-- Binary search trees (ex. 6..8)

data BST a = Leaf
           | Node (BST a) a (BST a)
  deriving Show

-- | Is the tree a BST between the given endpoints?
isBSTBetween :: (Ord a) =>
             Maybe a     -- ^ lower bound, if one exists
             -> Maybe a  -- ^ upper bound, if one exists
             -> BST a    -- ^ tree to test
             -> Bool
isBSTBetween _       _       Leaf = True
isBSTBetween m_lower m_upper (Node left x right)
  = isBSTBetween m_lower  (Just x) left  &&
    isBSTBetween (Just x) m_upper  right &&
    case m_lower of
      Just lower -> lower <= x
      Nothing    -> True
    &&
    case m_upper of
      Just upper -> x <= upper
      Nothing    -> True

-- | Is this a valid BST?
isBST :: (Ord a) => BST a -> Bool
isBST = isBSTBetween Nothing Nothing


-- | Get a list of the elements in sorted order
getElements :: BST a -> [a]
getElements Leaf                = []
getElements (Node left x right) = getElements left ++ x : getElements right

instance (Arbitrary a, Ord a, Enum a, Rand.Random a) => Arbitrary (BST a) where
  arbitrary = do
    l <- arbitrary
    u <- arbitrary
    genBST l u

prop_ordered :: BST Int -> Bool
prop_ordered x = isBST x == is_sorted (getElements x)
  where
    is_sorted []             = True
    is_sorted [_]            = True
    is_sorted (x1 : x2 : xs) = x1 <= x2 && is_sorted (x2 : xs)

-- http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html
genBST :: (Arbitrary a, Ord a, Enum a, Rand.Random a) => a -> a -> Gen (BST a)
genBST l u = do
  genLeaf <- arbitrary
  if genLeaf
    then return Leaf
    else do
      x <- choose (l, u)
      left <- genBST l (pred x)
      right <- genBST (succ x) u
      return $ Node left x right

--------------------------------------------

type Name = String

data Employee = Employee { name    :: Name
                         , phone   :: String }
                deriving Show

m_name1, m_name2 :: Maybe Name
m_name1 = Nothing
m_name2 = Just "Brent"

m_phone1, m_phone2 :: Maybe String
m_phone1 = Nothing
m_phone2 = Just "555-1234"

ex01, ex02, ex03, ex04 :: Maybe Employee
ex01 = Employee <$> m_name1 <*> m_phone1
ex02 = Employee <$> m_name1 <*> m_phone2
ex03 = Employee <$> m_name2 <*> m_phone1
ex04 = Employee <$> m_name2 <*> m_phone2


------------------
-- HUnit

testParsableInteger :: Test
testParsableInteger = TestCase
  (assertEqual "Parse integer" (Just (13::Integer)) (parseAll "13"))

testParsableMod5 :: Test
testParsableMod5 = TestCase
  (assertEqual "Parse mod5"  (Just $ MkMod 3) (parseAll "13"))

testParsableMat2x2 :: Test
testParsableMat2x2 = TestCase
  (assertEqual "Parse mat2x2"  (Just $ MkMat 1 2 3 4) (parseAll "[[1,2][3,4]]"))

testParsableBool :: Test
testParsableBool = TestList [
  TestCase
    (assertEqual "Parse True"  (Just True) (parseAll "True")),
  TestCase
    (assertEqual "Parse False"  (Just False) (parseAll "False"))]

hUnitTests :: Test
hUnitTests = TestList [
  testParsableInteger,
  testParsableMod5,
  testParsableMat2x2,
  testParsableBool]