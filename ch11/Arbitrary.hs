-- file: ch11/Arbitrary.hs
module Arbitrary where

import System.Random
import Test.QuickCheck

--class Arbitrary a where --データ生成器class
--  arbitrary :: Gen a 
--  elements :: [a] -> Gen a
--  choose :: Random a => (a, a) -> Gen a
--  oneof :: [Gen a] -> Gen a
  
data Ternary = Yes
             | No
             | Unknown
             deriving(Eq, Show)
                      
instance Arbitrary Ternary where
  arbitrary  = do
    n <- choose(0,2) :: Gen Int
    return $ case n of 
      0 -> Yes
      1 -> No
      _ -> Unknown
      
instance ( Arbitrary a, Arbitrary b ) => Arbitrary (a ,b) where     
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (x, y)
  
