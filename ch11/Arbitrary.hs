-- file: ch11/Arbitrary.hs
module Arbitrary where

import System.Random
import Test.QuickCheck

class Arbitrary a where --データ生成器class
  arbitrary :: Gen a 
  elements :: [a] -> Gen a
  choose :: Random a => (a, a) -> Gen a
  oneof :: [Gen a] -> Gen a
  
