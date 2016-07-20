
module Prettify2 where

import Prettyfy
import QC
import Test.QuickCheck

prop_empty_id x =
  (empty <> x == x)
  &&
  (x <> empty == x)
                 
prop_char c = char c == Char c
prop_text s = text s == if null s then Empty else Text s
prop_line = line == Line
prop_double_d d = double d == text (show d)