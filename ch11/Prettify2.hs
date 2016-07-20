
module Prettify2 where

import Prettyfy
import Test.QuickCheck

prop_empty_id x =
  (empty <> x == x)
  &&
  (x <> empty == x)
                 
