-- file: ch11/Run.hs

import Prettify2
import Test.QuickCheck.All  
import Test.QuickCheck

  
main = do
  
    quickCheck prop_empty_id
    quickCheck prop_char
    quickCheck prop_text
    quickCheck prop_line
    quickCheck prop_double_d 
  
     
                  
                     
  