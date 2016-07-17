--file: ch04/InteractWith.hs

import System.Environment (getArgs)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputfile (function input)
  
main = mainWith myFuncion
where mainWith function = do
        args <- getArgs
        case args of
          [input,output] -> interactWith function input output
          _ -> putStrLn "error: exactly two arguments needed"
          
        myFuncion = id

