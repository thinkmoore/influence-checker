import System.Environment

import Parse
import Pretty

main :: IO ()
main =
  do args <- getArgs
     cmd <- parseFile $ head args
     prettyPrint cmd
