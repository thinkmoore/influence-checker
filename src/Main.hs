import System.Environment

import Imp.Parse
import Imp.Pretty

main :: IO ()
main =
  do args <- getArgs
     cmd <- parseFile $ head args
     prettyPrint cmd
