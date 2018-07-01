-- DAYSTROM main compiler


import qualified System.Environment
import qualified System.FilePath
import qualified Data.Char

import qualified DaystromParser

main :: IO ()
main =
        do
          args <- System.Environment.getArgs
          let fileName = case args of
                         [arg] -> arg
                         _ -> error "can only compile one file"
          -- Read the file (using lazy loading)
          file <- readFile fileName
          let lowercaseFile = map Data.Char.toLower file
          putStrLn ("'" ++ lowercaseFile ++ "'")
          let ast = DaystromParser.parse lowercaseFile

          putStrLn (show ast)
