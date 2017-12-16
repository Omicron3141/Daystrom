-- DAYSTROM main compiler


import qualified System.Environment
import qualified System.FilePath

import qualified DaystromParser

main :: IO ()
main =
        do
          args <- System.Enviroment.getArgs
          let fileName = case args of
                         [arg] -> arg
                         _ -> error "can only compile one file"
          -- Read the file (using lazy loading)
          file <- readFile fileName
          let ast = DaystromParser.parseProgram file

          putStrLn (show ast)
