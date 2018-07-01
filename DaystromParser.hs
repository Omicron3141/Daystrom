module DaystromParser where

-- Imports related to parser combinators
import ParserBase
import ParserCombinators
import Control.Monad
import Control.Applicative hiding (optional)


-- Other imports
import DaystromSyntax
import qualified Data.Char

---------------------------
-- MAIN PARSING FUNCTION --
---------------------------

parseProgram :: String -> Program
parseProgram input =
  parse program input

