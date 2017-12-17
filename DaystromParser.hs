module DaystromParser where

-- Imports related to parser combinators
import ParserBase
import ParserCombinators
import Control.Monad
import Control.Applicative hiding (optional)


-- Other imports
import DaystromSyntax
import qualified Data.Char

deadspace :: Parser ()
deadspace = skipMany ((string " ") <|> (string "\n") <|> (string "\r\n"))

punct :: Parser ()
punct = skipMany ((sym ',') <|> (sym '.'))

endline :: Parser ()
endline = punct <-+-> deadspace

program :: Parser Program
program = ((many alias) <+-> deadspace) <+> (many function) >>=: \(al, fn) -> DaystromProgram al fn

alias :: Parser Alias
alias = ((aliasName <+-> ((text "is on") <|> (text "is now")) <+> stationName <+-> endline) >>=: \(al, st) -> StationAlias al st) <??> "malformed alias declaration"

aliasName :: Parser Name
aliasName = ident

stationName :: Parser Name
stationName = ident 

function :: Parser Function
function = (((between mainDecl mainEnd ((some command) <???> "improper commands in main function")) >>=: \c -> MainFunction c)
         <|>
           ((between functionDecl functionEnd ((ident <+-> endline <+> (many command)) <???> "improper commands in function")) >>=: \(i, c) -> HelperFunction i c))


functionDecl :: Parser ()
functionDecl = (aliasName <-+-> (many (sym ',')) <-+-> ((text "prepare") <|> (text "ready")) <-+-> (text "maneuver")) <??> "malformed function declaration"

functionEnd :: Parser ()
functionEnd = (endline <-+-> (text "end maneuver") <-+-> endline) <??> "missing or malformed function ending"

mainDecl :: Parser ()
mainDecl = ((text "captain") <-+-> (many (text "'")) <-+-> (sym 's') <-+-> (text "log") <-+-> garbage <-+-> deadspace) <??> "malformed main function start"

mainEnd :: Parser ()
mainEnd = (endline <-+-> (text "end log") <-+-> endline) <??> "missing or malformed main function ending"

garbage :: Parser ()
garbage = skipMany (alphanum <|> (sym ',') <|> (sym '.') <|> (sym ' '))

command :: Parser Command
command = (text "hi") <+-> endline >>=: \i -> Com i Swap

---------------------------
-- MAIN PARSING FUNCTION --
---------------------------

parseProgram :: String -> Program
parseProgram input =
  parse program input

