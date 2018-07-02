module DaystromParser where

import DaystromSyntax
import qualified Data.Char

type Token = String

---------------------------
---- HELPER  FUNCTIONS ----
---------------------------
newLineChars = ['\n']

tokenize :: String -> [Token]
tokenize input = reverse (tokenizeHelper input [""])

tokenizeHelper :: String -> [String] -> [String]
tokenizeHelper ""  l     = l
tokenizeHelper (' ':r) l     = tokenizeHelper r ("":l)
tokenizeHelper (c:r)   (w:l)
    | elem c newLineChars = tokenizeHelper r (["", [c], w] ++ l)
    | otherwise           = tokenizeHelper r ([w ++ [c]] ++ l)

isIdentifier :: Token -> Bool
isIdentifier token = if elem token reservedWords then False
                     else if not (isAlphaString token) then False
                          else True

isAlphaString :: String -> Bool
isAlphaString "" = True
isAlphaString (c:r) = if Data.Char.isAlpha c then isAlphaString r else False

isNewLine :: String -> Bool
isNewLine []     = False
isNewLine (c:[]) = elem c newLineChars
isNewLine _      = False

isWhiteSpace :: String -> Bool
isWhiteSpace []     = False
isWhiteSpace (c:[]) = (elem c newLineChars) || (Data.Char.isSeparator c)
isWhiteSpace _      = False

isPunctuation :: String -> Bool
isPunctuation []     = False
isPunctuation (c:[]) = Data.Char.isPunctuation c
isPunctuation _      = False


nextLineStartsWith :: [Token] -> [Token] -> Bool
nextLineStartsWith [] tokens = True
nextLineStartsWith (desired:desiredRest) (found:tokensRest)
    | desired == found = True
    | otherwise        = nextLineStartsWith desiredRest tokensRest

nextLineStartsWithSingle :: Token -> [Token] -> Bool
nextLineStartsWithSingle desired found = nextLineStartsWith [desired] found

nextLineContains :: [Token] -> [Token] -> Bool
nextLineContains [] _ = True
nextLineContains (desired:desiredRest) (token:tokensRest)
    | desired == token = nextLineContains desiredRest tokensRest
    | isNewLine token  = False
    | otherwise        = nextLineContains (desired:desiredRest) tokensRest

nextLineContainsSingle :: Token -> [Token] -> Bool
nextLineContainsSingle desired tokens = nextLineContains [desired] tokens

consume :: [Token] -> [Token] -> [Token]
consume [] tokens = tokens
consume (desired:desiredRest) (found:foundRest)
    | desired == found = consume desiredRest foundRest
    | otherwise        = error ("Expected " ++ (show (desired:desiredRest)) ++" but found " ++ (show (found:foundRest)))

consumeNewLine :: [Token] -> [Token]
consumeNewLine (token:rest)
    | isNewLine token = rest
    | otherwise       = error ("Expected new line but found " ++ (show rest))

consumeWhiteSpace :: [Token] -> [Token]
consumeWhiteSpace [] = []
consumeWhiteSpace (w:r)
    | w == ""        = consumeWhiteSpace r
    | isWhiteSpace w = consumeWhiteSpace r
    | otherwise      = (w:r)

possiblyConsumePunctuation :: [Token] -> [Token]
possiblyConsumePunctuation [] = []
possiblyConsumePunctuation (w:r)
    | w == ""         = possiblyConsumePunctuation r
    | isPunctuation w = possiblyConsumePunctuation r
    | otherwise       = (w:r)

endLineStatement :: [Token] -> [Token]
endLineStatement tokens = consumeWhiteSpace ( consumeNewLine (possiblyConsumePunctuation tokens))

---------------------------
--- CONSTANTS AND NAMES ---
---------------------------
stations = ["helm", "computer", "operations", "communications"]
aliasText = ["is", "on"]
functionText = ["prepare", "maneuver"]
mainFunctionText = ["captain's", "log"]
-- UPDATE AS NEEDED
reservedWords = stations ++ aliasText

---------------------------
-- MAIN PARSING FUNCTION --
---------------------------
parse :: String -> Program
parse input = let tokens = tokenize input
                  (program, leftovers) = parseProgram tokens
              in if leftovers == [] then program
		 else error ("Parsed :\n\n"++(show program)++"\n\n but had leftover tokens:\n\n" ++ (show leftovers))

parseProgram :: [Token] -> (Program, [Token])
parseProgram input = let (aliases, tokens1) = parseAliasBlock input
                     in  ((DaystromProgram aliases []), tokens1)

parseAliasBlock :: [Token] -> ([Alias], [Token])
parseAliasBlock input = if nextLineContains aliasText input then
                           let (alias, tokens1) = parseAlias input
                               (aliases, tokens2) = parseAliasBlock tokens1
                           in  (([alias] ++ aliases), tokens2)		
                        else ([], input)

parseStation :: [Token] -> (String, [Token])
parseStation (token:rest)
    | elem token stations = (token, rest)
    | otherwise           = error ("Invalid station found: " ++ token)

parseAlias :: [Token] -> (Alias, [Token])
parseAlias (iden:tokens1)
    | isIdentifier iden = let tokens2            = consume aliasText tokens1
                              (station, tokens3) = parseStation tokens2
                              tokens4            = endLineStatement tokens3
                          in ((StationAlias iden station), tokens4)
    | otherwise         = error ("Invalid identifier found in alias: " ++ iden)


--parseFunctionBlock :: [Token] -> ([Function], [Token])
--parseFunctionBlock :: 
