module ParserBase (Parser,pfail,get,parse,parseFile,parseNamed,
                   (<||>),succeeding,eof) where

        -- Also, is instances of Functor, Applicative, Monad and MonadPlus

import Control.Applicative hiding (optional)
import Control.Monad

-- Our parser carries both the status of the most recent parse attempted
-- and the "deepest error" found so far.  That way, when the whole parse
-- fails, we can give better error messages.

type ParsePosn = (Int, Int) -- Line, Column
type ParseInput = (String, ParsePosn)
type ParseError = (String, ParsePosn)
newtype Parser a = ParsingFunction (ParseError -> ParseInput -> ParseResult a)
data ParseStatus a = Success a ParseInput
                   | Failure ParseError
                        deriving Show
type ParseResult a = (ParseError, ParseStatus a)

failure prevErr@(prevMsg,prevPosn) msg posn = (bestErr, Failure newErr)
    where newErr                    = (msg,posn)
          bestErr | prevPosn > posn = prevErr
                  | otherwise       = newErr

pfail :: Parser a
pfail = fail "No parse (via pfail)"
-- pfail = ParsingFunction (\err -> \(_,posn) -> (err, Failure ("No parse.",posn)))

get :: Parser Char
get = ParsingFunction readChar
    where readChar e ('\n':t, (line,_))   = (e, Success '\n' (t,(line+1,0)))
          readChar e (h:t,    (line,col)) = (e, Success h    (t,(line,col+1)))
          readChar e (_,      posn)       = failure e "Unexpected EOF" posn

eof :: Parser ()
eof = ParsingFunction tryRead
    where tryRead e state@("", _)    = (e, Success () state)
          tryRead e       (_,  posn) = failure e "EOF expected" posn

parse :: Parser a -> String -> a
parse p = parseNamed p "<input>" 

parseFile :: Parser a -> String -> IO a
parseFile parser fileName = readFile fileName 
                            >>= return . parseNamed parser fileName

parseNamed :: Parser a -> String -> String -> a
parseNamed (ParsingFunction f) fileName inputString = 
    case f ("No (known) error", (0,0)) (inputString,(1,1)) of
        (_,   Success result ("", _)) -> 
            result
        (bErr@(bMsg,bPosn), Success result (_,  posn))  ->
            makeError $ if bPosn >= posn then bErr
                                         else ("EOF expected", posn)
        (err, _) ->
            makeError err
    where
        makeError (msg, (line,col)) =
            error (fileName ++ ":" ++ show line ++ ":" ++ show col ++ " -- " 
                   ++ msg)

-- Combine two parsers using an 'or' type operation -- this is the
-- code used for mplus and <|>       
orElseWithMergedErr :: Parser a -> Parser a -> Parser a 
orElseWithMergedErr (ParsingFunction f) (ParsingFunction g) =
   ParsingFunction f_or_g
   where f_or_g err1 state =
             case f err1 state of 
                 (err2, Failure ffail@(why_f,pos_f)) -> 
                     case g err2 state of
                         (err3, Failure gfail@(why_g,pos_g)) ->
                             if pos_f > pos_g then (err3, Failure ffail)
                                              else (err3, Failure gfail)
                         result -> result
                 success -> success

-- Combine two parsers using an 'or' type operation -- this is the
-- code used for <||>, this version throws away any error information produced
-- by the first parser, the only error information is the information produced
-- by the second one.
orElse :: Parser a -> Parser a -> Parser a 
orElse (ParsingFunction f) (ParsingFunction g) =
   ParsingFunction f_or_g
   where f_or_g err1 state =
             case f err1 state of 
                 (_, Failure _) -> g err1 state
                 success -> success

infixl 3 <||>
(<||>) :: Parser a -> Parser a -> Parser a
(<||>) = orElse


-- succeeding x p tries to parse p, and if it succeeds, all is good, otherwise
-- it will return x.  It's almost identical to
--    succeeding x p = p <|> return x
-- except that the *guaranteed* success is useful if you're trying to parse
-- lazily.

succeeding :: a -> Parser a -> Parser a
succeeding fallback (ParsingFunction f) = ParsingFunction f'
   where f' err1 state = (err2, Success result state'')
             where ~(err2, result,state'') =   -- <-- vital lazy match!!!
                       case f err1 state of 
                           (err2, Success x state') -> (err2, x, state')
                           (err2, Failure _)        -> (err2, fallback, state)

-- The core functions

instance Monad Parser where
    return x  = ParsingFunction (\err -> \state -> (err, Success x state))
    fail msg  = ParsingFunction (\err -> \(_,posn) -> failure err msg posn)
    ParsingFunction f >>= makeG = ParsingFunction f_then_g 
        where f_then_g err1 str = 
                  case f err1 str of
                      (err2, Success x state') -> 
                          let ParsingFunction g = makeG x
                          in  g err2 state'
                      (err2, Failure whypos) -> (err2, Failure whypos)

-- Derive other monades using existing/derived functions

instance Functor Parser where
    fmap = liftM

instance MonadPlus Parser where
    mzero = pfail
    mplus = orElseWithMergedErr

instance Applicative Parser where
    pure = return
    (<*>) = ap

instance Alternative Parser where
    empty = mzero
    (<|>) = mplus
