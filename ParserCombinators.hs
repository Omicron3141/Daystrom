module ParserCombinators where

import Data.Char as Char
import ParserBase

import Control.Monad
import Control.Applicative hiding (optional)

-- With the above import, we have
--
-- pfail  :: Parser a
-- return :: a -> Parser a
-- get    :: Parser Char
-- <|>    :: Parser a -> Parser a -> Parser a 
-- <||>   :: Parser a -> Parser a -> Parser a 
-- >>=    :: Parser a ->  (a -> Parser b) -> Parser b

-- Define some handy variations of >>= and >>

infixl 1 >>=:
(>>=:) :: Functor parser => parser a -> (a -> b) -> parser b
(>>=:) = flip fmap

infixl 1 >>:
(>>:) :: Functor parser => parser a -> b -> parser b
p >>: v  = fmap (const v) p

-- p <+> q
-- Given two parsers, p and q, makes a new parser for "p then q" that
--    - runs the first parser on the input
--    - runs the second parser on what remains in the input
--    - returns a pair of their results
--
-- (If you think it the code is confusing, it could be worse I could have
--  written it in point-free style as:
--      (<+>) = (((,) <$>) .) . (<*>)
--  -- ugh!)
infixl 6 <+>
(<+>) :: Applicative parser => parser a -> parser b -> parser (a,b)
p <+> q = (,) <$> p <*> q

-- p <:> q
-- Given two parsers, p and q, makes a new parser for "p then q" that
--    - runs the first parser on the input
--    - runs the second parser (which gives a list) on the remaining input
--    - returns a the first result 'cons'ed onto the front of the list
--      returned by the second result
--
infixr 5 <:>
(<:>) :: Applicative parser => parser a -> parser [a] -> parser [a]
p <:> q = (:) <$> p <*> q

-- p <++> q
-- Given two parsers, p and q, makes a new parser for "p then q" that
--    - runs the first parser (which gives a list) on the input
--    - runs the second parser (which gives a list) on the remaining input
--    - returns a the first result appended onto the front of the list
--      returned by the second result
--
infixr 5 <++>
(<++>) :: Applicative parser => parser [a] -> parser [a] -> parser [a]
p <++> q = (++) <$> p <*> q


-- p <-+> q
-- Given two parsers, p and q, makes a new parser for "p then q" that
--    - runs the first parser on the input
--    - runs the second parser on what remains in the input
--    - returns only the results of the second one.
infixl 6 <-+>
(<-+>) :: Applicative parser => parser a -> parser b -> parser b
(<-+>) = (*>)

-- p <+-> q
-- Given two parsers, p and q, makes a new parser for "p then q" that
--    - runs the first parser on the input
--    - runs the second parser on what remains in the input
--    - returns only the results of the first one.
infixl 6 <+->
(<+->) :: Applicative parser => parser a -> parser b -> parser a
(<+->) = (<*)


-- p <-+-> q
-- Given two parsers, p and q, makes a new parser for "p then q" that
--    - runs the first parser on the input
--    - runs the second parser on what remains in the input
--    - ignores both results and returns a Haskell unit, ---i.e.,--this--> ()
infixl 6 <-+->
(<-+->) :: Applicative parser => parser a -> parser b -> parser ()
p <-+-> q = p *> q *> pure ()


-- parser <=> pred
-- Given a parser and a predicate, return the result of the parser only if
-- it also satisfies the predicate.
infix 7 <=> 
(<=>) :: MonadPlus parser => parser a -> (a -> Bool) -> parser a
(<=>) = flip mfilter

-- parser <??> errMsg
-- Given a parser p, makes a new parser where if p fails, the new parser
-- also fails, but unconditionally replaces p's error message with
-- errMsg.  All error information from p (including how far it got) is thrown
-- away.  (This operator uses <||> which is idential to <|> except that it
-- handles error messages slightly differently).
infixl 3 <??>
(<??>) :: Parser a -> String -> Parser a
parser <??> message = parser <||> fail message

-- parser <???> errMsg
-- Given a parser p, makes a new parser where if p fails, the new parser
-- also fails, but it can replace a parser's failure error message with
-- a new one.  But unlike <??>, we only do the replacement if the parser got
-- *nowhere* with things.  If it made  some headway at all, we let its error
-- message stand, in the hope it'll be more useful.  [To avoid thinking the
-- provided parser made headway when it didn't just because it happened to
-- skip whitespace, we skip whitespace before giving the failure message so
-- that it competes on even terms with the failing parse.]
infixl 3 <???>
(<???>) :: Parser a -> String -> Parser a
parser <???> message = parser <|> (skipws (fail message))

                                                 
-- Now that we have <=> working, we can write some more interesting 
-- single-character parsers

getCharThat :: (Char -> Bool) -> Parser Char
getCharThat predicate = get <=> predicate
                      <??> "Different kind of character expected"

digit :: Parser Char
digit  = getCharThat isDigit

letter :: Parser Char
letter = getCharThat isAlpha

space :: Parser Char
space = getCharThat isSpace

alphanum :: Parser Char
alphanum = digit <|> letter

-- These two allow us to look for a particular character or a particular
-- string.

-- char c returns c if c is the next character in the input
char :: Char -> Parser Char
char c = get <=> (==c)
         <??> "Expected '" ++ [c] ++ "'"


-- string "somechars" returns "somechars" if those are exactly the next
-- characters in the input
string :: String -> Parser String
string ""         = return ""
string str@(h:hs) = char h <:> string hs
                    <??> "Expected '" ++ str ++ "'"

-- Variations on the theme of some/many

-- Some other Parsing toolkits use many1 as their word for some, so we define
-- this for compatibility

many1 ::  Parser a -> Parser [a]
many1 = some

-- skipMany p is like
--    many p >> return ()
-- except that its sliightly more efficient because it doesn't build
-- a list of results (as many would)

skipMany :: Alternative f => f a -> f ()
skipMany p  =     (p <-+-> skipMany p)
              <|> pure ()

skipMany1 :: Alternative parser => parser a -> parser ()
skipMany1 p = p <-+-> skipMany p


-- optional p tries to parse a p, but also succeeds if it doesn't find one.
-- You can think of the type of optional as being either
--   optional :: Parser a -> Parser (Maybe a)
--   optional :: Parser a -> Parser [a]
-- but actually we use a more general type.  If Monads and point-free style
-- scares you, this code goes "Boo!".
optional :: (Alternative parser, Alternative t) => parser a -> parser (t a)
optional p = (pure <$> p) <|> pure empty

-- perhaps p is like optional p, but has different type, it assumes we're trying
-- to parse something type that has a built-in notion of emptiness (e.g.,
-- strings with "", lists with [], etc.), specifically something with mzero
-- value.  If we can't parse the thing, we return that 'empty' value.
--   perhaps :: Parser String -> Parser String
--   perhaps :: Parser [a] -> Parser [a]
--   perhaps :: Parser (Maybe a) -> Parser (Maybe a)
-- but actually we use a more general type.  If Monads and point-free style
-- scares you, this code goes "Boo!".

perhaps :: (Alternative p, Monad m, Alternative m) => p (m a) -> p (m a)
perhaps = (join <$>) . optional


-- manyEndingWith end p is equivalent to
--     many p <+-> end
-- *except* that it the above might give error messages related to
-- not being able to parse end (because many always succeeds), whereas this
-- version can give the best error message out of the one for not parsing p
-- and not parsing end.
--
-- In practice, our strategy of choosing the deepest error message should
-- mean that we don't need this function.

-- manyEndingWith :: Parser b -> Parser a -> Parser [a]
manyEndingWith :: Alternative parser => parser b -> parser a -> parser [a]
manyEndingWith end p =     (end                        >>:  [])
                       <|> (p <+> manyEndingWith end p >>=: \(r,rs) -> r:rs)
                    

-- someEndingWith end p is equivalent to
--     some p <+-> end
-- *except* that it the above might give error messages related to
-- not being able to parse end (because some always succeeds if it can read
-- at least one p), whereas this version can give the best error message out
-- of the one for not parsing p and not parsing end.
--
-- In practice, our strategy of choosing the deepest error message should
-- mean that we don't need this function.

someEndingWith :: Alternative parser => parser b -> parser a -> parser [a]
someEndingWith end p = (:) <$> p <*> manyEndingWith end p


-- Identifiers

identifier :: Parser String
identifier =      (letter <+> many alphanum          >>=: \(l,ls) -> l:ls)
             <??> "<identifier> expected"

reservedword str = (identifier <=> ( == str))
           <??> "reserved word (e.g., \"" ++ str ++ "\") expected"


-- Numbers

-- number parses an Integer.  
-- Lab note: The Haskell function read can do the conversion from a string 
-- to an Integer
number :: Parser Integer
number =     (optional (char '-') <+> some digit    >>=: \(m,d) -> read (m++d))
        <??> "<number> expected"

-- Lots of things are delimited by tokens, this lets us read them.  Note the
-- order of its arguments -- it takes the parsers delimiters *first*, and
-- *then* the thing to parse inside them

between :: Applicative parser =>
               parser open -> parser close -> parser a -> parser a
between open close p = open <-+> p <+-> close     

-- Our parser does not ignore whitespace, if we want to skip it, we have
-- to do so explicitly.

whitespace :: Parser ()
whitespace = skipMany space

-- skipws p returns a parser that parses what p does, but skips whitespace
-- before and after

skipws :: Parser a -> Parser a
skipws p = whitespace <-+> p -- <+-> whitespace

-- num, ident, sym and text are like number, identifier, char, and string,
-- respectively but allow there to be space either side of the thing.

num :: Parser Integer
num = skipws number

ident :: Parser String
ident = skipws identifier

sym :: Char -> Parser Char
sym ch  = skipws (char ch)

text :: String -> Parser String
text str = skipws (string str) 

rword :: String -> Parser String
rword str = skipws (reservedword str)

openparen :: Parser Char
openparen  = sym '('

closeparen :: Parser Char
closeparen = sym ')'

parens :: Parser a -> Parser a
parens p = between openparen closeparen p

sepBy1 :: Alternative parser => parser a -> parser sep -> parser [a]
sepBy1 p sep = p <:> many (sep <-+> p)

sepBy :: Alternative parser => parser a -> parser sep -> parser [a]
sepBy p sep =    sepBy1 p sep
             <|> pure []

endBy :: Alternative parser => parser a -> parser sep -> parser [a]
endBy p sep = many (p <+-> sep)

endBy1 :: Alternative parser => parser a -> parser sep -> parser [a]
endBy1 p sep = some (p <+-> sep)

chainr1 :: Alternative parser => parser a -> parser (a -> a -> a) -> parser a
chainr1 p op = flip (foldr (\(r,f) a -> f r a)) <$> many (p <+> op) <*> p 

chainl1 :: Alternative parser => parser a -> parser (a -> a -> a) -> parser a
chainl1 p op = foldl (\a (f,r) -> f a r) <$> p <*> many (op <+> p)
