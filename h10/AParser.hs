{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import Control.Applicative
import Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-- Exercise 1
-- Implemet a Functor instance for Parser
instance Functor Parser where
  -- It maps the function f over the Parser `p`, resulting in a Parser that 
  -- consumes some input and then applies the function f to that input
  fmap f p = Parser (\str -> 
                      let p1 = runParser p 
                      -- since `p1 srt` results in Maybe (a, String), then 
                      -- use `fmap (first f)` to apply f to the value 
                      in fmap (first f) $ p1 str
                    )
  
first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)
  
-- Exercise 2

-- Implement an Applicative instance for Parser
-- `pure a` represents the parser which consumes no input and successfully 
-- returns a result of `a`  
-- p1 <*> p2 represents the parser which first runs p1 (which will
-- consume some input and produce a function), then passes the
-- remaining input to p2 (which consumes more input and produces
-- some value), then returns the result of applying the function to the
-- value. However, if either p1 or p2 fails then the whole thing should
-- also fail (put another way, p1 <*> p2 only succeeds if both p1 and
-- p2 succeed).

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure a = Parser (\_ -> Just (a, ""))
  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b 
  -- running the parser p1 results in a function `f` and String str2
  -- then pass that `str2` to parser p2, resulting in value `a` and String `str3`
  -- The final result should be `f` applied to `a` and String `str3`
  p1 <*> p2 = Parser (\str -> 
    case runParser p1 str of
      Nothing -> Nothing
      Just (f, str2) -> case runParser p2 str2 of
        Just (a, str3) -> Just (f a , str3)
        Nothing -> Nothing
    )

-- Exercise 3

-- We can test your Applicative instance using other simple
-- applications of functions to multiple parsers. Implement each of the 
-- following exercises using the Applicative interface to put together simpler 
-- parsers into more complex ones. Do not implement them using the low-level 
-- definition of a Parser! In other words, pretend that you do not have access 
-- to the Parser constructor or even know how the Parser type is defined.

-- A parser which expects to see the characters 'a' and 'b' and returns them
-- as pair
-- A helper function tuplefy is used - first it is mapped over the parser 
-- `char 'a'`, which results in a Parser (t -> (Char, t)), which will parse some
-- input, and when successful, results in Just (f, str), where f is basically a 
-- function (\x -> ('a', x)). Then the `char b` parser is run with the remaining
-- input `str` and when successful then f is applied to the value
abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

-- A parser which acts the same way as abParser but returns () instead of the
-- characters 'a' and 'b'
-- Reused the abParser, fmapped `const ()` function to it. So if abParser would
-- successfully return a value, instead the value is fed to `const ()` function
-- that returns ()
abParser_ :: Parser ()
abParser_ = const () <$> abParser

-- A parser intPair which reads two integer values separated by space and 
-- returns the integer values in a list
intPair :: Parser [Integer]
intPair = parseIntAndSpace <*> parseIntToList
  where parseIntToList = pure <$> posInt -- Parses an Integer and puts it into list
        -- Parses an Integer followed by space, returns Parser ([Integer] -> [Integer])
        -- Then that parser can be applied with parseIntToList
        parseIntAndSpace = (const. (++)) <$> parseIntToList <*> char ' ' 
        
-- Exercise 4

-- Implement the Alternative instance for Parser
-- empty represents the parser which always fails.
-- p1 <|> p2 represents the parser which first tries running p1. If
-- p1 succeeds then p2 is ignored and the result of p1 is returned.
-- Otherwise, if p1 fails, then p2 is tried instead.
instance Alternative Parser where
  empty = Parser (\_ -> Nothing)
  -- (<|>) :: Parser a -> Parser a -> Parser a
  p1 <|> p2 = Parser (\str -> 
      case runParser p1 str of
        Just (a, str2) -> Just (a, str2)
        Nothing -> runParser p2 str 
    )
-- Note: as Maybe already is an instance of Alternative, then <|> for Parser
-- can be defined as (\str -> runParser p1 str <|> runParser p2 str)
-- (because runParser returns a Maybe value)
    
-- Exercise 5

-- Implement a parser intOrUppercase which parses either integer value or an 
-- uppercase character and fails otherwise
intOrUppercase :: Parser ()
intOrUppercase = parseInt <|> parseUpper
  where parseInt = const () <$> posInt
        parseUpper = const () <$> satisfy isUpper
