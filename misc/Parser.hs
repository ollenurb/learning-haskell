{- Implements a simple monadic Parser -}
{-# LANGUAGE TupleSections #-}

module Parser where

import Data.Char

{-
A Parser is just a function that takes a String
(the input to consume), and returns the corresponding
type that matches and the remaining input
-}
newtype Parser a = Parser { runParser :: String -> (a, String) }

{- Make a Parser a Functor -}
instance Functor Parser where
    fmap f p = Parser $ \s -> let (r,rr) = runParser p s in (f r, rr)

{- Make a Parser an Applicative -}
instance Applicative Parser where
    pure a = Parser (a,)
    a <*> b = Parser $ \s ->
        let (f, ss) = runParser a s
            (r, rr) = runParser b ss
        in (f r, rr)

{- Make a Parser a Monad -}
instance Monad Parser where
    return  = pure
    p >>= f = Parser $ \s ->
        let (r, ss) = runParser p s
        in runParser (f r) ss

{- Parse a single Char, throw an error if it doesn't match -}
char :: Char -> Parser Char
char s = Parser $ \(c:ss) -> if s == c then (c, ss) else error $ "Cannot match " ++ [s]

{- Parse a single Digit, throw an error if it doesn't match (it's not a valid digit) -}
digit :: Parser Int
digit = Parser $ \(c:ss) -> (emitDigit c, ss)
    where emitDigit c
            | isNumber c = digitToInt c
            | otherwise  = error "Cannot match digit"

{-
Use monadic composition to parse a string with the following format:
(v:digit)
-}
charThenDigit :: Parser (Char, Int)
charThenDigit = do
    char '('
    c <- char 'd'
    char ':'
    d <- digit
    char ')'
    return (c, d)

