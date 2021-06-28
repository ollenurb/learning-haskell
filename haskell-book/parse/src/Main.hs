module Main where

import Text.Trifecta
import Control.Applicative

one = char '1'

oneTwo = char '1' >> char '2'

oneTwoEOF :: Parser Char
oneTwoEOF = char '1' >> char '2' <* eof -- <* run computation then discard the result

diffStrings :: Parser String
diffStrings
  =  try (exactString "123")
 <|> try (exactString "12")
 <|> exactString "1"

exactString :: String -> Parser String
exactString s = string s <* eof

-- Rewrite string parser using char parsers
strWithChar :: [Char] -> Parser [Char]
strWithChar = mapM char

oneTwo' = oneTwo >> stop

stop :: Parser a
stop = unexpected "stop"

testParse :: Parser Char -> IO ()
testParse p =
    print $ parseString p mempty "123"

pNL s = putStrLn ('\n' : s)

-- allParser = try (string "1") >> try (string "12")

main :: IO ()
main = do
    pNL "stop:"
    testParse stop
    pNL "one:"
    testParse one
    pNL "oneTwo:"
    testParse oneTwo



