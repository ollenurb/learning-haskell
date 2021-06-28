module Parser where

import Text.Trifecta
import Control.Applicative
import Data.Char
import Text.Parser.Token

data Status = Done | Todo
    deriving (Show)

type Item = (Status, String)

parseStatus :: Parser Status
parseStatus = (string "TODO" >> return Todo)
           <|> (string "DONE" >> return Done)

-- Parse end of line and discard the result
eol :: Parser ()
eol = newline >> return ()

-- Take everything until either eol or eof
parseBody :: Parser String
parseBody = manyTill anyChar (eol <|> eof)

-- Item format:
-- STATUS: Content
parseItem :: Parser Item
parseItem = do
    spaces
    status <- parseStatus
    _ <- char ':'
    spaces
    body <- parseBody
    return (status, body)

parseContent :: Parser [Item]
parseContent = many parseItem
