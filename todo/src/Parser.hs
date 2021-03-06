module Parser where

import Text.Trifecta
import Control.Applicative
import Data.Char
import Text.Parser.Token

data Status = Done | Todo
    deriving (Eq)

data Item = Item Status String
    deriving (Eq)

instance Show Status where
    show Done = "DONE"
    show Todo = "TODO"

instance Show Item where
    show (Item status body) = show status ++ ": " ++ body

switchState :: Item -> Item
switchState (Item Done body) = Item Todo body
switchState (Item Todo body) = Item Done body

parseStatus :: Parser Status
parseStatus = (string "TODO" >> return Todo)
           <|> (string "DONE" >> return Done)

-- Parse end of line and discard the result
eol :: Parser ()
eol = newline

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
    Item status <$> parseBody

parseContent :: Parser [Item]
parseContent = many parseItem
