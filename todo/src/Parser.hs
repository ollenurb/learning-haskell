module Parser where

import Text.Trifecta
import Control.Applicative
import Data.Char
import Text.Parser.Token

data Status = Done | Todo
    deriving (Eq)

instance Show Status where
    show Done = "[X]"
    show _    = "[ ]"

data Item = Item Status String
    deriving (Eq)

instance Show Item where
    show (Item status body) = show status ++ " " ++ show body

switchState :: Item -> Item
switchState (Item Done body) = Item Todo body
switchState (Item Todo body) = Item Done body

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
    return $ Item status body

parseContent :: Parser [Item]
parseContent = many parseItem
