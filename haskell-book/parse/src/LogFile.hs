{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module LogFile where

import           Text.Trifecta
import           Control.Applicative
import           Text.RawString.QQ
import           Data.Map (Map)
import qualified Data.Map as M


data TimeStamp = TimeStamp { hour :: Integer
                           , minutes :: Integer
                           } deriving (Show)

data Record = Record { time :: TimeStamp
                     , content :: String
                     } deriving (Show)

type Date = String

type Log = Map Date [Record]

parseTimeStamp :: Parser TimeStamp
parseTimeStamp = do
    hour <- parse2Digits
    skipSome $ char ':'
    minutes <- parse2Digits
    return $ TimeStamp hour minutes
        where
            parse2Digits = read <$> count 2 digit

parseRecord :: Parser Record
parseRecord = Record
           <$> parseTimeStamp <* (skipMany $ space)
           <*> manyTill anyChar (try $ char '\n') -- take until newline
           <* skipWhitespaces

parseDate :: Parser Date
parseDate = do
    char '#'
    skipWhitespaces
    date <- some (noneOf "\n")
    skipMany (oneOf "\n")
    return date

skipWhitespaces :: Parser ()
skipWhitespaces = skipMany (char ' ' <|> char '\n')

recordExample :: String
recordExample = [r|
    # 2018-05-04

    14:35 Ciao grande
    18:30 We master, come stai?
    14:35 Ciao grandissimo

    # 2018-05-06

    14:35 Secondo stack di record bro!
    14:35 Non ho piu' voglia di scrivere..
    |]

parseLogSection :: Parser (String, [Record])
parseLogSection = do
    skipWhitespaces
    date <- parseDate
    skipWhitespaces
    records <- many parseRecord
    return $ (date, records)

parseLog :: Parser Log
parseLog = do
    sections <- many parseLogSection
    return $ M.fromList sections


