{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module SemVer where

import           Text.Trifecta
import           Control.Applicative

data NumberOrString = NOSS String
                    | NOSI Integer
                    deriving (Eq, Show)

type Major    = Integer
type Minor    = Integer
type Patch    = Integer
type Release  = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata
    deriving (Eq, Show)

instance Ord SemVer where
    (SemVer ma mi pa _ _) <= (SemVer ma' mi' pa' _ _)
      = (ma <= ma') && (mi <= mi') && (pa <= pa')

parseSemVer :: Parser SemVer
parseSemVer = do
    (ma, mi, pa) <- parseVersionCore
    rel <- parseRelease
    meta <- parseMetadata
    return $ SemVer ma mi pa rel meta

parseVersionCore :: Parser (Major, Minor, Patch)
parseVersionCore = (,,)
                <$> parseMajor <* char '.'
                <*> parseMinor <* char '.'
                <*> parsePatch

parseRelease :: Parser Release
parseRelease = (char '-' >> parseNumberOrString `sepBy` (symbol "."))
            <|> (eof >> return [])

parseMetadata :: Parser Metadata
parseMetadata = (char '+' >> parseNumberOrString `sepBy` (symbol "."))
            <|> (eof >> return [])

-- Parsers corresponds to BNF
parseMajor :: Parser Major
parseMajor = integer

parseMinor :: Parser Minor
parseMinor = integer

parsePatch :: Parser Patch
parsePatch = integer

-- NOSS :: String -> NumberOrString
-- some letter :: CharParsing f => f [Char]
-- NOSS <$> some letter :: CharParsing f => f NumberOrString
--
-- Basically it "injects" the type constructor into the parser context
parseNumberOrString :: Parser NumberOrString
parseNumberOrString =  (NOSS <$> some letter)
                   <|> (NOSI <$> integer)

main :: IO ()
main = do
    putStrLn "Insert version to parse: "
    ver <- filter (/= '\n') <$> getLine
    let res = parseString parseSemVer mempty ver
    case res of
      Success parsed -> putStrLn $ show parsed
      Failure err -> putStrLn $ show err


-- Other Exercises
-- 2. >> 3.
parseDigit :: Parser Char
parseDigit = oneOf digits
    where digits = '-' : foldr ((++) . show) "" [0..9]

parseInteger :: Parser Integer
parseInteger = read <$> many parseDigit

-- 4. US/Canada phone numbers

