{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module IniParse where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Char (isAlpha)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Test.Hspec
import Text.RawString.QQ
import Text.Trifecta


-- ======== Parser for header ======== --
headerEx :: ByteString
headerEx = "[blah]"

newtype Header = Header String
    deriving (Eq, Ord, Show)

parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader = parseBracketPair (Header <$> some letter)

-- ======== Parser for assignment ======== --
assignmentEx :: ByteString
assignmentEx = "woot=1"

type Name = String
type Value = String
type Assignments = Map Name Value

parseAssignment :: Parser (Name, Value)
parseAssignment = do
    name <- some letter
    char '='
    val <- some (noneOf "\n")
    skipEOL
    return (name, val)

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

-- ======== Parser for ignoring comments ======== --
commentEx :: ByteString
commentEx = "; last modified 1 April 2001 by John Doe"

commentEx' :: ByteString
commentEx' = "; blah\n woot\n \n;hah"

-- Since the parser has to ignore, it doesn't have any value to "return" in the
-- monadic context
skipComments :: Parser ()
skipComments = skipMany commentParser
    where
        commentParser = do _ <- char ';' <|> char '#' -- a comment can either begin with ; or #
                           skipMany (noneOf "\n")     -- go forward until '\n'
                           skipEOL                    -- skip EOL (last '\n')

-- ======== Parser for section ======== --
sectionEx :: ByteString
sectionEx = "; ignore me\n[states]\nChrist=Texas"

sectionEx' :: ByteString
sectionEx' = [r|
    ; ignore me
    [state]
    Chris=Texas
    |]

sectionEx'' :: ByteString
sectionEx'' = [r|
    ; comment
    [section]
    host=wikipedia.org
    alias=claw

    [whatisit]
    red=intoothandclaw
    |]

data Section = Section Header Assignments
    deriving (Eq, Show)


skipWhitespaces :: Parser ()
skipWhitespaces = skipMany (char ' ' <|> char '\n')

parseSection :: Parser Section
parseSection = do
    skipWhitespaces
    skipComments
    h <- parseHeader
    skipEOL
    assignments <- some parseAssignment
    return $
        Section h (M.fromList assignments)


-- ======== Parser for entire config file ======== --
newtype Config = Config (Map Header Assignments)
    deriving (Eq, Show)

-- Sections are rolled up into a Map that keys section data by section name,
-- with the value being further more Maps of assignment names mapped to their
-- values
rollup :: Section -> Map Header Assignments -> Map Header Assignments
rollup (Section h a) m = M.insert h a m

parseIni :: Parser Config
parseIni = do
    sections <- some parseSection
    let mapOfSections =
            foldr rollup M.empty sections
    return (Config mapOfSections)

-- ======== Main just runs some tests ======== --
maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success v) = Just v
maybeSuccess _           = Nothing

main :: IO ()
main = hspec $ do
    describe "Assignment Parsing" $
        it "can parse a simple assignment" $ do
            let m = parseByteString
                    parseAssignment
                    mempty assignmentEx
                r' = maybeSuccess m
            print m
            r' `shouldBe` Just ("woot", "1")

    describe "Header Parsing" $
        it "can parse a simple header" $ do
            let m = parseByteString
                    parseHeader
                    mempty headerEx
                r' = maybeSuccess m
            print m
            r' `shouldBe` Just (Header "blah")

    describe "Comment Parsing" $
        it "skip comment before header" $ do
            let p = skipComments >> parseHeader
                i = "; woot\n[blah]"
                m = parseByteString p mempty i
                r' = maybeSuccess m
            print m
            r' `shouldBe` Just (Header "blah")

    describe "Section Parsing" $
        it "can parse a simple section" $ do
            let m = parseByteString parseSection
                    mempty sectionEx
                r' = maybeSuccess m
                states = M.fromList [("Chris", "Texas")]
                expected' =
                    Just (Section (Header "states") states)
            print m
            r' `shouldBe` expected'

    describe "Ini Parsing" $
        it "can parse multiple sections" $ do
            let m = parseByteString parseIni
                    mempty sectionEx''
                r' = maybeSuccess m
                sectionValues =
                    M.fromList
                    [("alias", "claw"), ("host", "wikipedia.org")]
                whatisitValues =
                    M.fromList
                    [("red", "intoothandclaw")]
                expected' =
                    Just (Config
                            (M.fromList
                            [(Header "section"
                            , sectionValues)
                            , (Header "whatisit"
                            , whatisitValues)]))
            print m
            r' `shouldBe` expected'
