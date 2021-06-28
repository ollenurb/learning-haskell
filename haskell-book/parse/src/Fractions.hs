{-# LANGUAGE OverloadedStrings #-}

module Fractions where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shoudlAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
    numerator <- decimal
    char '/'
    denominator <- decimal
    return (numerator % denominator)

virtuousFraction :: Parser Rational
virtuousFraction = do
    numerator <- decimal
    char '/'
    denominator <- decimal
    case denominator of
      0 -> fail "Denominator canno be zero"
      _ -> return (numerator % denominator)

-- <* takes the first applicative, executes it and takes its value, then
-- executes the second and return first applicative's return value
onlyNumber :: Parser Integer
onlyNumber = integer <* eof



