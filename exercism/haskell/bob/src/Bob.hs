module Bob (responseFor) where

import Data.Char
import Data.List

responseFor :: String -> String
responseFor str
    | isQuestion && isYelling = "Calm down, I know what I'm doing!"
    | isQuestion = "Sure."
    | isYelling = "Whoa, chill out!"
    | isNull = "Fine. Be that way!"
    | otherwise = "Whatever."
    where
        (letters, symbols) = partition isLetter str
        isQuestion = (not . null) symbols && filter (not . isSpace) symbols `isPrefixOf` "?"
        isYelling = (not . null) letters && all isUpper letters
        isNull = all isSpace str
