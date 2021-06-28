module Anagram (anagramsFor) where

import Data.List
import Data.Char

toLowerWord :: String -> String
toLowerWord = map toLower

anagramsFor :: String -> [String] -> [String]
anagramsFor x xs = filter (predicate x) xs

predicate :: String -> String -> Bool
predicate x y = (lcX /= lcY) && ((sort lcX) == (sort lcY))
    where
        lcX = toLowerWord x
        lcY = toLowerWord y

