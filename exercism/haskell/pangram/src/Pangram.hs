module Pangram (isPangram) where

import Data.Set (Set, fromList, (\\), empty)
import Data.Char

alphabet :: Set Char
alphabet = fromList ['a'..'z']

isPangram :: String -> Bool
isPangram str = (alphabet \\ fromList filteredWord) == empty
    where
        predicate x = isAscii x && isLetter x
        filteredWord = (map toLower . filter predicate) str

