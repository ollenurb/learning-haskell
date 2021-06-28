module TimeConversion where

import Data.Char
import Data.List

main :: IO ()
main = interact clockToMilitary

data Format = PM | AM
    deriving (Read, Show)

clockToMilitary :: String -> String
clockToMilitary str =
    case format of
        AM -> if hour == 12 then ("00"++rest) else noFormat
        PM -> if hour == 12 then noFormat else (show $ hour + 12) ++ rest
    where
        format = read (lastN 2 str) :: Format
        noFormat = let m = length str in take (m-2) str
        (hour, rest) = ((read $ take 2 noFormat :: Int), (drop 2 noFormat))


lastN :: Int -> [a] -> [a]
lastN n xs = let m = length xs in drop (m-n) xs
