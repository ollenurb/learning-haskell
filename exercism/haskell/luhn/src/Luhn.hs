module Luhn (isValid) where

import Data.Char


isValid :: String -> Bool
isValid n = undefined 
    where
        ints = map digitToInt n
        res = sum $ firstStep n

firstStep :: [Int] -> [Int]
firstStep xs = zipWith step [1..] xs

step :: Int -> Int -> Int
step x y | x `mod` 2 /= 0 = if prod > 9 then prod - 9 else prod
    where
        prod = y * 2
step _ y = y


