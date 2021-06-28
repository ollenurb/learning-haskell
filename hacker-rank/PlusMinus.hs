module PM where

import Data.List
import Control.Monad
import Text.Printf

main :: IO()
main = do
    n <- readLn :: IO Int
    vals <- getLine
    let solution = solve $ parseIntsList vals
    mapM_ (printf "%.6f\n") solution

parseIntsList :: String -> [Int]
parseIntsList = map (read :: String -> Int) . words

solve :: [Int] -> [Float]
solve xs
  = map (\x->fromIntegral (length x)/fromIntegral (length xs)) $ groupByNpz xs

groupByNpz :: [Int] -> [[Int]]
groupByNpz xs = [filter (>0) xs, filter (<0) xs, filter (==0) xs]


