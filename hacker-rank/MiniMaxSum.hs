module MMS where

import Data.List

main = do
    inList <- getLine
    let (min, max) = solve $ parseIntsList inList
    putStrLn $ show min ++ " " ++ show max

solve :: [Int] -> (Int, Int)
solve xs = ((sum . init) sorted, (sum . tail) sorted)
    where sorted = sort xs

parseIntsList :: String -> [Int]
parseIntsList = map (read :: String -> Int) . words
