module BDC where

birthdayCakeCandles candles = undefined

main :: IO()
main = do
    n <- readLn :: IO Int
    inList <- readAndParse
    putStrLn $ show $ solve inList


readAndParse :: IO [Int]
readAndParse = fmap ((map read) . words) getLine

solve :: [Int] -> Int
solve xs = let m = maximum xs in
               (length . filter (==m)) xs

