module Main where

type Row = [Double]
type Matrix = [Row]

triangularize :: Matrix -> Matrix
triangularize []     = []
triangularize (x:xs) = x:(triangularize reduced)
    where reduced = map (reduceRow x) xs

reduceRow :: Row -> Row -> Row
reduceRow (p:pr) (i:ir) = zipWith (-) ir $ map (*m) pr
    where m = i/p -- compute multiplier

fillLeft :: Int -> a -> [a] -> [a]
fillLeft n x xs
  | length xs >= n = xs
  | otherwise      = x:(fillLeft (n-1) x xs)

gauss :: Matrix -> Matrix
gauss m = (map (fillLeft order 0.0) . triangularize) m
    where order = length m

main :: IO ()
main = undefined
