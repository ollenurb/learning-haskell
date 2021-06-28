
main :: IO ()
main = interact (show . diagonalsSum . toIntMatrix . getMatrix . lines)

getMatrix :: [String] -> [String]
getMatrix (x:xs) = take (read x) xs

toIntMatrix :: [String] -> [[Int]]
toIntMatrix = map (map read . words)

diagonalsSum :: [[Int]] -> Int
diagonalsSum m = abs $ s1 - s2
    where
        s1 = sum $ zipWith (!!) m [0..]
        s2 = sum $ zipWith (!!) (reverse m) [0..]


