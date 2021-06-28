import Data.List

magicSquare :: [[Int]]
magicSquare = [[8, 3, 4],
               [1, 5, 9],
               [6, 7, 2]]

rotate :: [[Int]] -> [[Int]]
rotate m = regroup (xs ++ [x]) 3
    where
        (x:xs) = concat m

regroup :: [a] -> Int -> [[a]]
regroup [] _ = []
regroup xs b = (take b xs) : (regroup (drop b xs) b)
