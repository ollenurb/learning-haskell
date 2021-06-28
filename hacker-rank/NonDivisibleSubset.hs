import Data.List

binom :: [Int] -> [(Int, Int)]
binom []     = []
binom (x:xs) = [(x, y) | y <- xs] ++ binom xs

nonDivisibleSubset :: [Int] -> Int -> Int
nonDivisibleSubset xs k = (length . filter (sumIsMultipleOf k) . binom) xs
    where
        sumIsMultipleOf k (x,y) = (x + y) `mod` k /= 0

nds :: [Int] -> Int -> [(Int, Int)]
nds xs k = (filter (sumIsMultipleOf k) . binom) xs
    where
        sumIsMultipleOf k (x,y) = (x + y) `mod` k == 0


input :: [Int]
input = [278, 576, 496, 727, 410, 124, 338, 149, 209, 702, 282, 718, 771, 575, 436]


