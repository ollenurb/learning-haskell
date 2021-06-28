-- Leaderboards.hs
import Data.List
import Data.Maybe

ranked :: [Int]
ranked = [100, 90, 90, 80]

player :: [Int]
player = [70, 80, 105]

assignScores :: [Int] -> [(Int, Int)]
assignScores xs = concat unZipped
    where
        unZipped = map unzipTuple $ zip grouped [1..]
        unzipTuple (xs, y) = [(x, y) | x <- xs]
        grouped = groupBy (==) xs

computeRank :: [Int] -> Int -> Int
computeRank lb s = fromJust $ lookup s scored
    where
        newLb = reverse $ sort (s:lb)
        scored = assignScores newLb

computeRanks :: [Int] -> [Int] -> [Int]
computeRanks r p = map (computeRank r) p 
