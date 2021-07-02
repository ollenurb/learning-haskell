solve2 :: [String] -> Int
solve2 path = product $ map (uncurry (solveWithSlope path)) positions

solveWithSlope :: [String] -> Int -> Int -> Int
solveWithSlope path right down
    = sum $ zipWith checkTree downPath $ map (`mod` lineLenght) rightPath
    where
        lineLenght = length $ head path
        downPath = takeEvery down path
        rightPath = takeEvery right [0..]

-- Input positions given by the problem
positions :: [(Int, Int)]
positions = [(1,1), (3,1), (5,1), (7,1), (1,2)]

checkTree :: String -> Int -> Int
checkTree line pos
    | line !! pos == '#' = 1
    | otherwise          = 0

takeEvery :: Int -> [a] -> [a]
takeEvery n xs = [x | (x, i) <- zip xs [0..], i `mod` n == 0]

showSolutions :: [String] -> String
showSolutions path
  = "Solution 1: " ++ show sol1 ++ "\n" ++
    "Solution 2: " ++ show sol2 ++ "\n"
    where
        sol1 = solveWithSlope path 3 1
        sol2 = solve2 path

main :: IO ()
main = interact (showSolutions . lines)
