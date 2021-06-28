
solve :: [String] -> Int
solve path = sum $ zipWith checkTree path $ map (`mod` 31) [0,3..]

checkTree :: String -> Int -> Int
checkTree line pos
    | line !! pos == '#' = 1
    | otherwise = 0

main :: IO ()
main = interact (show . solve . lines)
