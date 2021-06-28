module CardsPermutation where

import Data.List

readIntList :: IO [Int]
readIntList = fmap ((map read) . words) getLine

main :: IO ()
main = do n <- readLn :: IO Int
          fp <- readIntList
          putStrLn $ show $ solve n fp

checkPerm :: [Int] -> [Int] -> Bool
checkPerm [] [] = True
checkPerm (x:xs) (y:ys)
    | x == 0    = checkPerm xs ys
    | otherwise = (x == y) && checkPerm xs ys

solve :: Int -> [Int] -> Int
solve n fav = iSum `mod` 1000000007
    where
        iSum = sum $ map snd filtered
        filtered = filter (\(p, _) -> checkPerm fav p) indexed
        indexed = zip perm [1..]
        perm = permute [1..n]

permute :: Eq a => [a] -> [[a]]
permute [] = [[]]
permute as = do a <- as
                let l = delete a as
                ls <- permute l
                return $ a : ls
