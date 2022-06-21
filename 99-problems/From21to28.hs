-- From21to28.hs

module From21to28 where

-- !! If you don't have random installed run: stack install random
import System.Random
import Data.List

-- 21. Insert an element at a given position into a list.
insertAt :: a -> [a] -> Int -> [a]
insertAt el xs n =
                let (hs, ts) = splitAt (n-1) xs
                in hs ++ [el] ++ ts

-- 22. Create a list containing all integers within a given range.
range :: Int -> Int -> [Int]
range n m = take (m-n+1) [n..]

-- 23. Extract a given number of randomly selected elements from a list.
rndSelect :: [a] -> Int -> IO [a]
rndSelect xs n = do stdGen <- getStdGen
                    let indexes = randomRs (0, length xs-1) stdGen
                    return $ getMultiple xs $ take n indexes
    where
        getMultiple xs ys = [xs !! y | y <- ys]


-- 24. Lotto: Draw N different random numbers from the set 1..M.
diffSelect :: Int -> Int -> IO [Int]
diffSelect n m = do stdGen <- getStdGen
                    let set = (nub . randomRs (1, m)) stdGen
                    return $ take n set

-- 25. Generate a random permutation of the elements of a list.
rndPermu :: [a] -> IO [a]
rndPermu xs = do stdGen <- getStdGen
                 return [xs !! i | i <- take (length xs) $ (nub . randomRs (0, length xs-1)) stdGen]

-- 26. Generate the combinations of K distinct objects chosen from the N elements of a list
-- It means that you have to generate explicitly the list of the binomial coefficients
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations _ [] = []
combinations n (x : xs) = map(x:) (combinations (n-1) xs) ++ combinations n xs

-- 27. Group the elements of a set into disjoint subsets.
-- In how many ways can a group of n people work in m disjoint subgroups of different number of persons?
-- Write a function that generates all the possibilities and returns them in a list.
combWithRemaining :: Eq a => Int -> [a] -> [([a], [a])]
combWithRemaining n xs = map (withRemaining xs) $ combinations n xs
    where
        withRemaining ys xs = (xs, ys \\ xs)

-- I had to copy spoiler me this solution, it was too hard for me :'(
myGroup :: Eq a => [Int] -> [a] -> [[[a]]]
myGroup [] _ = [[]]
myGroup (n:ns) xs = [ g:gs | (g, rs) <- combWithRemaining n xs, gs <- myGroup ns rs]

-- 28. Sort a list of lists according to length of sublists
-- Can be done with a simple quicksort (not the best in term of efficiency tho)
lensort :: [[a]] -> [[a]]
lensort [] = []
lensort (x:xs) = let ls = [l | l <- xs, length l <= length x]
                     hs = [h | h <- xs, length h > length x]
                 in lensort ls ++ [x] ++ lensort hs


