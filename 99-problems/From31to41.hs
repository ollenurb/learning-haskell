-- From31to41.hs

module From31to41 where

import Data.List

-- 31. Determine whether a given integer number is prime
isPrime :: Int -> Bool
isPrime 1 
    = False
isPrime n 
    = let sqrd = (floor . sqrt . fromIntegral) n
      in length [x | x <- [2..sqrd], n `mod` x == 0] == 0
 
-- 32. Determine the greatest common divisor of two positive integer 
-- numbers. (Using Euclid's algorithm)
euclid :: Int -> Int -> Int
euclid a 0 
    = a
euclid a b 
    = euclid b $ a `mod` b

-- 33. Determine whether two positive integer numbers are coprime. 
-- Two numbers are coprime if their greatest common divisor equals 1.
coprime :: Int -> Int -> Bool 
coprime a b 
    = euclid a b == 1

-- 34. Calculate Euler's totient function phi(m)
-- Euler's so-called totient function phi(m) is defined as the number
-- of positive integers r (1 <= r < m) that are coprime to m. 
totient :: Int -> Int 
totient 1 = 1
totient m 
    = length [x | x <- [1..m], coprime x m]

-- 35. Determine the prime factors of a given positive integer. 
-- Construct a flat list containing the prime factors in ascending order. 
primeFactors :: Int -> [Int]
primeFactors n 
    = primeFactors' 2 n 

primeFactors' :: Int -> Int -> [Int]
primeFactors' f n 
    | n <= 1         = []
    | n `mod` f == 0 = f : primeFactors' f  (n `div` f)
    | otherwise      = primeFactors' (f + 1) n

-- 36. Given an integer, construct a list containing its prime 
-- factors and their multiplicity
primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult n 
    = map (\(x:xs)-> (x, length xs + 1)) $ group $ primeFactors n

-- 37. Calculate Euler's totient function phi(m) (improved)
phi :: Int -> Int 
phi m 
    = product [(p - 1) * p ^ (c - 1) | (p, c) <- primeFactorsMult m] 

-- 38. No solution required

-- 39. Given a range of integers by its lower and upper limit, 
-- construct a list of all prime numbers in that range.
primesR :: Int -> Int -> [Int]
primesR lo up 
    = [x | x <- [lo..up], isPrime x]

-- 40. Goldbach's conjecture says that every positive even number 
-- greater than 2 is the sum of two prime numbers. Example: 28 = 5 + 23
-- Given a number, compute its goldbach's numbers
type GoldBach = (Int, Int)
goldbach :: Int -> (Int, Int)
goldbach n  
    = let primesList = primesR 2 n 
      in head [(p, p') | p <- primesList
                       , p' <- primesList
                       , (p + p') == n]

-- 41. Given a range of integers by its lower and upper limit, print a list
-- of all even numbers and their Goldbach composition. 
goldbachList :: Int -> Int -> [GoldBach]
goldbachList lo up 
    = [goldbach n | n <- [lo..up], n `mod` 2 == 0]


