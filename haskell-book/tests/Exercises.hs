module Exercises where

import Data.List (sort)
import Test.Hspec
import Test.QuickCheck

-- Ex. 1
half :: Fractional a => a -> a
half x = x / 2

floatGen :: Gen Float
floatGen = arbitrary

prop_halfIdChecker :: Float  -> Bool
prop_halfIdChecker x = x == idHalf x
    where idHalf = (*2) . half

-- Ex. 2
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
    snd $ foldr go (Nothing, True) xs
    where go _ status@(_, False) = status
          go y (Nothing, t) = (Just y, t)
          go y (Just x, t) = (Just y, x >= y)

prop_listOrdered :: [Int] -> Bool
prop_listOrdered xs = listOrdered (sort xs)

-- Ex. 3
plusAssociative x y z =
    x + (y + z) == (x + y) + z

plusCommutative x y =
    x + y == y + x

threeInts :: Gen (Int, Int, Int)
threeInts = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (a, b, c)

testAssSum :: Property
testAssSum = forAll threeInts (\(x, y, z) -> plusAssociative x y z)

pairInts :: Gen (Int, Int)
pairInts = do
    a <- arbitrary
    b <- arbitrary
    return (a, b)

testCommSum :: Property
testCommSum = forAll pairInts (\(x, y) -> plusCommutative x y)

-- Ex. 4
multAssociative x y z =
    x * (y * z) == (x * y) * z

multCommutative x y =
    x * y == y * x

testAssMult :: Property
testAssMult = forAll threeInts (\(x, y, z) -> multAssociative x y z)

testCommMult :: Property
testCommMult =
    forAll pairInts (\(x, y) -> multCommutative x y)

-- Ex. 5
testQuotRem :: Property
testQuotRem = forAll pairInts prop
    where prop (x, y) = (quot x y) * y + (rem x y) == x

testMod :: Property
testMod = forAll pairInts prop
    where prop (x, y) = (div x y) * y + (mod x y) == x

-- Ex. 6


testExpComm :: Property
testExpComm = forAll pairInts prop
    where prop (x, y) = (x ^ y) == (y ^ x)

testExpAss :: Property
testExpAss = forAll threeInts prop
    where prop (x, y, z) = (x ^ y) ^ z == z ^ (y ^ x)

-- Ex. 7
intListGen :: Gen [Int]
intListGen = arbitrary

identityReverse :: Property
identityReverse = forAll intListGen prop
    where prop xs = (reverse . reverse) xs == id xs

-- Ex. 9
prop9a :: Property
prop9a = forAll intListGen prop
    where prop x = foldr (:) [] x == [] ++ x

prop9b :: Property
prop9b = forAll (arbitrary :: Gen [[Int]]) prop
    where prop x = foldr (++) [] x == concat x

-- Ex. 10
f :: Int -> [a] -> Bool
f n xs = length (take n xs) == n

prop10 :: Property
prop10 = forAll (arbitrary :: Gen (Int, [Int])) prop
    where prop (x, xs) = f x xs

-- Ex. 11
prop11 :: Property
prop11 = forAll (arbitrary :: Gen String) prop
    where prop x = (read (show x)) == x


