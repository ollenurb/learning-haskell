-- Addition.hs
module Addition where

import Data.List (sort)
import Test.Hspec
import Test.QuickCheck

sayHello :: IO ()
sayHello = putStrLn "hello!"

main :: IO ()
main = hspec $ do
    describe "Division" $ do
        it "15 divided by 3 is 5" $ do
            dividedBy 15 3 `shouldBe` (5, 0)
        it "22 divided by 5 is 4 remainder 2" $ do
            dividedBy 22 5 `shouldBe` (4, 2)

dividedBy:: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where go n   d count
           | n < d = (count, n)
           | otherwise = go (n-d) d (count+1)


recMult :: (Eq a, Num a) => a -> a -> a
recMult 0 _ = 0
recMult n m = m + recMult (n - 1) m

testMult :: IO ()
testMult = hspec $ do
    describe "Multiplication" $ do
        it "3 times 2 is 6" $ do
            recMult 2 3 `shouldBe` 6
        it "6 times 3 is 18" $ do
            recMult 3 6 `shouldBe` 18

qtAddition :: IO ()
qtAddition = hspec $ do
    describe "Addition" $ do
        it "x + 1 is always greater than x" $ do
            property $ \x -> x + 1 > (x :: Int)


