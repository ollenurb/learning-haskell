module Idempotence where

import Test.Hspec
import Test.QuickCheck
import Data.List (sort)
import Data.Char

twice :: (a -> a) -> (a -> a)
twice f = f . f

fourTimes :: (a -> a) -> (a -> a)
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord = map toUpper

f :: String -> Bool
f x = (capitalizeWord x == twice     capitalizeWord x) &&
      (capitalizeWord x == fourTimes capitalizeWord x)

propf :: Property
propf = forAll arbitrary f

g :: Ord a => [a] -> Bool
g x = (sort x == twice     sort x) &&
      (sort x == fourTimes sort x)

propg :: Property
propg = forAll (arbitrary :: Gen [Int]) g

main :: IO ()
main = do
    quickCheck propf
    quickCheck propg
