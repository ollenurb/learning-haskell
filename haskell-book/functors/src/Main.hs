module Main where

import Test.QuickCheck
import Test.QuickCheck.Function

newtype Identity a = Identity a
    deriving (Show, Eq)

data Pair a = Pair a a
    deriving (Show, Eq)

data Two a b = Two a b
    deriving (Show, Eq)

data Three a b c = Three a b c
    deriving (Show, Eq)

data Three' a b = Three' a b b
    deriving (Show, Eq)

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return $ Identity a

instance (Arbitrary a) => Arbitrary (Pair a) where
    arbitrary = do
        a <- arbitrary
        return $ Pair a a


instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Two a b

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three a b c

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Three' a b b

instance Functor Identity where
    fmap f (Identity x) = Identity $ f x

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance Functor (Three' a) where
    fmap f (Three' a b _) = Three' a (f b) (f b)

functorCompose :: (Eq (f c), Functor f) =>
    f a ->
    Fun a b ->
    Fun b c ->
    Bool
functorCompose x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type IntId = Identity Int -> IntToInt -> IntToInt -> Bool
type IntPair = Pair Int -> IntToInt -> IntToInt -> Bool
type IntTwo = Two Int Int -> IntToInt -> IntToInt -> Bool
type IntThree = Three Int Int Int -> IntToInt -> IntToInt -> Bool
type IntThree' = Three' Int Int -> IntToInt -> IntToInt -> Bool

data Possibly a = LolNope | Yeppers a
    deriving (Show, Eq)

instance Functor Possibly where
    fmap f (Yeppers a) = Yeppers $ f a
    fmap _ LolNope = LolNope

main :: IO ()
main = do
    putStrLn "Testing Identity"
    quickCheck (functorCompose :: IntId)
    putStrLn "Testing Pair"
    quickCheck (functorCompose :: IntPair)
    putStrLn "Testing Two"
    quickCheck (functorCompose :: IntTwo)
    putStrLn "Testing Three"
    quickCheck (functorCompose :: IntThree)
    putStrLn "Testing Three'"
    quickCheck (functorCompose :: IntThree')
