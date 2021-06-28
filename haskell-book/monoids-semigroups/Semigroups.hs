module Semigroups where

import Test.QuickCheck
import Data.Monoid

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

-- Exercise 1.
data Trivial = Trivial
    deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

-- Exercise 2.
data Identity a = Identity a
    deriving (Show, Eq)

instance (Semigroup a) => Semigroup (Identity a) where
    (Identity a) <> (Identity b) = Identity (a <> b)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
    a <- arbitrary
    return $ Identity a

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = identityGen

type IdentityAssoc =
    Identity String ->
    Identity String ->
    Identity String ->
    Bool

-- Exercise 3.
data Two a b = Two a b
    deriving (Show, Eq)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

genTwo :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
genTwo = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = genTwo

type TwoAssoc =
    Two (Sum Int) String ->
    Two (Sum Int) String ->
    Two (Sum Int) String ->
    Bool

-- Exercise 4.
data Three a b c = Three a b c
    deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    (Three n m k) <> (Three n' m' k') = Three (n <> n') (m <> m') (k <> k')

threeGen :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
threeGen = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = threeGen

type ThreeAssoc =
    Three String String String ->
    Three String String String ->
    Three String String String ->
    Bool

-- Exercise 5.
data Four a b c d = Four a b c d
    deriving (Show, Eq)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
    Semigroup (Four a b c d) where
        (Four a b c d) <> (Four a' b' c' d') = Four (a <> a') (b <> b') (c <> c') (d <> d')


fourGen :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)
fourGen = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = fourGen

type FourAssoc =
    Four [Int] String String String ->
    Four [Int] String String String ->
    Four [Int] String String String ->
    Bool

-- Exercise 6, 7
newtype BoolConj = BoolConj Bool
    deriving (Show, Eq)

newtype BoolDisj = BoolDisj Bool
    deriving (Show, Eq)

instance Semigroup BoolConj where
    (BoolConj a) <> (BoolConj b) = BoolConj (a && b)

instance Semigroup BoolDisj where
    (BoolDisj a) <> (BoolDisj b) = BoolDisj (a || b)

instance Arbitrary BoolConj where
    arbitrary = do
        a <- arbitrary
        return $ BoolConj a

instance Arbitrary BoolDisj where
    arbitrary = do
        a <- arbitrary
        return $ BoolDisj a

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool
type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- Exercise 8.
data Or a b = Fst a
            | Snd b
            deriving (Show, Eq)

instance Semigroup (Or a b) where
    (Fst _) <> (Snd b) = Snd b
    (Snd a) <> _       = Snd a
    (Fst _) <> (Fst b) = Fst b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [Fst a, Snd b]

type OrAssoc =
    Or String [Int] ->
    Or String [Int] ->
    Or String [Int] ->
    Bool

-- Exercise 9.
newtype Combine a b = Combine { unCombine :: (a -> b) }

instance (Semigroup b) => Semigroup (Combine a b) where
    (Combine f) <> (Combine g) = Combine $ f <> g

-- Run tests
main :: IO ()
main = do
    quickCheck (semigroupAssoc :: TrivAssoc)
    quickCheck (semigroupAssoc :: IdentityAssoc)
    quickCheck (semigroupAssoc :: TwoAssoc)
    quickCheck (semigroupAssoc :: ThreeAssoc)
    quickCheck (semigroupAssoc :: FourAssoc)
    quickCheck (semigroupAssoc :: BoolConjAssoc)
    quickCheck (semigroupAssoc :: BoolDisjAssoc)
    quickCheck (semigroupAssoc :: OrAssoc)


