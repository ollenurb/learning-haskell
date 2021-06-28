module Monoids where

import Test.QuickCheck
import Semigroups

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = a `mappend` mempty == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = mempty `mappend` a == a

instance Monoid Trivial where
    mempty = Trivial
    mappend = (<>)

-- Exercise 2.
instance (Monoid a) => Monoid (Identity a) where
    mempty = Identity mempty
    mappend = (<>)

-- Exercise 3.
instance (Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty
    mappend = (<>)

-- Exercise 4.
instance Monoid BoolConj where
    mempty = BoolConj True
    mappend = (<>)

-- Exercise 5.
instance Monoid BoolDisj where
    mempty = BoolDisj False
    mappend = (<>)

-- Exercise 6.
instance (Monoid b) => Monoid (Combine a b) where
    mempty = Combine $ mempty
    mappend = (<>)

-- Exercise 8.

newtype Mem s a = Mem { runMem :: s -> (s, a) }

combineMems f g x =
  let
    (a, b) = g x
    (c, d) = f b
  in
    (a <> c, d)

instance (Semigroup a) => Semigroup (Mem s a) where
  (<>) (Mem { runMem = f })
       (Mem { runMem = g }) = Mem $ combineMems f g

instance Monoid a => Monoid (Mem s a) where
    mempty = Mem $ \x -> (mempty, x)
    mappend = (<>)

f' = Mem $ \s -> ("hi!", s+1)

testMem :: IO ()
testMem = do
    let rmzero = runMem mempty 0
        rmleft = runMem (f' <> mempty) 0
        rmright = runMem (mempty <> f') 0
    print $ rmleft
    print $ rmright
    print $ (rmzero :: (String, Int))
    print $ rmleft == runMem f' 0
    print $ rmright == runMem f' 0

main :: IO ()
main = do
    quickCheck (semigroupAssoc :: TrivAssoc)
    quickCheck (monoidLeftIdentity :: Trivial -> Bool)
    quickCheck (monoidRightIdentity :: Trivial -> Bool)


