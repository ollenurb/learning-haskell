import Control.Applicative (liftA3)

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)

-- Write Applicative instances of the following data types
-- Ex 1
data Pair a = Pair a a
    deriving Show

instance Functor Pair where
    fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
    (Pair f g) <*> (Pair a b) = Pair (f a) (g b)
    pure a = Pair a a
