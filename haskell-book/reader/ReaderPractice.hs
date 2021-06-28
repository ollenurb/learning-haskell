module ReaderPractice where

import Control.Applicative
import Data.Maybe

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' k = lookup k $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = pure (,) <*> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = pure (,) <*> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = (,) <$> z' <*> z'

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = pure (&&) <*> (>3) <*> (<8)

-- ((->) Int) ()
-- <$> :: (a -> b) -> f a -> f b
-- <$> :: ((Bool -> Bool) -> Bool) -> ((-> Int) (Bool -> Bool)) -> ((->Int) Bool)
-- <*> :: (Int -> ((Bool -> Bool) -> Bool)) -> (Int -> (Bool -> Bool)) -> (Int -> Bool)

main :: IO ()
main = do

    print $ sequenceA [Just 3, Just 2, Just 1]
    print $ sequenceA [x, y]
    print $ sequenceA [xs, ys]
    print $ summed <$> ((,) <$> xs <*> ys)
    print $ fmap summed ((,) <$> xs <*> zs)
    print $ bolt 7
    print $ fmap bolt z
    print $ sequenceA [(>3), (<8), even] 7

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

seqAFolded :: Integral a => a -> Bool
seqAFolded = (foldr (&&) True) <$> sequA

ssequA = sequA $ fromMaybe (-1) s'

