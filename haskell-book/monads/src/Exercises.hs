module Exercises where

-- 1.
data Nope a = NopeDotJpg

instance Functor Nope where
    fmap _ _ = NopeDotJpg

instance Applicative Nope where
    pure = \_ -> NopeDotJpg
    f <*> x = NopeDotJpg

instance Monad Nope where
    return = pure
    _ >>= _ = NopeDotJpg

-- 2.
data PhbtEither b a = Lefty a
                    | Righty b
    deriving (Show, Eq)

instance Functor (PhbtEither b) where
    fmap f (Lefty a) = Lefty (f a)

instance Applicative (PhbtEither b) where
    pure = Lefty
    (Lefty f) <*> (Lefty x) = Lefty (f x)

instance Monad (PhbtEither b) where
    return = pure
    (Lefty x) >>= f = f x

-- 3.
newtype Identity a = Identity a
    deriving (Show, Eq, Ord)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
    pure = Identity
    (Identity f) <*> (Identity x) = Identity $ f x

instance Monad Identity where
    return = pure
    (Identity x) >>= f = f x

-- 4.
data List a = Nil
            | Cons a (List a)
    deriving (Show)

-- Monoidal function useful for applicative
glue :: List a -> List a -> List a
glue (Cons x xs) ys  = Cons x $ glue xs ys
glue Nil (Cons y ys) = Cons y $ glue Nil ys
glue Nil Nil         = Nil

instance Functor List where
    fmap f (Nil)       = Nil
    fmap f (Cons a xs) = Cons (f a) $ fmap f xs

instance Applicative List where
    pure = (flip Cons) Nil
    (Cons f xs) <*> ys = glue (fmap f ys) (xs <*> ys)

instance Monad List where
    return = pure
    (Cons x xs) >>= f = glue (f x) (xs >>= f)
    (Nil) >>= _ = Nil

-- 1b.
j :: Monad m => m (m a) -> m a
j m = m >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f = flip (>>=) $ (return . f)

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f m1 m2 = do x <- m1
                y <- m2
                return $ f x y

a :: Monad m => m a -> m (a -> b) -> m b
a m mf = do x <- m
            f <- mf
            return $ f x

-- meh :: Monad m => [a] -> (a -> m b) -> m [b]
-- meh (x:xs) f = f x >>= (\u -> (meh xs f) >>= (\z -> return $ u : z ))
-- meh [] _ = return []

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = do u <- f x
                  z <- meh xs f
                  return $ u : z


