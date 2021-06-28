module ListApp where

data List a = Nil
            | Cons a (List a)
    deriving (Show, Eq)

instance Functor List where
    fmap _ (Nil)       = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
    pure x = (Cons x Nil)
    (Cons f xs) <*> ys = (fmap f ys) <> (xs <*> ys)
    Nil <*> ys = Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f (Cons x xs) = (f x) <> (flatMap f xs)
flatMap _ (Nil) = Nil

instance Semigroup (List a) where
    (Cons x xs) <> b = Cons x $ xs <> b
    (Nil) <> b = b

instance Monoid (List a) where
    mempty = Nil



