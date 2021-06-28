-- Identity
newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Foldable Identity where
    foldr f z (Identity a) = f a z

instance Traversable Identity where
    traverse f (Identity a) = fmap (Identity) (f a)


newtype Constant a b = Constant { getConstant :: a }

instance Functor (Constant a) where
    fmap _ (Constant y) = Constant y

-- Maybe
data Optional a = Nada
                | Yep a
            deriving (Show, Eq)

instance Functor Optional where
    fmap _ (Nada) = Nada
    fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
    foldr _ z (Nada) = z
    foldr f z (Yep a) = f a z

instance Traversable Optional where
    traverse f (Nada) = pure Nada
    traverse f (Yep a) = Yep <$> f a

-- List
data List a = Nil
            | Cons a (List a)
            deriving (Show, Eq)

instance Functor List where
    fmap _ (Nil) = Nil
    fmap f (Cons a xs) = Cons (f a) (fmap f xs)

instance Foldable List where
    foldr _ z (Nil)       = z
    foldr f z (Cons a xs) = f a (foldr f z xs)

instance Traversable List where
    traverse _ (Nil) = pure Nil
    traverse f (Cons x xs) = (Cons) <$> f x <*> traverse f xs

-- Three
data Three a b c = Three a b c
    deriving (Show, Eq)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
    foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
    traverse f (Three a b c) = Three a b <$> f c
