module Chapter where

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
    fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea

instance Applicative m => Applicative (EitherT e m) where
    pure x = EitherT $ (pure . pure) x
    (EitherT f) <*> (EitherT mea)
      = EitherT $ (<*>) <$> f <*> mea

instance Monad m => Monad (EitherT e m) where
    return = pure
    (EitherT ma) >>= f = EitherT $ do
        v <- ma
        case v of
          Left e  -> return (Left e)
          Right a -> runEitherT (f a)


swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT et = EitherT $ fmap swapEither $ runEitherT et

swapEither :: Either e a -> Either a e
swapEither (Left v)  = (Right v)
swapEither (Right v) = (Left v)


