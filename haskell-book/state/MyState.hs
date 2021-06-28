import Data.Functor

newtype Moi s a = Moi { runState :: s -> (a, s) }

instance Functor (Moi s) where
    fmap f (Moi g) = Moi $ \s0 -> let (a, s1) = g s0 in (f a, s1)


instance Applicative (Moi s) where
    pure a = Moi (\s -> (a, s))
    (Moi f) <*> (Moi g) = Moi $ \s0 -> let (f1, s1) = f s0
                                           (a,  s2) = g s1
                                        in (f1 a, s2)

instance Monad (Moi s) where
    return = pure

    -- (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
    (Moi f) >>= g = Moi $ \s0 -> let (a, s1)  = f s0
                                     (Moi f2) = g a
                                     (b, s2)  = f2 s1
                                 in  (b, s2)


get :: Moi s s
get = Moi $ \s -> (s, s)

put :: s -> Moi s ()
put s = Moi $ \_ -> ((), s)

exec :: Moi s a -> s -> s
exec moi s = let (a, s') = (runState moi) s in s'

eval :: Moi s a -> s -> a
eval moi s = let (a, s') = (runState moi) s in a

modify :: (s -> s) -> Moi s ()
modify f = Moi (\s -> ((), f s))
