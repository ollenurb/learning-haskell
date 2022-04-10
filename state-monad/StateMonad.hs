-- Implement a basic finite state automata using the state monad to
-- declutter boilerplate code, taken from the official wiki example

import Control.Monad

data TurnstileState = Locked | Unlocked
    deriving (Eq, Show)

data TurnstileOutput = Thank | Open | Tut
    deriving (Eq, Show)

coin :: TurnstileState -> (TurnstileOutput, TurnstileState)
coin _ = (Thank, Unlocked)

push :: TurnstileState -> (TurnstileOutput, TurnstileState)
push Locked   = (Tut, Locked)
push Unlocked = (Open, Locked)

regularPerson :: TurnstileState -> ([TurnstileOutput], TurnstileState)
regularPerson s0 =
    let (r1, s1) = coin s0
        (r2, s2) = push s1
    in ([r1, r2], s2)

distractedPerson :: TurnstileState -> ([TurnstileOutput], TurnstileState)
distractedPerson s0 =
    let (r1, s1) = coin s0
    in ([r1], s1)

hastyPerson :: TurnstileState -> ([TurnstileOutput], TurnstileState)
hastyPerson s0 =
    let (r1, s1) = push s0
    in case r1 of
        Open -> ([r1], s1)
        Tut  -> regularPerson s1

tuesday :: TurnstileState -> ([TurnstileOutput], TurnstileState)
tuesday s0 =
    let (o1, s1) = regularPerson s0
        (o2, s2) = hastyPerson s1
        (o3, s3) = distractedPerson s2
        (o4, s4) = hastyPerson s3
    in (o1 ++ o2 ++ o3 ++ o4, s4)


newtype State s a = State { runState :: s -> (a, s) }

state :: (s -> (a, s)) -> State s a
state = State

instance Functor (State s) where
    fmap = liftM

instance Applicative (State s) where
    pure  = return
    (<*>) = ap

instance Monad (State s) where
    return x = state (\s -> (x, s))

    p >>= k = q where
        p' = runState p        -- p' :: s -> (a, s)
        k' = runState . k      -- k' :: a -> s -> (b, s)
        q' s0 = (y, s2) where  -- q' :: s -> (b, s)
            (x, s1) = p' s0    -- (x, s1) :: (a, s)
            (y, s2) = k' x s1  -- (y, s2) :: (b, s)
        q = state q'


coinS, pushS :: State TurnstileState TurnstileOutput
coinS = state coin
pushS = state push
