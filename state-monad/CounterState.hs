import Control.Monad
import Control.Monad.State.Lazy


data GameState = MkGameState { p1Score :: Int, p2Score::Int }
    deriving (Show)

gameState :: Int -> Int -> GameState
gameState = MkGameState

incPlayer1 :: GameState -> GameState
incPlayer1 gs = MkGameState p1Score gs + 1 $ p2Score gs

incPlayer1S :: State GameState ()
incPlayer1S = modify incPlayer1
-- modify is just a particular "constructor for the state monad" that
-- basically throws away any intermediate computation.

inc2TimesPlayer1S :: State GameState ()
inc2TimesPlayer1S = do incPlayer1S
                       incPlayer1S

newGameState :: State GameState ()
newGameState = state const ((), MkGameState 0 0)
