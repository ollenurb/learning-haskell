import Data.Char

cap :: [Char] -> [Char]
cap str = map toUpper str

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = reverse . cap

fmapped :: [Char] -> [Char]
fmapped = reverse <$> cap

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> rev <*> cap

tupledMonad :: [Char] -> ([Char], [Char])
tupledMonad = do
    r <- rev
    c <- cap
    return (r, c)

tupledMonadBind :: [Char] -> ([Char], [Char])
tupledMonadBind = rev >>= (\r -> cap >>= (\c -> return (r, c)))


