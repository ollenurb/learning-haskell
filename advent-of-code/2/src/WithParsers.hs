module WithParsers where

import Text.Trifecta

data InputLine = InputLine (Int, Int) Char String
    deriving (Show)

-- Requires Trifecta library, but uses base parsers from haskell base lib
lineParser :: Parser InputLine
lineParser = do
    lb <- natural
    char '-'
    ub <- natural
    whiteSpace
    c <- anyChar
    char ':'
    whiteSpace
    pwd <- some letter
    return $ InputLine (fromIntegral lb, fromIntegral ub) c pwd

solve :: (InputLine -> Bool) -> [String] -> Result Int
solve checkLines lines = do
    inputLines <- mapM (parseString lineParser mempty) lines
    let results = map checkLines inputLines
    return $ (length . filter (== True)) results

checkLine1 :: InputLine -> Bool
checkLine1 (InputLine (lb, ub) c pwd)
  = let nc = length $ filter (== c) pwd
    in  nc >= lb && nc <= ub

checkLine2 :: InputLine -> Bool
checkLine2 (InputLine (lp, up) c pwd)
  = let lc = pwd !! (lp-1)
        uc = pwd !! (up-1)
        xor p q = (p || q) && not (p && q)
    in  (lc == c) `xor` (uc == c)

solveBoth :: [String] -> String
solveBoth input
  = "Result 1: "   ++ show sol1 ++
    "\nResult 2: " ++ show sol2 ++ "\n"
    where
        sol1 = solve checkLine1 input
        sol2 = solve checkLine2 input

main :: IO ()
main = interact (solveBoth . lines)
