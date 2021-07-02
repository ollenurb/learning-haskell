import Data.Maybe

-- Create a datatype to hold the single line
data InputLine = InputLine (Int, Int) Char String
    deriving (Show)

-- TO REMEMBER: In a maybe context, patterns that doesn't match are
-- automatically turned into a Nothing
-- Inside the Maybe's monadic context, unmatched patterns that otherwise would
-- terminate the program with an exception are just turned into a Nothing
-- value.
parseInputLine :: String -> Maybe InputLine
parseInputLine str = do
    [nr,c:':':_,pwd] <- pure $ words str
    (lb, '-':ub) <- pure $ span (/='-') nr
    return $ InputLine (readInt lb, readInt ub) c pwd

-- Just to make code little bit cleaner
readInt :: String -> Int
readInt = read

-- If one of the lines fails to parse, just return Nothing
solve :: (InputLine -> Bool) -> [String] -> Maybe Int
solve checkLines lines = do
    inputLines <- mapM parseInputLine lines
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
        sol1 = fromJust $ solve checkLine1 input
        sol2 = fromJust $ solve checkLine2 input

main :: IO ()
main = interact (solveBoth . lines)
