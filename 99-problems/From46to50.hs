-- From46to50.hs

module From46to50 where 

import Control.Monad (replicateM) 

-- 46. Define and/or/nand/nor/xor/impl/equ predicates. 
-- Define a predicate table which prints the truth table of a given logical expression 

and' :: Bool -> Bool -> Bool 
and' True True = True
and' _    _    = False

or' :: Bool -> Bool -> Bool 
or' True _ = True
or' _ True = True
or' _    _ = False

nand' :: Bool -> Bool -> Bool 
nand' a b = not $ and' a b  

nor' :: Bool -> Bool -> Bool 
nor' a b = not $ or' a b 

xor' :: Bool -> Bool -> Bool 
xor' True True = False
xor' a    b    = or' a b 

impl' :: Bool -> Bool -> Bool 
impl' a b = not a `or'` b

equ' :: Bool -> Bool -> Bool 
equ' a b = not $ xor' a b

table :: (Bool -> Bool -> Bool) -> IO ()
table p 
    = mapM_ (putStrLn . pprint) combs
    where
        combs = [(f, s, p f s) | f <- [True, False], s <- [True, False]]
        pprint (f, s, t) = show f ++ "\t" ++ show s ++ "\t" ++ show t


-- 47. Write operators in a way so that they can be written in infix notation. 
infixl 4 `or'`
infixl 6 `and'`

-- 48. Define table/2 in a way that table(List,Expr) prints the truth table for 
-- the expression Expr, which contains the logical variables enumerated in List. 
table' :: Int -> ([Bool] -> Bool) -> IO ()
table' n p 
    = mapM_ (putStrLn . pprint) combs
    where
        combs = [row ++ [p row] | row <- replicateM n [True, False]]
        pprint r = concat $ map (\x -> show x ++ "\t") r  

-- 49. Write a Gray code given n and G 
gray :: Int -> [String]
gray 1 = ["0", "1"]
gray n = map((:) '0') prec ++ map((:) '1') (reverse prec)
    where 
        prec = gray $ n-1

-- 50.


