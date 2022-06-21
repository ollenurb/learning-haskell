module Main where

-- Lambda expressions must be represented using DeBrujin's notation In this
-- Integers are represented using machine-encoded integers for performance
-- reasons.
data LExpr = LApp LExpr LExpr
          | LVar Int
          | LAbs LExpr
          | LConst Int
          | LPlus LExpr LExpr
          deriving (Show, Eq)

type Code = [Instruction]

data Instruction = IConst Int
                 | IClo Code
                 | IRet
                 | IApp
                 | IAdd
                 | IAcc Int
                 deriving (Show, Eq)

data Data = DConst Int
          | DClo Code Env
          | DUndefined
          deriving (Show, Eq)

-- Standard notation vs De Brujin's one
-- (\x.x + 1)2 <=> \.(#1 + 1) 2
add :: LExpr
add = LApp (LAbs (LPlus (LVar 0) (LConst 1))) (LConst 2)

add2 :: LExpr
add2 = LAbs (LPlus (LVar 0) (LConst 1))

compile :: LExpr -> Code
compile (LConst k)  = [IConst k]
compile (LAbs a)    = [IClo $ compile a ++ [IRet]]
compile (LApp m n)  = compile n ++ compile m ++ [IApp]
compile (LVar i)    = [IAcc i]
compile (LPlus a b) = compile b ++ compile a ++ [IAdd]

type Stack = [Data]
type Env = [Data]

type MachineState = (Code, Env, Stack)

-- Given an initial state, execute the code until no more code is left
exec :: MachineState -> MachineState
exec (IConst k:c, e, s) = exec  (c, e, DConst k:s)
exec (IClo c':c, e, s) = exec (c, e, DClo c' e:s)
exec (IApp:c, e, (DClo c' e'):v:s) = exec (c', v:e', DClo c e:s)
exec (IRet:c, e, v:(DClo c' e'):s) = exec (c', e', v:s)
exec (IAdd:c, e, (DConst n):(DConst m):s) = exec (c, e, DConst(n + m):s)
exec (IAcc n:c, e, s) = exec (c, e, e !! n:s)
exec ms@([], [], s) = ms
exec (c, e, s) = (c, e, DUndefined : s)

-- Evaluate the expression
eval :: LExpr -> MachineState
eval expr = exec (c, [], [])
    where c = compile expr

main :: IO ()
main = undefined
