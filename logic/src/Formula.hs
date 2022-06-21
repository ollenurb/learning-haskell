module Formula where

encapsulate :: a -> [a] -> [a]
encapsulate x xs = x:xs ++ [x]

type Symbol = String

data Formula = BinaryOp Symbol Formula Formula
             | UnaryOp Symbol Formula
             | Atom Symbol

inorder :: Formula -> String
inorder (Atom p) = p
inorder (UnaryOp op f) = op ++ inorder f
inorder (BinaryOp op l r) = inorder l ++ encapsulate ' ' op ++ inorder r

-- Represents a formula with inorder notation with parentheses
inorder' :: Formula -> String
inorder' (Atom p) = p
inorder' (UnaryOp op f) = op ++ inorder' f
inorder' (BinaryOp op l r) =
    "(" ++ inorder' l ++ encapsulate ' ' op ++ inorder' r ++ ")"

-- Represents a formula in reverse Polish notation
preorder :: Formula -> String
preorder (Atom p) = p
preorder (UnaryOp op f) = op ++ preorder f
preorder (BinaryOp op l r) = op ++ preorder l ++ preorder l

testFormula :: Formula
testFormula =
    BinaryOp "↔"
    (BinaryOp "→" (Atom "p") (Atom "q"))
    (BinaryOp "→" (UnaryOp "¬" (Atom "p")) (UnaryOp "¬" (Atom "p")))


