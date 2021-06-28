-- From54to60.hs

module From54to60 where

data Tree a = Empty
            | Branch a (Tree a) (Tree a)
    deriving (Show, Eq)

leaf :: a -> Tree a
leaf x = Branch x (Empty) (Empty)

-- 55. Create a list of balanced binary trees
cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty] 
cbalTree 1 = [leaf 'x']
cbalTree n 
    | (n-1) `mod` 2 == 0 = [Branch 'x' l r | l <- subTF, r <- subTF]
    | otherwise = concat [[Branch 'x' r l, Branch 'x' l r] | l <- subTF, r <- subTS]
    where 
        subTF = cbalTree ((n-1) `div` 2)
        subTS = cbalTree (n `div` 2)


-- 56. Let us call a binary tree symmetric if you can draw a vertical line through the root node and then 
-- the right subtree is the mirror image of the left subtree. 
-- Write a predicate symmetric/1 to check whether a given binary tree is symmetric.
symmetric :: Tree a -> Bool 
symmetric (Branch _ l r) = mirror l r 
symmetric Empty = True 

mirror :: Tree a -> Tree b -> Bool 
mirror Empty Empty = True 
mirror (Branch _ l r) (Branch _ l' r') 
    = mirror l l' && mirror r r'
mirror _ _ = False

-- 57. Define a function add/3 to add an element to a binary search tree

add :: Ord a => a -> Tree a -> Tree a 
add n (Empty) = Branch n Empty Empty    
add n t@(Branch x l r)
    | n > x  = Branch x l (add n r)
    | n < x  = Branch x (add n l) r
    | n == x = t

construct :: [Int] -> Tree Int
construct xs = foldl (flip add) Empty xs 


-- 58. Apply the generate-and-test paradigm to construct all symmetric, completely balanced binary trees 
-- with a given number of nodes. 
symCbalTrees :: Int -> [Tree Char]
symCbalTrees = filter symmetric . cbalTree


-- 59. Construct a list of height-balanced binary trees  
-- We start by defining a function to calculate the height of a given BinTree
height :: Tree a -> Int 
height Empty = 0 
height (Branch _ l r) = max (height l) (height r)
-- TOFINISH


-- 60. 
-- TOFINISH
