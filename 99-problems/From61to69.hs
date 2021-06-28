-- From61to69.hs

module From61to69 where 

import From54to60

-- defined tree for testing purposes
tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

tree64 :: Tree Char 
tree64 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'h'
                                        (Branch 'g'
                                                (Branch 'e' Empty Empty)
                                                Empty
                                        )
                                        Empty
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 's'
                                        (Branch 'q' Empty Empty)
                                        Empty
                                )
                        )
                        Empty
                ) 


-- 61. Count leaves in a bintree
count :: Tree a -> Int 
count Empty = 0 
count (Branch _ Empty Empty) = 1 
count (Branch _ l r) = count l + count r

leaves :: Tree a -> [a]
leaved Empty = []
leaves (Branch x Empty Empty) = [x]
leaves (Branch _ l r) = leaves l ++ leaves r 

-- 62. Collect the internal nodes of a binary tree in a list 
-- An internal node of a binary tree has either one or two non-empty successors.
internals :: Tree a -> [a]
internals node
    | (Branch x l _) <- node = x : internals l  
    | (Branch x _ r) <- node = x : internals r
    | otherwise                  = []

atLevel :: Tree a -> Int -> [a]
atLevel (Empty)        n = []
atLevel (Branch x l r) n 
    | n == 1    = [x]
    | otherwise = atLevel l (n-1) ++ atLevel r (n-1) 


-- 63. Complete binary tree
complete :: Int -> Tree Char
complete n = generate n 1 


generate :: Int -> Int -> Tree Char
generate n l
    | l <= n    = Branch 'x' (generate n (l*2)) (generate n (l*2+1))
    | otherwise = Empty 

-- 64. Write a function to annotate each node of the tree with a position, where (1,1) in the 
-- top left corner or the rectangle bounding the drawn tree. 
type Coords = (Int, Int)

layout :: Tree a -> Tree (a, Coords) 
layout = fst . ((flip annotate) (1,1)) 

-- Post: Returns the annotated tree and the last annotated label 
-- (right subtree)
annotate :: Tree a -> Coords -> (Tree (a, Coords), Coords)
annotate (Empty)        coords = (Empty, coords)
annotate (Branch e l r) (x, y) = (genBranch, (rx, ry))
    where 
        genBranch = Branch((e, (lx, ly))) lt rt
        (lt, (lx, ly)) = annotate l (x, y+1)
        (rt, (rx, ry)) = annotate r (lx+1, y+1)

-- 65. An alternate method of annotating a tree
