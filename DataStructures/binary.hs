data Tree a = Node a (Tree a) (Tree a) | Empty
    deriving Show

-- copy over your solution to the previous exercise,
-- so that you have access to the 'insert' function

-- define your function here
buildtree :: Ord a => [a] -> Tree a 
-- don't forget the type signature!
-- if an empty list just return empty tree 
buildtree [] = Empty 
-- if one element just insert that element into a tree
buildtree [x] = insert x Empty 

-- if more than one element we insert the first element
-- into the tree and build the rest of the tree 
buildtree (x:xs) = insert x (buildtree xs)

-- define your function here
insert :: Ord a => a -> Tree a -> Tree a 

insert x Empty = (Node x Empty Empty)
insert y (Node x l r) 
    | x > y = Node x (insert y l) r
    | x < y = Node x l (insert y r)
    | otherwise = Node x l r 

treesort :: Ord a => Tree a -> [a]

treesort Empty = []
-- treesort  = [x]
treesort (Node x l r) = treesort l ++ output ++ treesort r 
	where output = [x] 

