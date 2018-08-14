-- an AVL tree has a height corresponding to it as well 
data AVLTree a = Node a (AVLTree a) (AVLTree a) | Empty
    deriving Show

buildtree :: Ord a => [a] -> AVLTree a 
-- don't forget the type signature!
-- if an empty list just return empty tree 
buildtree [] = Empty 
-- if one element just insert that element into a tree
buildtree [x] = insert x Empty

-- if more than one element we insert the first element
-- into the tree and build the rest of the tree 
buildtree (x:xs) = balancing_insert new 
	where new = insert x (buildtree xs)

insert :: Ord a => a -> AVLTree a -> AVLTree a 
-- insert a node into the AVL tree and perform the balancing/rotation step

-- base case : if there is no tree, return a node with the value 
insert x Empty = (Node x Empty Empty)
-- recursive case : balance the subtrees 
insert y (Node x l r ) 
    | x > y = Node x (balancing_insert (insert y l)) r 
    | x < y = Node x l (balancing_insert (insert y r))  
    | otherwise = balancing_insert(Node x l r) 

balancing_insert :: AVLTree a -> AVLTree a
-- helper function 
-- balance the resultant tree after an insert has been performed 
balancing_insert (Node a l r )  
  | (balance (Node a l r )) > 1 && (balance r) > 0 = rotate_left (Node a l r)  
  | (balance (Node a l r )) < -1 && (balance l) < 0 = rotate_right (Node a l r ) 
  | (balance (Node a l r )) > 1 && (balance l) < 0 = rotate_left (Node a l (rotate_right r)) 
  | (balance (Node a l r )) < -1 && (balance r) > 0 = rotate_right (Node a (rotate_left l) r)
  | otherwise = (Node a l r ) 
	
treesort :: AVLTree a -> [a]
-- sort the tree 
treesort Empty = []
-- treesort  = [x]
treesort (Node x l r) = treesort l ++ output ++ treesort r 
	where output = [x] 

height :: AVLTree a -> Int
-- calculate the height of a tree 
height Empty = -1 
height (Node x l r) 
	| height l > height r = height l + 1
	| otherwise = height r + 1

balance :: AVLTree a -> Int
-- calculate the balance of a tree (r - l) 
balance Empty = 0 
balance (Node x l r) = height r - height l 

rotate_left :: AVLTree a -> AVLTree a
-- rotate a subtree left about its root
rotate_left Empty = Empty 
rotate_left (Node a t1 (Node r t2 t3)) = (Node r (Node a t1 t2) t3)

rotate_right :: AVLTree a -> AVLTree a
-- rotate a subtree right about its root
rotate_right Empty = Empty  
rotate_right (Node a (Node l t1 t2) r) = (Node l t1 (Node a t2 r))

