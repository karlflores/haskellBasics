data Tree a = Node a (Tree a) (Tree a) | Empty
-- show elements in the list as they appear left to right 
elements :: Ord a => Tree a -> [a]
elements Empty = []
elements (Node x Empty Empty) = [x]

-- need to do an in order traversal -- visit left subtree, then mark visited
-- then visit the right subtree
elements (Node x l r) = elements l ++ output ++ elements r 
    where output = [x]
