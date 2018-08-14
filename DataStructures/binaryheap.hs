data Heap t = Node t (Heap t) (Heap t) | Empty 
	deriving (Eq, Show) 

insert ::  (Ord t) => t -> Heap t -> Heap t

insert y Empty = Node y Empty Empty 
insert y (Node x l r) 
  | height l > height r = swap (<) (Node x l (insert y r))
  | otherwise = swap (<) (Node x (insert y l) r )

swap :: (t -> t -> Bool) -> Heap t -> Heap t
swap f Empty = Empty 
--swap f (Node x (Node y lc rc) r) 
--  | (f x y) = (Node y (Node x lc rc) r)
--  | otherwise = (Node x (Node y lc rc) r)
--swap f (Node x l (Node y lc rc)) 
--  | (f x y) = (Node y l (Node x lc rc))
--  | otherwise = (Node x l (Node y lc rc)) 
swap f (Node x Empty Empty) = (Node x Empty Empty)
swap f (Node x (Node y l r) Empty) 
  | f y x = Node y (Node x l r) Empty
  | otherwise = Node x (Node y l r) Empty
swap f (Node x Empty (Node y l r)) 
  | f y x = Node y Empty (Node x l r)
  | otherwise = Node x Empty (Node y l r)
swap f (Node x (Node y ll rl) (Node z lr rr))
  | (f z y) && (f y x) = Node z (Node y ll rl) (Node x lr rr)
  | (f y z) && (f z x) = Node y (Node x ll rl) (Node z lr rr)
  | otherwise = (Node x (Node y ll rl) (Node z lr rr))


height :: Heap t -> Int 
height Empty = -1
height (Node x l r) 
  | height l > height r = height l + 1
  | otherwise = height r + 1

--delete :: (Ord t) => t -> Heap t -> Heap t

--poll :: 

--peek :: 

--upheap :: 

--downheap :: 

-- upsift :: 
 
--downshift :: 
