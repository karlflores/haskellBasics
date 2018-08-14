-- binary search a sorted list 
bsearch :: Int -> [Int] -> Bool
-- don't forget the type signature!
bsearch val [x] 
    | x == val = True 
    | otherwise = False
bsearch val xs 
    -- Base case : if xs is none, then we return false 
    | null xs = False 
    -- search left 
    | val < xs !! pivot = bsearch val (take pivot xs)
    -- search right 
    | otherwise = bsearch val (drop pivot xs)
    where pivot = quot (length xs) 2 

