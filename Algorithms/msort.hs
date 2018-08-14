-- merge two lists together 
merge :: [Int] -> [Int] -> [Int] 
merge [] xs = xs 
merge xs [] = xs 
merge (x:xs) (y:ys)	
	| x < y = final ++ [x] ++ [y] ++ merge xs ys
	| otherwise = final ++ [y] ++ [x] ++ merge xs ys 
	where final = []
