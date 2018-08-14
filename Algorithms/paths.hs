paths :: Integer -> Integer -> Integer 
 -- Take inputs x,y
paths m n
	-- base case -- this is when you have a single row : there is only one paths  
 	| m == 1 || n == 1 = 1 

	-- these two base cases are irrelevant : don't need to explore the bounds since you are only going left and right 
	-- 
	-- otherwise you have to explore both directions 
	| otherwise = paths (m - 1) n + paths m (n - 1)
