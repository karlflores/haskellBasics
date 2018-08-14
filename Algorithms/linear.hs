linearsearch :: Int -> [Int] -> Bool 

linearsearch val [] = False 
linearsearch val [x] 
	| val == x = True 
	| otherwise = False 

linearsearch val (x:xs)
	| linearsearch val [x] = True 
	| otherwise = linearsearch x xs 

sums :: [Int] -> Int 

sums [] = 0
sums (x:xs) = x + sums xs
