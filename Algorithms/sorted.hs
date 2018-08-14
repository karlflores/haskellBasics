-- Merge Sort --

-- Merge Helper Function -- 
merge :: [Int] -> [Int] -> [Int]

merge xs [] = xs 
merge [] xs = xs 
merge (x:xs) (y:ys)
	| x <= y = final ++ [x] ++ merge xs (y:ys)
	| otherwise = final ++ [y] ++ merge (x:xs) ys
	where final = [] 
-- Helper Function Ends -- 

-- Sort Function -- 
msort :: [Int] -> [Int] 
msort [] = [] 
msort [x] = [x]
msort xs = merge (msort (take mid xs)) (msort (drop mid xs))
	where mid = quot (length xs) 2


-- Quick Sort -- 
qsort :: [Int] -> [Int]

qsort [] = [] 
qsort (pivot:others) = (qsort lowerlist) ++ [pivot] ++ (qsort upperlist) 
	where lowerlist = filter (<pivot) others 
	      upperlist = filter (>=pivot) others 

-- Insertion Sort -- 
isort :: [Int] -> [Int]

isort [] = [] 
isort [x] = [x]
isort (x:xs) = sorted ++ (inplace x sorted) ++ isort xs 
	where sorted = [] 

inplace :: Int -> [Int] -> [Int]
inplace x [] = [x] 
inplace val (x:xs)
	| val < x = final ++ [val] ++ inplace x xs 
	| otherwise = final ++ [x] ++ inplace val xs 
	where final = []

-- Bubble Sort -- 
-- bsort :: [Int] -> [Int]


-- Selection Sort --
-- ssort :: [Int] -> [Int] 



