data List a = ListNode a (List a) | ListEnd
    deriving (Eq, Show) 

mymaximum :: Ord a => List a -> a

-- pattern match with the end of the list : recursive basecase 
mymaximum (ListNode a ListEnd) = a

-- processing the rest of the list 
mymaximum (ListNode x xs)
    | x > (mymaximum xs) = x 
    | otherwise = (mymaximum xs)
