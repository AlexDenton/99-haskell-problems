main = do
    print $ length' [1,2,3,4,5]

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs