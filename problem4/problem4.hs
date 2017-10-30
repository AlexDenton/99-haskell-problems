main = do
    print $ length' [1,2,3,4,5]
    print $ length' []
    print $ length'' [1,2,3]

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

length'' :: [a] -> Int
length'' xs = foldr (\_ -> (+1)) 0 xs