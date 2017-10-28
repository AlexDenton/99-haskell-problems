main = do
    print $ last' [1, 2, 3, 4]
    print $ last' ["first", "second", "last"]

last' :: [a] -> a
last' [] = error "Can not call 'last' on an empty list"
last' (x:[]) = x
last' (x:xs) = last xs