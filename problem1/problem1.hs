main = do
    print $ last' [1, 2, 3, 4]
    print $ last' ["first", "second", "last"]

last' :: [a] -> a
last' (x:[]) = x
last' (x:xs) = last xs