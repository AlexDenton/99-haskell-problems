main = do
    print $ last' [1, 2, 3, 4]
    print $ last' ["first", "second", "last"]
    print $ last'' [1, 2, 3]

last' :: [a] -> a
last' [] = canNotCallLastOnEmptyList
last' (x:[]) = x
last' (x:xs) = last xs

last'' :: [a] -> a
last'' [] = canNotCallLastOnEmptyList
last'' xs = xs !! ((length xs) - 1)

canNotCallLastOnEmptyList = error "Can not call 'last' on an empty list"
