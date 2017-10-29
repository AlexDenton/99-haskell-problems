main = do
    print $ elementAt [1,2,3] 2
    print $ elementAt [1,2] 3

elementAt [] _ = error "out of range"
elementAt (x:_) 1 = x
elementAt (_:xs) k
    | k < 1 = error ""
    | otherwise = elementAt xs (k-1)