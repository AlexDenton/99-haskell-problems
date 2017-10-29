main = do
    print $ secondToLast [1,2,3,4,5]

secondToLast :: [a] -> a
secondToLast [] = error "not enough elements"
secondToLast [x] = error "not enough elements"
secondToLast [x,_] = x
secondToLast (x:xs) = secondToLast xs