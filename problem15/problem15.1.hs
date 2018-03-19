main = do
    putStrLn $ show $ duplicateElements 3 "abc"

duplicateElements :: Int -> [a] -> [a]
duplicateElements _ [] = []
duplicateElements n (x:xs) = replicate n x ++ duplicateElements n xs