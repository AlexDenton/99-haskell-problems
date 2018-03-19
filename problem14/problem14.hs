main = do
    putStrLn $ show $ duplicate [1, 2, 3]

duplicate :: [a] -> [a]
duplicate [] = []
duplicate (x:xs) = x : x : duplicate xs