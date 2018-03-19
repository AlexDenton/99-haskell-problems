main = do
    putStrLn $ show $ encode "aaaabccaadeeee"

encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode (x:xs) =
    let g = takeWhile (==x) xs
    in (length g + 1, x) : encode (dropWhile (/=x) xs)