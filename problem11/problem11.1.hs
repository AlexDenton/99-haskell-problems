main = do
    putStrLn $ show $ encode "aaaabccaadeeee"

encode :: Eq a => [a] -> [EncodedElement a]
encode [] = []
encode (x:xs) =
    let g = takeWhile (==x) xs
    in
        (if length g == 1 then
            Single x
        else
            Multiple ((length g) + 1) x)
    : encode (dropWhile (/=x) xs)

data EncodedElement a = Multiple Int a | Single a
    deriving (Show)