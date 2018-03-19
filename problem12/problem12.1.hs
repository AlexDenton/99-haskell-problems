main = do
    putStrLn $ show $ decode [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']

decode :: [EncodedElement a] -> [a]
decode [] = []
decode (x:xs) = decodeElement x ++ decode xs
    where
        decodeElement (Single x) = [x]
        decodeElement (Multiple n x) = replicate n x

data EncodedElement a = Multiple Int a | Single a