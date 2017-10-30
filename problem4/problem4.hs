import Test.HUnit
import Test.HUnit.Tools

main = do
    runTestTT lengthTests

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

length'' :: [a] -> Int
length'' xs = foldr (\_ -> (+1)) 0 xs

lengthTests = test [
    shouldReturnLength length',
    shouldReturnLength length'']

shouldReturnLength f = TestCase $ f [1,2,3,4,5] @?= 5