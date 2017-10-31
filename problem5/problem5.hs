import Test.HUnit

main = do
    runTestTT reverseTests

reverse' :: [a] -> [a]
reverse' l = reverse'' l []
    where 
        reverse'' [] reversed = reversed
        reverse'' (x:xs) reversed = reverse'' xs (x : reversed)

reverse''' :: [a] -> [a]
reverse''' = foldl (flip (:)) []

reverseTests = test [
    shouldReverseAList reverse',
    shouldReverseAList reverse''']

shouldReverseAList f = TestCase $ f [1,2,3,4] @?= [4,3,2,1]