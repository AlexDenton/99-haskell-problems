import Test.HUnit
import Control.Exception
import Test.HUnit.Tools

main = do
    runTestTT shouldGetLastElement
    runTestTT shouldErrorWithEmptyList

last' [] = errorWithoutStackTrace canNotCallLastOnEmptyList
last' (x:xs) = last xs

last'' :: [a] -> a
last'' [] = error canNotCallLastOnEmptyList
last'' xs = xs !! ((length xs) - 1)

canNotCallLastOnEmptyList = "Can not call 'last' on an empty list"

shouldGetLastElement = TestCase (assertEqual "List of integers" 4 (last' [1,2,3,4]))

shouldErrorWithEmptyList = TestCase $ assertRaises "Empty list raises an error" (ErrorCall canNotCallLastOnEmptyList) $ evaluate (last' [] :: String)