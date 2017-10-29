import Test.HUnit
import Control.Exception
import Test.HUnit.Tools

main = do
    runTestTT lastTests

last' [] = errorWithoutStackTrace canNotCallLastOnEmptyList
last' (x:xs) = last xs

last'' :: [a] -> a
last'' [] = errorWithoutStackTrace canNotCallLastOnEmptyList
last'' xs = xs !! ((length xs) - 1)

canNotCallLastOnEmptyList = "Can not call 'last' on an empty list"

lastTests = TestList [shouldGetLastElement, shouldErrorWithEmptyList]

shouldGetLastElement = TestCase (assertEqual "List of integers" 4 (last' [1,2,3,4]))

shouldErrorWithEmptyList = TestCase $ assertRaises "Empty list raises an error" (ErrorCall canNotCallLastOnEmptyList) $ evaluate (last' [] :: String)