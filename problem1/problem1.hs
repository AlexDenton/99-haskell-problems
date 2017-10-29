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

lastTests = TestList [
    shouldGetLastIntElement last',
    shouldGetLastIntElement last'',
    shouldErrorWithEmptyList]

shouldGetLastIntElement :: ([Int] -> Int) -> Test
shouldGetLastIntElement f = TestCase $ 4 @=? f [1,2,3,4]

shouldErrorWithEmptyList = TestCase $ assertRaises "Empty list raises an error" (ErrorCall canNotCallLastOnEmptyList) $ evaluate (last' [] :: String)