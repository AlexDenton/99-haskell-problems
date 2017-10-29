import Test.HUnit
import Control.Exception
import Test.HUnit.Tools

main = do
    runTestTT lastTests

last' [] = errorWithoutStackTrace canNotCallLastOnEmptyList
last' (_:xs) = last xs

last'' :: [a] -> a
last'' [] = errorWithoutStackTrace canNotCallLastOnEmptyList
last'' xs = xs !! ((length xs) - 1)

canNotCallLastOnEmptyList = "Can not call 'last' on an empty list"

lastTests = TestList [
    shouldGetLastIntElement last',
    shouldGetLastIntElement last'',
    shouldGetLastStringElement last',
    shouldGetLastStringElement last'',
    shouldErrorWithEmptyList last',
    shouldErrorWithEmptyList last'']

shouldGetLastIntElement f = shouldGetLastElement f [1,2,3,4] 4

shouldGetLastStringElement f = shouldGetLastElement f ["first", "second", "third"] "third"

shouldGetLastElement f xs l = TestCase $ l @=? f xs

shouldErrorWithEmptyList f = TestCase $ assertRaises "Empty list raises an error" (ErrorCall canNotCallLastOnEmptyList) $ evaluate (f [] :: String)