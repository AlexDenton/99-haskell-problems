import Test.HUnit
import Control.Exception
import Test.HUnit.Tools

main = do
    runTestTT secondToLastTests

secondToLast :: [a] -> a
secondToLast [] = errorWithoutStackTrace canNotCallSecondToLastOnEmptyList
secondToLast [x] = errorWithoutStackTrace canNotCallSecondToLastOnOneElementList
secondToLast [x,_] = x
secondToLast (x:xs) = secondToLast xs

canNotCallSecondToLastOnEmptyList = "Can not call 'secondToLast' on empty list"
canNotCallSecondToLastOnOneElementList = "Can not call 'secondToLast' on list with one element"

secondToLastTests = test [
    shouldReturnSecondToLastElement,
    shouldReturnErrorWithEmptyList,
    shouldReturnErrorWithOneElementList]

shouldReturnSecondToLastElement = TestCase $ secondToLast [1,2,3,4] @?= 3

shouldReturnErrorWithEmptyList = TestCase $ assertRaises "Empty list raises an error" 
    (ErrorCall canNotCallSecondToLastOnEmptyList) $ evaluate (secondToLast [] :: String)

shouldReturnErrorWithOneElementList = TestCase $ assertRaises "One element list raises an error" 
    (ErrorCall canNotCallSecondToLastOnOneElementList) $ evaluate (secondToLast [1])