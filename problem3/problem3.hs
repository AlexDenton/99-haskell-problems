import Test.HUnit
import Control.Exception
import Test.HUnit.Tools

main = do
    runTestTT elementAtTests

elementAt :: [a] -> Int -> a
elementAt [] _ = errorWithoutStackTrace indexOutOfRange
elementAt (x:_) 1 = x
elementAt (_:xs) k
    | k < 1 = errorWithoutStackTrace indexOutOfRange
    | otherwise = elementAt xs (k-1)

indexOutOfRange = "Index out of range"

elementAtTests = test [
    shouldGetElementAt,
    shouldErrorWithEmptyList,
    shouldErrorWithIndexLargerThanList,
    shouldErrorWithNegativeIndex]

shouldGetElementAt = TestCase $ elementAt [1,2,3] 2 @?= 2

shouldErrorWithEmptyList = TestCase $ assertRaises "Empty list raises error"
    (ErrorCall indexOutOfRange) $ evaluate $ elementAt ([] :: [Int]) 3

shouldErrorWithIndexLargerThanList = TestCase $ assertRaises "Index larger than list raises error"
    (ErrorCall indexOutOfRange) $ evaluate $ elementAt [1,2] 3

shouldErrorWithNegativeIndex = TestCase $ assertRaises "Negative index raises error"
    (ErrorCall indexOutOfRange) $ evaluate $ elementAt [1,2,3] (-2)