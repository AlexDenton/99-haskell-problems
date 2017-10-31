import Test.HUnit
import Control.Applicative

main = do
    runTestTT isPalindromeTests

isPalindrome xs = xs == reverse xs

-- apply on functions creates a new function which takes a value
-- (in this case the list), calls the second function (in this case reverse) with the value
-- and then calls the first function with the same list. In effect, reverse the list and compare
-- to the original list. This is maybe not the most expressive but is very fun!
isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' = (==) <*> reverse 

isPalindromeTests = test [
    shouldReturnTrueWithPalindrome isPalindrome,
    shouldReturnFalseWithNonPalindrome isPalindrome,
    shouldReturnTrueWithPalindrome isPalindrome',
    shouldReturnFalseWithNonPalindrome isPalindrome']

shouldReturnTrueWithPalindrome f = TestCase $ f "kayak" @?= True
shouldReturnFalseWithNonPalindrome f = TestCase $ f "canoe" @?= False