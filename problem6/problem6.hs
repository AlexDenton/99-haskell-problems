import Test.HUnit

main = do
    runTestTT isPalindromeTests

isPalindrome xs = xs == reverse xs

isPalindromeTests = test [
    shouldReturnTrueWithPalindrome,
    shouldReturnFalseWithNonPalindrome]

shouldReturnTrueWithPalindrome = TestCase $ isPalindrome "kayak" @?= True
shouldReturnFalseWithNonPalindrome = TestCase $ isPalindrome "canoe" @?= False