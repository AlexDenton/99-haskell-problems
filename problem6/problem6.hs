main = do
    print $ isPalindrome "kayak"
    print $ isPalindrome "canoe"

isPalindrome xs = xs == reverse xs