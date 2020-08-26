module Palindrome

import Data.List
import Data.Strings
import System.REPL

checkPalindrome : String -> String
checkPalindrome str = if isPalindrome then "Yes, this is a palindrome\n" else "No, this is not a palindrome\n"
  where
    isPalindrome : Bool
    isPalindrome = toLower str == toLower (reverse str)

main : IO ()
main = repl "Enter a string: " checkPalindrome
