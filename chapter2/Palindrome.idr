module Palindrome

import Data.Strings
import System.REPL

%default total

palindrome : String -> Bool
palindrome s = toLower s == toLower (reverse s)

checkPalindrome : (str : String) -> String
checkPalindrome str = if palindrome str
                         then "Yes" ++ "\n"
                         else "No" ++ "\n"

partial
main : IO ()
main = repl "Enter a string: " checkPalindrome