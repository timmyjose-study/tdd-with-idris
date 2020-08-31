module Main

import Data.Strings
import System.REPL

checkPalindrome : (str : String) -> String
checkPalindrome str = if toLower str == toLower (reverse str) 
                         then "Palindrome!\n"
                         else "Not a palindrome\n"

main : IO ()
main = repl "Enter a string: " checkPalindrome
