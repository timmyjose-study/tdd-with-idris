module Main

import System.REPL
import Data.Strings

checkPalindrome : (str : String) -> String
checkPalindrome str = if isPalin then "True\n" else "False\n"
  where
    isPalin : Bool
    isPalin = toLower str == reverse (toLower str)

main : IO ()
main = repl "Enter a string: " checkPalindrome