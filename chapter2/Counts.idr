module Main

import Data.List
import Data.Strings
import System.REPL

showCounts : String -> String
showCounts str = "Number of words = " ++ show wordCount ++ ", and number of characters = " ++ show numChars ++ "\n"
  where
    wordCount : Nat
    wordCount = length (words str)

    numChars : Nat
    numChars = length str

main : IO ()
main = repl "Enter a string: " showCounts