module Main

import System.REPL
import Data.Strings -- words
import Data.List -- length

showCounts : (str : String) -> String
showCounts str = show (wordCount, charCount) ++ "\n"
  where
      wordCount : Nat
      wordCount = length (words str)

      charCount : Nat
      charCount = length str

main : IO ()
main = repl "Enter a string: " showCounts
