module Main

import System.REPL
import Data.List
import Data.Strings

showAverageWordLength : (str : String) -> String
showAverageWordLength str = "The average word length is " ++ show wordLen ++ "\n"
  where
    totalLen : Nat
    totalLen = sum . map length $ (words str)

    numWords : Nat
    numWords = length (words str)

    wordLen : Double
    wordLen = cast totalLen / cast numWords

main : IO ()
main = repl "Enter a string: " showAverageWordLength