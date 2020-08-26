module Main 

import Data.List
import Data.Strings
import System.REPL

showAverage : (str : String) -> String
showAverage str = "The average word length is: " ++ show wordLen ++ "\n"
  where
    totalLength : Nat
    totalLength = length str

    numWords : Nat
    numWords = length (words str)

    wordLen : Double
    wordLen = cast totalLength / cast numWords

main : IO ()
main = repl "Enter a string: " showAverage