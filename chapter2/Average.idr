module Average

import Data.List
import Data.Strings
import System.REPL

average : (str : String) -> Double
average str = let numWords = length (words str)
                  totalLength = sum (allLengths (words str)) in
                  cast totalLength / cast numWords
               where
                 allLengths : List String -> List Nat
                 allLengths strs = map length strs

showAverage : (str : String) -> String
showAverage str = "The average word length is " ++ show (average str) ++ "\n"

main : IO ()
main = repl "Enter a string: " showAverage