module Main

import Data.Strings -- for words
import Data.List -- for length
import System.REPL -- for repl

average : (str : String) -> Double
average str = let numWords = length (words str)
                  totalLen = sum (map length $ words str)
              in
                cast totalLen / cast numWords

showAverage : String -> String
showAverage str = "The average word length is: " ++
                  show (average str) ++ "\n"

main : IO ()
main = repl "Enter a string: " showAverage