module Counts

import Data.Strings
import System.REPL

showCounts : String -> String
showCounts str = show (length (words str), length str) ++ "\n"

main : IO ()
main = repl "Enter a string: " showCounts