module Main

import System.REPL

main : IO ()
main = repl "Enter the string to be reversed: " reverse
