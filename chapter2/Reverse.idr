module Main

import System.REPL

main : IO ()
main = repl "> " (\s => reverse s ++ "\n")