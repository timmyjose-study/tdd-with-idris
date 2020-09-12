module Reverse

import Data.Strings
import System.REPL

main : IO ()
main = repl "Enter a string: " reverse