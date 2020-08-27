module WordLength

import Data.List
import Data.Strings

%default total

allLengths : List String -> List Nat
allLengths [] = []
allLengths (x :: xs) = length x :: allLengths xs
