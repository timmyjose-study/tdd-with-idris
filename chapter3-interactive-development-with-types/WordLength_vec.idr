module WordLength_vec

import Data.Strings
import Data.Vect

%default total

allLengths : Vect n String -> Vect n Nat
allLengths [] = []
allLengths (x :: xs) = length x :: allLengths xs
