module Vectors

import Data.Vect

%default total

fourInts : Vect 4 Int
fourInts = [1, 2, 3, 4]

sixInts : Vect 6 Int
sixInts = [5, 6, 7, 8, 9, 10]

tenInts : Vect 10 Int
tenInts = fourInts ++ sixInts

allLengths : Vect len String -> Vect len Nat
allLengths [] = []
allLengths (x :: xs) = length x :: allLengths xs
