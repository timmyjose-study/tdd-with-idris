module Matrix

import Data.Vect

%default total

transposeHelper : Vect n elem -> Vect n (Vect len elem) -> Vect n (Vect (S len) elem)
transposeHelper [] [] = []
transposeHelper (x :: xs) (y :: ys) = (x :: y) :: transposeHelper xs ys

transposeMatrix : { n, m : _ } -> Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMatrix [] = replicate n []
transposeMatrix (x :: xs) = let xsTrans = transposeMatrix xs in
                                transposeHelper x xsTrans