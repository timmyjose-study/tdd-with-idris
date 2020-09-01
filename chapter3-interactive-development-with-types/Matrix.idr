module Matrix

import Data.Vect

%default total

transposeMat : { n : Nat } -> Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = replicate n []
transposeMat (x :: xs) = let xsTrans = transposeMat xs in
                             transposeHelper x xsTrans
                             where
                               transposeHelper : { n : Nat } -> Vect n elem -> Vect n (Vect k elem) -> Vect n (Vect (S k) elem)
                               transposeHelper [] [] = []
                               transposeHelper (x :: xs) (y :: ys) = (x :: y) :: transposeHelper xs ys

mat1 : Vect 2 (Vect 3 Int)
mat1 = [[1, 2, 3], [4, 5, 6]]