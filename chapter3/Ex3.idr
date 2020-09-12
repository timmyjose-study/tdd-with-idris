module Ex3

import Data.Vect

%default total

myLength : List a -> Nat
myLength [] = 0
myLength (_ :: xs) = 1 + myLength xs

myReverse : List a -> List a
myReverse [] = []
myReverse (x :: xs) = myReverse xs ++ [x]

myMap : (a -> b) -> List a -> List b
myMap _ [] = []
myMap f (x :: xs) = f x :: myMap f xs

myVectMap : (a -> b) -> Vect n a -> Vect n b
myVectMap _ [] = []
myVectMap f (x :: xs) = f x :: myVectMap f xs

transposeMatrix : { n : _ } -> Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMatrix [] = replicate n []
transposeMatrix (x :: xs) = let xsTrans = transposeMatrix xs in
                                zipWith (::) x xsTrans

addMatrix : Num elem => Vect m (Vect n elem) -> Vect m (Vect n elem) -> Vect m (Vect n elem)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = zipWith (+) x y :: addMatrix xs ys

subMatrix : (Neg elem, Num elem) => Vect m (Vect n elem) -> Vect m (Vect n elem) -> Vect m (Vect n elem)
subMatrix [] [] = []
subMatrix (x :: xs) (y :: ys) = zipWith (-) x y :: subMatrix xs ys

multMatrix : Num elem => { m, n, o : _ } -> Vect m (Vect n elem) -> Vect n (Vect o elem) -> Vect m (Vect o elem)
multMatrix xs ys = let ysTrans = transposeMatrix ys in
                       aux xs ysTrans

  where
    dotProduct : { n : _ } -> Vect n elem -> Vect n elem -> elem
    dotProduct [] [] = 0
    dotProduct (x :: xs) (y :: ys) = x * y + dotProduct xs ys

    aux :  {m, n, o : _} -> Vect m (Vect n elem) -> Vect o (Vect n elem) -> Vect m (Vect o elem)
    aux [] _ = []
    aux (x :: xs) ys = map (\y => dotProduct x y) ys :: aux xs ys
