module Ex3 

import Data.List
import Data.Strings
import Data.Vect

%default total

myLength : List a -> Nat
myLength [] = 0
myLength (_ :: xs) = 1 + myLength xs

myReverse : List a -> List a
myReverse [] = []
myReverse (x :: xs) = myReverse xs ++ [x]

mapl : (a -> b) -> List a -> List b
mapl f [] = []
mapl f (x :: xs) = f x :: map f xs

mapv : (a -> b) -> Vect n a -> Vect n b
mapv f [] = []
mapv f (x :: xs) = f x :: mapv f xs

transposeMat : {n : _} -> Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = replicate n []
transposeMat (x :: xs) = zipWith (::) x (transposeMat xs)

addMatrix : Num a => Vect m (Vect n a) -> Vect m (Vect n a) -> Vect m (Vect n a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = zipWith (+) x y :: addMatrix xs ys

subMatrix : (Num a, Neg a) => Vect m (Vect n a) -> Vect m (Vect n a) -> Vect m (Vect n a)
subMatrix [] [] = []
subMatrix (x :: xs) (y :: ys) = zipWith (-) x y :: subMatrix xs ys

multMatrix : Num a => Vect m (Vect n a) -> Vect n (Vect p a) -> Vect m (Vect p a)
multMatrix [] [] = ?foo
multMatrix (x :: xs) (y :: ys) = ?bar