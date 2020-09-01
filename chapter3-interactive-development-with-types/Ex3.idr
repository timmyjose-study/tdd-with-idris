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
myMap f [] = []
myMap f (x :: xs) = f x :: myMap f xs

myMapV : (a -> b) -> Vect n a -> Vect n b
myMapV f [] = []
myMapV f (x :: xs) = f x :: myMapV f xs

transposeMat : { n: Nat } -> Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = replicate n []
transposeMat (x :: xs) = let xsTrans = transposeMat xs in
                             zipWith (::) x xsTrans

addMatrix : Num elem => { n : Nat } -> Vect m (Vect n elem) -> Vect m (Vect n elem) -> Vect m (Vect n elem)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = zipWith (+) x y :: addMatrix xs ys

subMatrix : (Neg elem, Num elem) => { n : Nat } -> Vect m (Vect n elem) -> Vect m (Vect n elem) -> Vect m (Vect n elem)
subMatrix [] [] = []
subMatrix (x :: xs) (y :: ys) = zipWith (-) x y :: subMatrix xs ys

multMatrix : Num elem => { m, n, o : _ } -> Vect m (Vect n elem) -> Vect n (Vect o elem) -> Vect m (Vect o elem)
multMatrix xs ys = mult xs (transposeMat ys)
  where
    dotProduct : { n : _ } -> Vect n elem -> Vect n elem -> elem
    dotProduct [] [] = 0
    dotProduct (x :: xs) (y :: ys) = x * y + dotProduct xs ys

    multHelper : { n, o: _ } -> Vect n elem -> Vect o (Vect n elem) -> Vect o elem
    multHelper _ [] =  []
    multHelper x (y :: ys) = dotProduct x y :: multHelper x ys

    mult : { m, n, o : _ } -> Vect m (Vect n elem) -> Vect o (Vect n elem) -> Vect m (Vect o elem)
    mult [] _ = []
    mult (x :: xs) ys = multHelper x ys :: mult xs ys

mat22 : Vect 2 (Vect 2 Int)
mat22 = [[1, 2], [3, 4]]

mat23 : Vect 2 (Vect 3 Int)
mat23 = [[1, 2, 3], [4, 5, 6]]
