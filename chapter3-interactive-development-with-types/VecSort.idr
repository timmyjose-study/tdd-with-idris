module VecSort

import Data.Vect

%default total

insertionSort : Ord a => Vect n a -> Vect n a
insertionSort [] = []
insertionSort (x :: xs) = insert x (insertionSort xs)
  where
    insert : {0 n : _} -> a -> Vect n a -> Vect (S n) a
    insert x [] = [x]
    insert x (y :: ys) = case compare x y of
                              GT => y :: insert x ys
                              _ => x :: y :: ys
