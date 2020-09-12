module InsSort

import Data.Vect

%default total

insert : Ord a => a -> Vect n a -> Vect (S n) a
insert x [] = [x]
insert x (y :: ys) = case compare x y of
                          GT => y :: insert x ys
                          _ => x  :: y :: ys

isort : Ord a => Vect n a -> Vect n a
isort [] = []
isort (x :: xs) = insert x (isort xs)