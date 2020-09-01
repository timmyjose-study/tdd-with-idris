module Implicits

import Data.Vect

%default total

-- using implicits n, m and elem
-- note however that such implicits have multiplicity 0, and 
-- therefore need to be brought explicitly into use when referring
-- to them in the body of the function (as in the matrix multiplication example)

append : Vect n elem -> Vect m elem -> Vect (n + m) elem
append [] ys = ys
append (x :: xs) ys = x :: append xs ys

-- everything explicit here - the horrors!
-- usage also gets complicated like so: 
--- append' _ _ _ [1, 2, 3] [4, 5, 6, 7, 8]

append' : (elem : Type) -> (n, m : Nat) -> Vect n elem -> Vect m elem -> Vect (n + m) elem
append' elem Z m [] ys = ys
append' elem (S k) m (x :: xs) ys = x :: append' elem k m xs ys

-- bound implicits (contrast with the explicits as shown above)

append'' : { elem : Type } -> { n, m : Nat } -> Vect n elem -> Vect m elem -> Vect (n + m) elem
append'' [] ys = ys
append'' (x :: xs) ys = x :: append'' xs ys

-- In Idris2, unbound implicits have multiplicity 0, and therefore have to be brought into view by making them
-- bound implicits explicitly

myLength : { n : _ } -> Vect n elem -> Nat
myLength {n} xs = n -- this notation allows us to refer to the implicits directly in the body of the function