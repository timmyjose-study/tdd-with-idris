module ReverseVec

import Data.Nat
import Data.Vect

%default total

myReverse : Vect n elem -> Vect n elem
myReverse [] = []
myReverse { n = S k } (x :: xs) = let rev = myReverse xs ++ [x] in
                          rewrite plusCommutative 1 k in rev

ourReverse : Vect n elem -> Vect n elem
ourReverse [] = []
ourReverse (x :: xs) = reverseProof (ourReverse xs ++ [x]) where
  reverseProof : Vect (len + 1) elem -> Vect (S len) elem
  reverseProof { len } xs = rewrite plusCommutative 1 len in xs
