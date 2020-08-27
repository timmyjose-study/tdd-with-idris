module Chapter3

import Data.List
import Data.Strings
import Data.Vect

invert : Bool -> Bool
invert False = True
invert True = False

describeList : List String -> String
describeList [] = "Empty"
describeList (x :: xs) = "Non-empty, tail = " ++ show xs

xor : Bool -> Bool -> Bool
xor False y = y
xor True y = not y

--isEven : Nat -> Bool
--isEven Z = True
--isEven (S k) = not (isEven k)

mutual
  isEven : Nat -> Bool
  isEven Z = True
  isEven (S k) = isOdd k

  isOdd : Nat -> Bool
  isOdd Z = False
  isOdd (S k) = isEven k

mutual
  evens : List a -> List a
  evens [] = []
  evens (x :: xs) = x :: odds xs

  odds : List a -> List a
  odds [] = []
  odds (_ :: xs) = evens xs

myZipWith : (a -> b -> c) -> Vect n a -> Vect n b -> Vect n c
myZipWith f [] [] = []
myZipWith f (x :: xs) (y :: ys) = f x y :: myZipWith f xs ys
