module Chapter3

import Data.Strings
import Data.Vect

%default total

describeList : Show a => List a -> String
describeList [] = "Empty"
describeList (_ :: xs) = "Non-empty, tail = " ++ show xs

allLengths : List String -> List Nat
allLengths [] = []
allLengths (x :: xs) = length x :: allLengths xs

xor : Bool -> Bool -> Bool
xor False y = y
xor True y = not y

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

-- working with vectors

fourInts : Vect 4 Int
fourInts = [11, 12, 13, 14]

sixInts : Vect 6 Int
sixInts = [1, 2, 3, 4, 5, 6]

tenInts : Vect 10 Int
tenInts = fourInts ++ sixInts

myZip : Vect n a -> Vect n b -> Vect n (a, b)
myZip [] [] = []
myZip (x :: xs) (y :: ys) = (x, y) :: myZip xs ys

myZipWith : (a -> b -> c) -> Vect n a -> Vect n b -> Vect n c
myZipWith f [] [] = []
myZipWith f (x :: xs) (y :: ys) = f x y :: myZipWith f xs ys
