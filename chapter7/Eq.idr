module Eq 

import Data.List

%default total

occurrences : Eq ty => ty -> List ty -> Nat
occurrences _ [] = 0
occurrences v (x :: xs) = case v == x of
                               False => occurrences v xs
                               True => 1 + occurrences v xs

data Matter = Solid | Liquid | Gas

{-
  interface Eq ty where
    (==) :: ty -> ty -> Bool
    (/=) :: ty -> ty -> Bool

    (==) x y = not (x /= y)
    (/=) x y = not (x == y)

-}

Eq Matter where
  (==) Solid Solid = True
  (==) Liquid Liquid = True
  (==) Gas Gas = True
  (==) _ _ = False

data Tree elem = Empty | Node (Tree elem) elem (Tree elem)

Eq ty => Eq (Tree ty) where
  (==) Empty Empty = True
  (==) (Node l v r) (Node l' v' r') = l == l' && v == v' && r == r'
  (==) _ _  = False

partial
fromList : List ty -> Tree ty
fromList [] = Empty
fromList [x] = Node Empty x Empty
fromList (x :: xs) = Node (fromList left) x (fromList right)
         where
           len : Nat
           len = integerToNat $ (natToInteger (length xs)) `div` 2

           left : List ty
           left = take len xs

           right : List ty
           right = drop len xs
