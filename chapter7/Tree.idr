module Tree

import Data.List

%default total

{-
  interface Functor (f : Type -> Type) where
    map : (func : a -> b) -> f a -> f b

  Functor List where
    map func [] = []
    map func (x :: xs) = f x :: map func xs

-}

interface MyFunctor f where
  myMap : (a -> b) -> f a -> f b

MyFunctor List where
  myMap func [] = []
  myMap func (x :: xs) = func x :: myMap func xs

data Tree elem = Empty 
               | Node (Tree elem) elem (Tree elem)

Functor Tree where
  map func Empty = Empty
  map func (Node l v r) = Node (map func l) (func v) (map func r)

partial
fromList : List a -> Tree a
fromList [] = Empty
fromList (x :: xs) = Node (fromList left) x (fromList right)
  where
    len : Nat
    len = fromInteger $ natToInteger (length xs) `div` 2

    left : List a
    left = take len xs

    right : List a 
    right = drop len xs

{-
  Functor (Vect n) where 
    map func [] = []
    map func (x :: xs) = func x :: map func xs

-}

{-
  foldr : Foldable t => (a -> b -> b) -> b -> t a -> b
  foldl : Foldable t => (b -> a -> b) -> b -> t a -> b

-}

sampleTree : Tree Int
sampleTree = Node (Node (Node Empty 1 Empty) 2 (Node Empty 3 Empty)) 4 (Node (Node Empty 5 Empty) 6 (Node Empty 7 Empty))

Foldable Tree where
  foldr func acc Empty = acc
  foldr func acc (Node l v r) = let rightFold = foldr func acc r
                                    leftFold = foldr func rightFold l in
                                    func v leftFold