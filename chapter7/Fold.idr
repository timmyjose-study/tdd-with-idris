module Fold

import Data.Strings

%default total

totalLen : List String -> Nat
totalLen = foldr (\str, acc => length str + acc) 0

{-
  interface Foldable (t : Type -> Type) where
    foldr : (a -> b -> b) -> b -> t a -> b
    foldl : (b -> a -> b) -> b -> t a -> b

  Foldable List where
    foldr func acc [] = acc
    foldr func acc (x :: xs) = func x (foldr func acc xs)

    foldl func acc [] = acc
    foldl func acc (x :: xs) = foldl func (func acc x) xs

-}


