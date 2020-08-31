module Chapter2 

import Data.Strings

%default total

-- partial evaluation example

add : Int -> Int -> Int
add x y = x + y

add10 : Int -> Int
add10 = \x => add x 10

longer : String -> String -> Nat
longer s1 s2 = let s1len = length s1
                   s2len = length s2 in
                   if s1len >= s2len then s1len else s2len

pythagoras : Double -> Double -> Double
pythagoras x y = sqrt (square x + square y)
  where
    square : Double -> Double
    square x = x * x

intAndString : (Integer, String)
intAndString = (94, "Ninety-four")

intList : List Int
intList = [1, 2, 3, 4, 5]
