module LetWhere

import Data.List

longer : String -> String -> Nat
longer s1 s2 = let l1 = length s1 
                   l2 = length s2 
               in
                if l1 >= l2 then l1 else l2

pythagoras : Double -> Double -> Double
pythagoras x y = sqrt (square x + square y)
  where
    square : Double -> Double
    square x = x * x