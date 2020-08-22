module Chapter2

import Data.List
import Data.Strings

double :  Int -> Int
double x = x + x

cube : (x : Int) -> Int
cube x = x * x * x

add : (x : Int) -> (y : Int) -> Int
add x y = x + y

-- partially-applied functions
add10 : Int -> Int
add10 = \x => add x 10

-- generic types

genDouble : Num ty => ty -> ty
genDouble x = x + x

||| Calculate the number of words in the input
||| @str the input string
wordCount : (str : String) -> Nat
wordCount str = length (words str)
