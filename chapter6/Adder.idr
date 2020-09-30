module Adder

%default total

||| Calculate the type of an adder function 
||| that expected `numargs` arguments (in addition
||| to the initial value argument)
AdderType : (numargs : Nat) -> Type
AdderType Z = Int
AdderType (S k) = (next : Int) -> AdderType k

adder : (numargs : Nat) -> (acc : Int) -> AdderType numargs
adder Z acc = acc
adder (S k) acc = \next => adder k (acc + next)
