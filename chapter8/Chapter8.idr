module Chapter8

import Data.Strings

%default total

-- guaranteeing equivalence of data with equality types

{-
  exactLength : (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
-}

data Vect : (len : Nat) -> (elem : Type) -> Type where
  Nil : Vect Z elem
  (::) : (x : elem) -> Vect len elem -> Vect (S len) elem

{-
exactLength : { m : Nat } -> (len : Nat) -> Vect m a -> Maybe (Vect len a)
exactLength len input = case len == m of
                               False => Nothing
                               True => Just input
-}

-- expressing equality of Nats as a type

data EqNat : (num1 : Nat) -> (num2 : Nat) -> Type where
  Same : (n : Nat) -> EqNat n n 

sameS : (j : Nat) -> (k : Nat) -> EqNat j k -> EqNat (S j) (S k)
sameS j j (Same j) = Same (S j)

checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (EqNat num1 num2)
checkEqNat Z Z = Just (Same Z)
checkEqNat Z _ = Nothing
checkEqNat _ Z = Nothing
checkEqNat (S j) (S k) = case checkEqNat j k of
                              Nothing => Nothing
                              Just eq => Just (sameS j k eq)

exactLength : { m : Nat } -> (len : Nat) -> Vect m a -> Maybe (Vect len a)
exactLength len input = case checkEqNat len m of
                             Nothing => Nothing
                             Just (Same len) => Just input

{-
  Idris provides a built-in type for equality, that is generic, and conceptually defined as:

  data (=) : a -> b -> Type where
    Refl : x = x

  indicating reflexivity as the mode of equality.

  This means that we don't have to define data types and functions for every type that we wish to perform equality checks on, as in the
  case of NatS above.



-}
