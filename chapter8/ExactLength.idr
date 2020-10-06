module ExactLength 

%default total

data Vect : (len : Nat) -> (elem : Type) -> Type where
  Nil : Vect Z elem
  (::) : (x : elem) -> (xs : Vect n elem) -> Vect (S n) elem

{-
exactLength : { m : _ } -> (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
exactLength len input = case m == len of
                                 False => ?foo
                                 True => Just input
-}

data EqNat : (num1 : Nat) -> (num2 : Nat) -> Type where
  Same : (num : Nat) -> EqNat num num

checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (EqNat num1 num2)
checkEqNat Z Z = Just (Same Z)
checkEqNat Z _ = Nothing
checkEqNat _ Z = Nothing
checkEqNat (S j) (S k) = case checkEqNat j k of
                              Nothing => Nothing
                              Just (Same l) => Just (Same (S l))
                              
exactLength : { m : _ } -> (len : Nat) -> Vect m a -> Maybe (Vect len a)
exactLength len input = case checkEqNat len m of
                             Nothing => Nothing
                             Just (Same len) => Just input

