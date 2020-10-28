module ExactLength 

%default total

data Vect : Nat -> Type -> Type where
  Nil : Vect Z a
  (::) : a -> Vect k a -> Vect (S k) a

{-
exactLength :  { m : _ } -> (len : Nat) -> Vect m a -> Maybe (Vect len a)
exactLength len input = case m == len of
                             False => Nothing
                             True => Just input
-}

-- build-up: equality of Nats (EqNat.idr)

data EqNat : Nat -> Nat -> Type where
  Same : (num : Nat) -> EqNat num num

sameS : (eq : EqNat k j) -> EqNat (S k) (S j)
sameS (Same l) = Same (S l)

checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (EqNat num1 num2)
checkEqNat Z Z = Just (Same Z)
checkEqNat Z _  = Nothing
checkEqNat _ Z = Nothing
checkEqNat (S k) (S l) = case checkEqNat k l of
                              Nothing => Nothing
                              Just eq => Just (sameS eq)

exactLength : { m : Nat } -> (len : Nat) -> Vect m a -> Maybe (Vect len a)
exactLength len input = case checkEqNat m len of
                             Nothing => Nothing
                             Just (Same len) => Just input

-- implementation of checkEqNat using the generic equality type

{-
  data (=) : a -> b -> Type where
    Refl : x = x

  Synonym: Equal
-}

checkEqNat' : (num1 : Nat) -> (num2 : Nat) -> Maybe (num1 = num2)
checkEqNat' Z Z = Just Refl
checkEqNat' Z _  = Nothing
checkEqNat' _ Z = Nothing
checkEqNat' (S k) (S l) = case checkEqNat' k l of
                               Nothing => Nothing
                               Just eq => Just (cong S eq)

exactLength' :  { m : Nat } -> (len : Nat) -> Vect m a -> Maybe (Vect len a)
exactLength' len input = case checkEqNat' m len of
                              Nothing => Nothing
                              Just Refl => Just input
