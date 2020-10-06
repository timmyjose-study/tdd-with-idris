module EqNat

%default total

data EqNat : (num1 : Nat) -> (num2 : Nat) -> Type where
  Same : (num : Nat) -> EqNat num num

sameS : (j : Nat) -> (k : Nat) -> EqNat j k -> EqNat (S j) (S k)
sameS j j (Same j) = Same (S j)

checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (EqNat num1 num2)
checkEqNat Z Z = Just (Same Z)
checkEqNat Z _ = Nothing
checkEqNat _ Z = Nothing
checkEqNat (S j) (S k) = case checkEqNat j k of
                              Nothing => Nothing
                              Just eq => Just (sameS j k eq)
