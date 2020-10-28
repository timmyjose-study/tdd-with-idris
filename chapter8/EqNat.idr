module EqNat

%default total

data EqNat : (num1 : Nat) -> (num2 : Nat) -> Type where
  Same : (num : Nat) -> EqNat num num

sameS : (eq : EqNat k j) -> EqNat (S k) (S j)
sameS (Same k) = Same (S k)

checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (EqNat num1 num2)
checkEqNat Z Z = Just (Same Z)
checkEqNat Z _  = Nothing
checkEqNat _ Z = Nothing
checkEqNat (S k) (S l) = case checkEqNat k l of
                              Nothing => Nothing
                              Just eq => Just (sameS eq)
