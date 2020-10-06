module CheckEqMaybe

%default total

data EqNat : (nu1 : Nat) -> (num2 : Nat) -> Type where
  Same : (num : Nat) -> EqNat num num

sameS : EqNat j k -> EqNat (S j) (S k)
sameS (Same j) = Same (S j)

checkEqualNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (EqNat num1 num2)
checkEqualNat Z Z = Just (Same Z)
checkEqualNat Z _ = Nothing
checkEqualNat _ Z = Nothing
checkEqualNat (S j) (S k) = case checkEqualNat j k of
                                 Nothing => Nothing
                                 Just eq => Just (sameS eq)

checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (num1 = num2)
checkEqNat Z Z = Just Refl
checkEqNat Z _ = Nothing
checkEqNat _ Z = Nothing
checkEqNat (S j) (S k) = case checkEqNat j k of
                              Nothing => Nothing
                              Just prf => Just (cong S prf) -- `cons S` is essentially the same as sameS above, applying the `S` function
