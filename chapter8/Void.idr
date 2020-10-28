module Void

%default total

{-
  data Void : Type where

-}

twoPlusTwoNotFive : 2 + 2 = 5 -> Void
twoPlusTwoNotFive Refl impossible

valueNotSucc : (x : Nat) -> x = S x -> Void
valueNotSucc _ Refl impossible

{-
  data Dec : (prop : Type) -> Type where
    Yes : (prf : prop) -> Dec prop
    No : (contra : prop -> Void) -> Dec prop
-}

-- more precise implementation of checkEqNat

zeroNotSucc : Z = S k -> Void
zeroNotSucc Refl impossible

succNotZero : S k = Z -> Void
succNotZero Refl impossible

noRec : (k = j -> Void) -> S k = S j -> Void
noRec contra Refl = contra Refl

checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Dec (num1 = num2)
checkEqNat Z Z = Yes Refl
checkEqNat Z (S k) = No zeroNotSucc
checkEqNat (S k) Z = No succNotZero
checkEqNat (S k) (S j) = case checkEqNat k j of
                              Yes prf => Yes (cong S prf)
                              No contra => No (noRec contra)
