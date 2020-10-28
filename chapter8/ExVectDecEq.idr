module ExVectDecEq 

import Decidable.Equality

%default total

data Vect : Nat -> Type -> Type where
  Nil : Vect Z a
  (::) : a -> Vect k a -> Vect (S k) a

{-
  interface DecEq ty where
    decEq : (val1 : ty) -> (val2 : ty) -> Dec (val1 = val2)

  data Dec : (prop : Type) -> Type where
    Yes : (prf : prop) -> Dec prop
    No : (contra ; prop -> Void) -> Dec prop
-}

headUnequal : DecEq a => { xs: Vect n a } -> { ys : Vect n a } -> 
                       (contra : (x = y) -> Void) -> ((x :: xs) = (y :: ys)) -> Void
headUnequal contra Refl = contra Refl

tailUnequal : DecEq a => { xs : Vect n a } -> { ys : Vect n a } ->
                         (contra : (xs = ys) -> Void) -> ((x :: xs) = (y:: ys)) -> Void
tailUnequal contra Refl = contra Refl

DecEq a => DecEq (Vect n a) where
  decEq [] [] = Yes Refl
  decEq [] _ = No ?nilNotEqualToCons
  decEq _ [] = No ?consNotEqualToNil
  decEq (x :: xs) (y :: ys) = case decEq x y of
                                   No contra => No (headUnequal contra)
                                   Yes Refl => case decEq xs ys of
                                                    No contra => No (tailUnequal contra)
                                                    Yes Refl => Yes Refl
