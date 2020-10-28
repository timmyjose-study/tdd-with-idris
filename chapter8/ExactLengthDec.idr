module ExactLengthDec

import Data.Vect
import Decidable.Equality 

%default total

{-
  interface DecEq ty where
    decEq : (val1 : ty) -> (val2 : ty) -> Dec (val1 = val2)

  data Dec: (prop : Type) -> Type where
    Yes : (prf : prop) -> Dec prop
    No : (contra : prop -> Void) -> Dec prop
-}

exactLength : { m : _ } -> (len : Nat) -> Vect m a -> Maybe (Vect len a)
exactLength len input = case decEq m len of
                          Yes Refl => Just input
                          No contra => Nothing
