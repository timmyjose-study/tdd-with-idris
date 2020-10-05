module Num

%default total

{-
  interface Num ty where
    (+) : ty -> ty -> ty
    (*) : ty -> ty -> ty
    fromInteger : Integer -> ty

  interface Num ty => Neg ty where
    negate : ty -> ty
    (-) : ty -> ty -> ty
    abs : ty -> ty

  interface Num ty => Integral ty  where
    div  : ty -> ty -> ty
    mod : ty -> ty -> ty

  interface Num ty => Fractional ty where
    (/) : ty -> ty -> ty
    recip : ty -> ty

    recip x = 1 / x
-}