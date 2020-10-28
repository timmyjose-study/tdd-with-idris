module Ex8

%default total

same_cons : { xs : List a } -> { ys : List a } -> xs = ys -> x :: xs = x :: ys
same_cons Refl = Refl

same_lists : { xs : List a } -> { ys : List a } -> x = y -> xs = ys -> x :: xs = y :: ys
same_lists Refl Refl = Refl

data ThreeEq : a -> b -> c -> Type where
  AllSame : ThreeEq x x x

allSameS : (x, y, z : Nat) -> ThreeEq x y z -> ThreeEq (S x) (S y) (S z)
allSameS _ _ _ AllSame = AllSame 
