module Ex8

import Decidable.Equality
import Data.Nat
import Data.Vect

%default total

same_cons : { xs : List a } -> { ys : List a } -> xs = ys -> x :: xs = x :: ys
same_cons Refl = Refl

same_lists : { xs : List a } -> { ys : List a } -> x = y -> xs = ys -> x :: xs = y :: ys
same_lists Refl Refl = Refl

data ThreeEq : a -> b -> c -> Type where
  AllSame : ThreeEq x x x

allSameS : (x, y, z : Nat) -> ThreeEq x y z -> ThreeEq (S x) (S y) (S z)
allSameS _ _ _ AllSame = AllSame 

myPlusCommutes : (n : Nat) -> (m : Nat) -> n + m =  m + n
myPlusCommutes Z m = rewrite plusZeroRightNeutral m in Refl
myPlusCommutes (S k) m = rewrite myPlusCommutes k m in
                                 rewrite plusSuccRightSucc m k in Refl

reverseProof_nil : Vect n a -> Vect (plus n 0) a
reverseProof_nil {n} xs = rewrite plusZeroRightNeutral n in xs

reverseProof_xs : Vect (S n + k) a -> Vect (plus n (S k)) a
reverseProof_xs {n} {k} xs = rewrite sym (plusSuccRightSucc n k) in xs

myReverse : Vect n a -> Vect n a
myReverse xs = reverse' [] xs where
  reverse' : forall n, m. Vect n a -> Vect m a -> Vect (n + m) a
  reverse' acc [] = reverseProof_nil acc
  reverse' acc (x :: xs) = reverseProof_xs (reverse' (x :: acc) xs)

headUnequal : DecEq a => { xs : Vect n a } ->  { ys : Vect n a } -> 
                         (contra : (x = y) -> Void) -> ((x :: xs) = (y :: ys)) -> Void
headUnequal contra Refl = contra Refl

tailUnequal : DecEq a => { xs : Vect n a } -> { ys : Vect n a } -> 
                         (contra : (xs = ys) -> Void) -> ((x :: xs) = (y :: ys)) -> Void
tailUnequal contra Refl = contra Refl
