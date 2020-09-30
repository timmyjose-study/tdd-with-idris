module GenericAdder

%default total

AdderType : (numargs : Nat) -> (elemType : Type) -> Type
AdderType Z ty = ty
AdderType (S k) ty = (next : ty) -> AdderType k ty

adder : Num elemType => (numargs : Nat) -> (acc : elemType) -> AdderType numargs elemType
adder Z acc = acc
adder (S k) acc = \next => adder k (acc + next)
