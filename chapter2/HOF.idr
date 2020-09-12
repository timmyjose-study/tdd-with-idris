module HOF

%default total

twice : (a -> a) -> a -> a
twice f x = f (f x)

Shape : Type
rotate : Shape -> Shape

double : Num ty => ty -> ty
double x = x + x

quadruple : Num ty => ty -> ty
quadruple = twice double

turnAround : Shape -> Shape
turnAround = twice rotate
