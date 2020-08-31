module Generic

double : Num ty => ty -> ty
double x = x + x

twice : (a -> a) -> a -> a
twice f x = f (f x)

quadruple : Num ty => ty -> ty
quadruple = twice double

Shape : Type
rotate : Shape -> Shape

turnAround : Shape -> Shape
turnAround = twice rotate
