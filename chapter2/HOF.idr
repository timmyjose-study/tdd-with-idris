module HOF

twice : (a -> a) -> a -> a
twice = \f => \x => f (f x)

Shape : Type

rotate : Shape -> Shape

turnAround : Shape -> Shape
turnAround = twice rotate

double : Num a => a -> a
double = \x => x + x

quadruple : Num a =>  a -> a
quadruple = \x => double (double x)