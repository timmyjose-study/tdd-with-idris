module Ex6

import Data.Vect

%default total

Matrix : Nat -> Nat -> Type
Matrix n m = Vect n (Vect m Double)

testMatrix : Matrix 2 3
testMatrix = [[0, 0, 0], [1, 2, 3]]

data Format = Integ Format
            | Dbl Format
            | Chr Format
            | Str Format
            | Lit String Format
            | End

PrintfType : (fmt : Format) -> Type
PrintfType (Integ fmt) = (i : Int) -> PrintfType fmt
PrintfType (Dbl fmt) = (d : Double) -> PrintfType fmt
PrintfType (Chr fmt) = (c : Char) -> PrintfType fmt
PrintfType (Str fmt) = (s : String) -> PrintfType fmt
PrintfType (Lit str fmt) = PrintfType fmt
PrintfType End = String

printfFmt : Num ty => (fmt : Format) -> (acc : String) -> PrintfType fmt
printfFmt (Integ fmt) acc = \i => printfFmt fmt (acc ++ show i)
printfFmt (Dbl fmt) acc = \d => printfFmt fmt (acc ++ show d)
printfFmt (Chr fmt) acc = \c => printfFmt fmt (acc ++ show c)
printfFmt (Str fmt) acc = \s => printfFmt fmt (acc ++ s)
printfFmt (Lit str fmt) acc = printfFmt fmt (acc ++ str)
printfFmt End acc = acc

toFormat : List Char -> Format
toFormat [] = End
toFormat ('%' :: 'd' :: chars) = Integ (toFormat chars)
toFormat ('%' :: 'f' :: chars) = Dbl (toFormat chars)
toFormat ('%' :: 'c' :: chars) = Chr (toFormat chars)
toFormat ('%' :: 's' :: chars) = Str (toFormat chars)
toFormat ('%' :: chars) = Lit "%" (toFormat chars)
toFormat (c :: chars) = case toFormat chars of
                             Lit lit chars' => Lit (strCons c lit) chars'
                             fmt => Lit (strCons c "") fmt

printf : (inp : String) -> PrintfType (toFormat (unpack inp))
printf fmt = printfFmt (toFormat (unpack fmt)) ""

{-

  TupleVect 0 ty = ()
  TupleVect 1 ty = (ty, ())
  TupleVect 2 ty = (ty, (ty, ()))
  ...

-}

TupleVect : (nesting : Nat) -> (elemType : Type) -> Type
TupleVect Z ty = ()
TupleVect (S k) ty = (ty, TupleVect k ty)

test : TupleVect 5 Nat
test = (1, 2, 3, 4, 5, ())