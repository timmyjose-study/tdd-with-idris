module Ex7

import Data.List

%default total

data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

area : Shape -> Double
area (Triangle b h) = 0.5 * b * h
area (Rectangle l b) = l * b
area (Circle r) = pi * r * r

Eq Shape where
  (==) (Triangle x y) (Triangle a b) = x == a && y == b
  (==) (Rectangle x y) (Rectangle a b) = x == a && y == b
  (==) (Circle x) (Circle y) = x == y
  (==) _ _  = False

Ord Shape where
  compare x y = compare (area x) (area y)

testShapes : List Shape
testShapes = [Circle 3, Triangle 3 9, Rectangle 2 6, Circle 4, Rectangle 2 7]

data Expr num = Val num
              | Add (Expr num) (Expr num)
              | Sub (Expr num) (Expr num)
              | Mul (Expr num) (Expr num)
              | Div (Expr num) (Expr num)
              | Abs (Expr num)

eval : (Abs ty, Neg ty, Integral ty) => Expr ty -> ty
eval (Val num) = num
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y
eval (Abs x) = abs (eval x)

Num ty => Num (Expr ty) where
  (+) = Add
  (*) = Mul
  fromInteger = Val . fromInteger

Neg ty => Neg (Expr ty) where
  negate x = 0 - x
  (-) = Sub

Abs ty => Abs (Expr ty) where
  abs = Abs

Show ty => Show (Expr ty) where
  show (Val n) = show n
  show (Add x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
  show (Sub x y) = "(" ++ show x ++ " - " ++ show y ++ ")"
  show (Mul x y) = "(" ++ show x ++ " * " ++ show y ++ ")"
  show (Div x y) = "(" ++ show x ++ " / " ++ show y ++ ")"
  show (Abs x) = "abs (" ++ show x ++ ")"

(Eq ty, Neg ty, Integral ty, Abs ty) => Eq (Expr ty) where
  (==) x y = eval x == eval y

e0 : Expr Int
e0 = Add (Val 6) (Mul (Val 3) (Val 12))

e1 : Expr Int
e1 = Add (Mul (Val 3) (Val 6)) (Val 12)

(Abs ty, Neg ty, Integral ty) => Cast (Expr ty) ty where
  cast e = eval e

foobar : Integer
foobar = let x = the (Expr Integer) 6 * 3 + 12 in
             the Integer (cast x)

foobaz : Int
foobaz = let y = the (Expr Int) 1 + 2 * 3 + 4 * 5 in
             the Int (cast y)