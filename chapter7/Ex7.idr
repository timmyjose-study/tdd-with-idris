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