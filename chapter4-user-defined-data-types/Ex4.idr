module Ex4

data BSTree : Type -> Type where
  Empty : Ord elem => BSTree elem
  Node : Ord elem => (left : BSTree elem) -> (val : elem) -> (right : BSTree elem) -> BSTree elem

insert : elem -> BSTree elem -> BSTree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node l v r) = case compare x v of
                                  LT => Node (insert x l) v r
                                  GT => Node l v (insert x r)
                                  EQ => orig

listToTree : Ord elem => List elem -> BSTree elem
listToTree [] = Empty
listToTree (x :: xs) = insert x (listToTree xs)

treeToList : BSTree elem -> List elem
treeToList Empty = []
treeToList (Node l v r) = treeToList l ++ [v] ++ treeToList r

data Expr = Val Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr

evaluate : Expr -> Int
evaluate (Val n) = n
evaluate (Add e1 e2) = evaluate e1 + evaluate e2
evaluate (Sub e1 e2) = evaluate e1 - evaluate e2
evaluate (Mul e1 e2) = evaluate e1 * evaluate e2

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe mx my = case mx of
                      Just x => case my of
                                     Just y => Just (max x y)
                                     Nothing => mx
                      Nothing => case my of
                                      Just y => my
                                      Nothing => Nothing

data Shape: Type where
  Triangle : (base : Double) -> (height : Double) -> Shape
  Rectangle : (length : Double) -> (breadth : Double) -> Shape
  Circle : (radius : Double) -> Shape

area : Shape -> Double
area (Triangle b h) = 0.5 * b * h
area (Rectangle l b) = l * b
area (Circle r) = pi * r * r

data Picture : Type where
  Primitive : Shape -> Picture
  Combine : Picture -> Picture -> Picture
  Rotate : (angle : Double) -> Picture -> Picture
  Translate : (x : Double) -> (y : Double) -> Picture -> Picture

biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive s) = case s of
                                     t@(Triangle _ _) => Just $ area t
                                     _ => Nothing
biggestTriangle (Combine s1 s2) = case biggestTriangle s1 of
                                       Just a1 => case biggestTriangle s2 of
                                                       Just a2 => Just $ max a1 a2
                                                       Nothing => Just a1
                                       Nothing => case biggestTriangle s2 of
                                                       Just a2 => Just a2
                                                       Nothing => Nothing
biggestTriangle (Rotate _ s) = biggestTriangle s
biggestTriangle (Translate _ _ s) = biggestTriangle s

testPicture1 : Picture
testPicture1 = Combine (Primitive (Triangle 2 3))
                       (Primitive (Triangle 2 4))

testPicture2 : Picture
testPicture2 = Combine (Primitive (Rectangle 1 3))
                       (Primitive (Circle 4))
