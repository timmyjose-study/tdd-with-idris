module Ex4

data Tree : (elem : Type) -> Type where
  Empty : Ord elem => Tree elem
  Node : Ord elem => (left : Tree elem) -> (val : elem) -> (right : Tree elem) -> Tree elem

insert : elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node l v r) = case compare x v of
                               LT => Node (insert x l) v r
                               EQ => orig
                               GT => Node l v (insert x r)

listToTree : Ord elem => List elem -> Tree elem
listToTree [] = Empty
listToTree (x :: xs) = insert x (listToTree xs)

treeToList : Tree elem -> List elem
treeToList Empty = []
treeToList (Node l v r) = treeToList l ++ [v] ++ treeToList r

data Expr : Type where
  Val : Int -> Expr
  Add : Expr -> Expr -> Expr
  Sub : Expr -> Expr -> Expr
  Mul : Expr -> Expr -> Expr

evaluate : Expr -> Int
evaluate (Val n) = n
evaluate (Add e1 e2) = evaluate e1 + evaluate e2
evaluate (Sub e1 e2) = evaluate e1 - evaluate e2
evaluate (Mul e1 e2) = evaluate e1 * evaluate e2

e1 : Expr
e1 = Mul (Val 10) (Add (Val 6) (Val 3))

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe (Just x) (Just y) = case compare x y of
                                  GT => Just x
                                  _ => Just y
maxMaybe _ _ = Nothing

data Shape : Type where
  Triangle : Double -> Double -> Shape
  Rectangle : Double -> Double -> Shape
  Circle : Double -> Shape

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length breadth) = length * breadth
area (Circle radius) = pi * radius * radius

data Picture : Type where
  Primitive : Shape -> Picture
  Combine : Picture -> Picture -> Picture
  Rotate : Double -> Picture -> Picture
  Translate : Double -> Double -> Picture -> Picture

pictureArea : Picture -> Double
pictureArea (Primitive s) = area s
pictureArea (Combine p1 p2) = pictureArea p1 + pictureArea p2
pictureArea (Rotate _ p) = pictureArea p
pictureArea (Translate _ _ p) = pictureArea p

testPic1 : Picture
testPic1 = Combine (Primitive (Triangle 2 3))
                   (Primitive (Triangle 2 4))

testPic2 : Picture
testPic2 = Combine (Primitive (Rectangle 1 3))
                   (Primitive (Circle 4)) 

biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive s) = case s of
                                     Triangle _ _ => Just (area s)
                                     _ => Nothing
biggestTriangle (Combine p1 p2) = case biggestTriangle p1 of
                                       Just a1 => case biggestTriangle p2 of
                                                       Just a2 => Just (max a1 a2)
                                                       Nothing => Just a1
                                       Nothing => case biggestTriangle p2 of
                                                       Just a2 => Just a2
                                                       Nothing => Nothing
biggestTriangle (Rotate _ p) = biggestTriangle p
biggestTriangle (Translate _ _ p) = biggestTriangle p