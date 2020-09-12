module Chapter4

-- enumerated types

data Direction = North | East | South | West

turnClockwise : Direction -> Direction
turnClockwise North = East
turnClockwise East = South
turnClockwise South = West
turnClockwise West = North

-- union types

{-
||| Represents possible shapes
data Shape = Triangle Double Double
           | Rectangle Double Double 
           | Circle Double
           -}

data Shape : Type where
  Triangle : Double -> Double -> Shape
  Rectangle : Double -> Double -> Shape
  Circle : Double -> Shape

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length breadth) = length * breadth
area (Circle radius) = pi * radius * radius

-- recursive types

data Picture : Type where
  Primitive : Shape -> Picture
  Combine : Picture -> Picture -> Picture
  Rotate : Double -> Picture -> Picture
  Translate : Double -> Double -> Picture -> Picture

testPicture : Picture
testPicture = Combine (Combine (Translate 5 5 (Primitive (Rectangle 20 10)))
                               (Translate 35 5 (Primitive (Circle 5))))
                      (Translate 15 25 (Primitive (Triangle 10 10)))

pictureArea : Picture -> Double
pictureArea (Primitive s) = area s
pictureArea (Combine p1 p2) = pictureArea p1 + pictureArea p2
pictureArea (Rotate _ p) = pictureArea p
pictureArea (Translate _ _ p) = pictureArea p

-- generic types

data Biggest = NoTriangle | Size Double

biggestTriangle : Picture -> Biggest
biggestTriangle (Primitive s) = case s of
                                     t@(Triangle _ _) => Size (area t)
                                     _ => NoTriangle
biggestTriangle (Combine p1 p2) = case biggestTriangle p1 of
                                       Size d1 => case biggestTriangle p2 of
                                                       Size d2 => Size (max d1 d2)
                                                       NoTriangle => NoTriangle
                                       NoTriangle => case biggestTriangle p2 of
                                                          Size d2 => Size d2
                                                          NoTriangle => NoTriangle
biggestTriangle (Rotate _ p) = case biggestTriangle p of
                                    s@(Size _) => s
                                    _ => NoTriangle
biggestTriangle (Translate _ _ p) = case biggestTriangle p of
                                         s@(Size _) => s
                                         _ => NoTriangle

safeDivide : Double -> Double -> Maybe Double
safeDivide x y = if y == 0 then Nothing
                           else Just (x / y)

data Tree elem = Empty | Node (Tree elem) elem (Tree elem)

%name Tree tree, tree1

insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node l v r) = case compare x v of
                             GT => Node l v (insert x r)
                             EQ => orig
                             LT => Node (insert x l) v r

data BSTree : (elem : Type) -> Type where
  BSEmpty : Ord elem => BSTree elem
  BSNode : Ord elem => (left : BSTree elem) -> (val : elem) -> (right : BSTree elem) -> BSTree elem

insertBST : elem -> BSTree elem -> BSTree elem
insertBST x BSEmpty = BSNode BSEmpty x BSEmpty
insertBST x orig@(BSNode l v r) = case compare x v of
                                    LT => BSNode (insertBST x l) v r
                                    EQ => orig
                                    GT => BSNode l v (insertBST x r)

-- dependent types

-- linear types