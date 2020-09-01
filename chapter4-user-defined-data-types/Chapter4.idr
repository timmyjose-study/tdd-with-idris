module Chapter4

%default total

-- enumerated types

data Direction = North | East | South | West

turnClockwise : Direction -> Direction
turnClockwise North = East
turnClockwise East = South
turnClockwise South = West
turnClockwise West = North

-- union types

{-
data Shape = Triangle Double Double
            | Rectangle Double Double
            | Circle Double
            -}

-- general form of a data type 

data Shape : Type where
  Triangle : Double -> Double -> Shape
  Rectangle : Double -> Double -> Shape
  Circle : Double -> Shape

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length breadth) = length * breadth
area (Circle radius) = pi * radius * radius

-- recursive types

data Picture = Primitive Shape
            | Combine Picture Picture
            | Rotate Double Picture
            | Translate Double Double Picture

rectangle : Picture
rectangle = Primitive (Rectangle 20 10)

circle : Picture
circle = Primitive (Circle 5)

triangle : Picture
triangle = Primitive (Triangle 10 10)

testPicture : Picture
testPicture = Combine (Translate 5 5 rectangle)
                      (Combine (Translate 35 5 circle)
                               (Translate 15 25 triangle))

pictureArea : Picture -> Double
pictureArea (Primitive s) = area s
pictureArea (Combine p1 p2) = pictureArea p1 + pictureArea p2
pictureArea (Rotate _ p) = pictureArea p
pictureArea (Translate _ _ p) = pictureArea p

-- todo
biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive s) = case s of
                                     triangle@(Triangle _ _ ) => Just $ area triangle
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

-- generic data types

data Tree elem = Empty | Node (Tree elem) elem (Tree elem)

insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node l v r) = case compare x v of
                             GT => Node l v (insert x r)
                             LT => Node (insert x l) v r
                             EQ => orig

-- binary search tree

