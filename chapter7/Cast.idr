module Cast 

%default total

{-
  interface Cast from to where
    caset : (orig : from) -> to

-}

Cast (Maybe elem) (List elem) where
  cast Nothing = []
  cast (Just x) = [x]

foo : List Integer
foo = cast (Just 100)

bar : List Integer
bar = cast (the (Maybe Integer) Nothing)

Cast (List elem) (Maybe elem) where
  cast [] = Nothing
  cast (x :: xs) = Just x

baz : Maybe Int
baz = cast  (the (List Int) [99])

quux : Maybe Int
quux = cast (the (List Int) [])


-- there is nothing special about the Cast interface. We can define our own version

interface Mould from to where
  mould : (oig : from) -> to

Mould (Maybe elem) (List elem) where
  mould Nothing = []
  mould (Just x) = [x]

foofoo : List String
foofoo = mould (the (Maybe String) (Just "Hello"))

barbar : List String
barbar = mould (the (Maybe String) Nothing)

Mould (List elem) (Maybe elem) where
  mould [] = Nothing
  mould (x :: xs) = Just x

bazbaz : Maybe String
bazbaz = mould (the (List String) ["world"])

quuxquux : Maybe String
quuxquux = mould (the (List String) [])