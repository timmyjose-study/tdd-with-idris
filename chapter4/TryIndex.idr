module TryIndex

import Data.Vect

%default total

tryIndex : { n : _ } -> Integer -> Vect n elem -> Maybe elem
tryIndex m xs = case integerToFin m n of
                     Nothing => Nothing
                     Just idx => Just $ index idx xs

vi : Vect 5 Int
vi = [1, 2, 3, 4, 5]