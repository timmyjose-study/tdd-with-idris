module Chapter6

import Data.Vect

%default total

-- type synonyms

Position : Type
Position = (Double, Double)

tri : Vect 3 Position
tri = [(0.0, 0.0), (1.0, 2.0), (-23.22, -11.089)]

-- type-level functions
-- type-level functions are merely functions that return a Type

-- defining functions with a variable number of arguments


