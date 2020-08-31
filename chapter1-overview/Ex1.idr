module Ex1

-- 1.
{-
  Input type: Vect n elem
  Output type: Vect n elem
  Operations: returning a copy of the list, returning a sorted version of the list, returning the reverse of the list.

  Input type: Vect n elem
  Output type: Vect (n * 2) elem
  Operations: returning a list appended to itself, returning a list appended with `n` random elements of type `elem`, returning a list appeneded to its reverse.

  Input type: Vect (1 + n) elem
  Output type: Vect n elem
  Operations: Popping an element off the end of the list, removing an element from a random index of the list, returning the tail of the list.

  Input type: Bounded n, Vect n elem
  Output type: elem
  Operations: returning the element at index `Bounded n`, deleting the element at index `Bounded n`.
-}