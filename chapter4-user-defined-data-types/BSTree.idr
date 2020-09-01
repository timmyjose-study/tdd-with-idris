module BSTree

data BSTree : Type -> Type where
  Empty : Ord elem => BSTree elem
  Node : Ord elem => (left : BSTree elem) -> (val : elem) -> (right : BSTree elem) -> BSTree elem

-- no Ord elem constraint here since the constraint is in the data type itself
insert : elem -> BSTree elem -> BSTree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node l v r) = case compare x v of
                                  LT => Node (insert x l) v r
                                  GT => Node l v (insert x r)
                                  EQ => orig
