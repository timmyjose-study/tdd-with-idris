module Chapter7 

%default total

{-
  interface Functor f => Applicative (f : Type -> Type) where
    pure : a -> f a
    (<*>) : f (a -> b) -> f a -> f b

  interface Applicative m => Monad (m : Type -> Type) where
    (>>=) : m a -> (a -> m b) -> m b
    join : m (m a) = m a


  Monad Maybe where
    (>>=) Nothing next = Nothing
    (>>=) (Just x) next = next x

-}

