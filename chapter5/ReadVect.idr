module ReadVect

import Data.Vect

%default total

-- reading a vector whose length is known ahead of time

readVectLen : HasIO io => (len : Nat) -> io (Vect len String)
readVectLen Z = pure []
readVectLen (S k) = do x <- getLine
                       xs <- readVectLen k
                       pure (x :: xs)

-- reading a vector whose length is not known ahead of time

||| This type represents a vector whose length is encoded as part of its
||| type, and this length is only known when the program is run
data VectUnknown : Type -> Type where
  MkVect : (len : Nat) -> Vect len a -> VectUnknown a

printVect : HasIO io => Show a => VectUnknown a -> io ()
printVect (MkVect len xs) = putStrLn $ show xs ++ "(length = " ++ show len ++ ")"

partial
readVect : IO (VectUnknown String)
readVect = do x <- getLine
              if x == ""
                 then pure $ MkVect _ []
                 else do MkVect _ xs <- readVect
                         pure $ MkVect _ (x :: xs)

-- The problem with the approach above is that we would have to define
-- such wrapper types for each type that may be read in from an external
-- source. A better way of embedding the length (unknown till runtime) 
-- or any other dynamic property along with the data is to use "Dependent Pairs"

-- a dependent pair is a pair where the type of the second element can be computed
-- by the value of the first element of the pair                                    

anyVect : (n : Nat ** Vect n String)
anyVect = (2 ** ["Hello", "world"])

anyVect1 : (n ** Vect n String)
anyVect1 = (3 ** ["Hola", "mundo", "hello"])