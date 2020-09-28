module DepPairs

import Data.Vect

%default total

partial
readVect : HasIO io => io (n : Nat ** Vect n String)
readVect = do x <- getLine
              if x == ""
                 then pure $ MkDPair _ []
                 else do MkDPair _ xs <- readVect
                         pure $ MkDPair _ (x :: xs)

 -- since the dependent pair syntax is merely syntactic sugar for
 -- the DPair type
partial
readVect' : HasIO io => io (n : Nat ** Vect n String)
readVect' = do x <- getLine
               if x == ""
                  then pure $ (_ ** [])
                  else do (_ ** xs) <- readVect'
                          pure $ (_ ** (x :: xs))

printVect : HasIO io => (n : Nat ** Vect n String) -> io ()
printVect (n ** xs) = putStrLn $ show xs ++ " (length " ++ show n ++ ")"

printVect' : HasIO io => (n : Nat ** Vect n String) -> io ()
printVect' (MkDPair n xs) = putStrLn $ show xs ++ " (length " ++ show n ++ ")"