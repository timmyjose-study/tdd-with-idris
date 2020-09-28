module Main

import Data.Strings
import Data.Vect

%default total

partial
readVectInt : HasIO io => io (n : Nat ** Vect n Int)
readVectInt = do x <- getLine
                 if x == ""
                    then pure $ (_ ** [])
                    else do (_ ** ns) <- readVectInt
                            pure $ (_ ** (cast x :: ns))

partial
addVectInts : HasIO io => io ()
addVectInts = do putStrLn "Enter the first vector (blank line to end)"
                 (len1 ** xs) <- readVectInt
                 putStrLn "Enter the second vector (blank line to end)"
                 (len2 ** ys) <- readVectInt
                 case exactLength len1 ys of
                      Nothing => putStrLn "Vectors are of unequal lengths"
                      Just ys' => printLn $ zipWith (+) xs ys'