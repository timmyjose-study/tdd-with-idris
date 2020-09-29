module Chapter5

import Data.Vect

%default total

printLength : HasIO io => io ()
printLength = putStr "Enter a string: " >>= \_ =>
                getLine >>= \input =>
                  let len = length input in
                    putStrLn $ "\"" ++ input ++ "\" has length " ++ show len

printTwoThings : HasIO io => io ()
printTwoThings = do putStrLn "Hello"
                    putStrLn "World"

printThreeThings : HasIO io => io ()
printThreeThings = putStrLn "Hello" >>= \_ =>
                    putStrLn "World" >>= \_ =>
                      putStrLn "Again"

printInput : HasIO io => io ()
printInput = do x <- getLine
                putStrLn x

printLengthAgain : HasIO io => io ()
printLengthAgain = do putStr "Enter a string: "
                      string <- getLine
                      let len = length string -- no `in` here
                      putStrLn $ "\"" ++ string ++ "\" has length " ++ show len

readPair : HasIO io => io (String, String)
readPair = do f <- getLine
              s <- getLine
              pure (f, s)

usePair : HasIO io => io ()
usePair = do p <- readPair
             case p of
                  (f, s) => putStrLn $ "You entered the following strings: " ++ f ++ ", " ++ s

-- destructuring pattern-matching

usePairImproved : HasIO io => io ()
usePairImproved = do (f, s) <- readPair
                     putStrLn $ "You entered the following strings: " ++ f ++ ", " ++ s

-- reading and validating dependent types

myZip : Vect n a -> Vect n b -> Vect n (a, b)
myZip [] [] = []
myZip (x :: xs) (y :: ys) = (x, y) :: myZip xs ys

partial
readVect : HasIO io => io (n : Nat ** Vect n String)
readVect = do x <- getLine
              if x == ""
                 then pure $ (_ ** [])
                 else do (_ ** xs) <- readVect
                         pure $ (_ ** (x :: xs))

partial
zipInputs : HasIO io => io ()
zipInputs = do putStrLn "Enter the first vector (blank line to end)"
               (len1 ** xs) <- readVect
               putStrLn "Enter the second vector (blank line to end)"
               (len2 ** ys) <- readVect
               case exactLength len1 ys of -- exactLength is needed here to ensure type safety (== check would not work)
                    Nothing => putStrLn "Vectors are of different lengths"
                    Just ys' => printLn $ zip xs ys'
