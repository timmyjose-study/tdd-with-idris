module Ex5

import Data.Strings
import System

%default total

printLonger : HasIO io => io ()
printLonger = do putStr "Enter the first string: "
                 first <- getLine
                 putStr "Enter the second string: "
                 second <- getLine
                 let firstlen = length first -- no `in` here
                 let secondlen = length second
                 putStrLn $ show (max firstlen secondlen)

printLongerBind : HasIO io => io ()
printLongerBind = putStr "Enter the first string: " >>= \_ =>
                    getLine >>= \first =>
                      putStr "Enter the second string: " >>= \_ =>
                        getLine >>= \second =>
                          let firstlen = length first 
                              secondlen = length second in -- `in` is used here - why the inconsistency?
                            putStrLn (show (max firstlen secondlen))

readNumber : HasIO io => io (Maybe Nat)
readNumber = do ns <- getLine
                if all isDigit (unpack ns) 
                   then pure $ Just (stringToNatOrZ ns)
                   else pure Nothing

-- guess the number game
partial
guess : HasIO io => (n : Nat) -> (guesses : Nat) -> io ()
guess secret count = do putStr "Enter your guess: "
                        Just g <- readNumber 
                          | Nothing => do putStrLn "Invalid input: not a number"
                                          pure ()
                        case compare g secret of
                             LT => do putStrLn "Too low!"
                                      guess secret (count + 1)
                             GT => do putStrLn "Too high!"
                                      guess secret (count + 1)
                             EQ => do putStrLn $ "You win! You took " ++ show (count + 1) ++ " guesses!"
                                      pure ()

partial
guessingGame : HasIO io => io ()
guessingGame = do currTime <- time 
                  let secret = currTime `mod` 101 + 1 -- secret in [1, 100]
                  guess (integerToNat secret) 0

partial
myRepl : HasIO io => String -> (String -> String) -> io ()
myRepl prompt func = do putStr prompt
                        inp <- getLine
                        putStr (func inp)
                        myRepl prompt func

partial
reverser : HasIO io => io ()
reverser = myRepl "Enter a string: " (\s => reverse s ++ "\n")

partial
myReplWith : HasIO io => a -> String -> (a -> String -> Maybe (String, a)) -> io ()
myReplWith state prompt func = do putStr prompt
                                  inp <- getLine
                                  case func state inp of
                                       Nothing => pure ()
                                       Just (out, newState) => do putStr out
                                                                  myReplWith newState prompt func

partial
adder : HasIO io => io ()
adder = myReplWith 0 "Enter a number: " (\acc, s => case all isDigit (unpack s) of
                                                         False => Nothing
                                                         True => let n = stringToNatOrZ s in
                                                                     Just $ ("Current value: " ++ show (acc + n) ++ "\n", acc + n))


