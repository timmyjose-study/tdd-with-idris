module Loops

import Data.Strings
import System

%default total

countdown : HasIO io => (secs : Nat) -> io ()
countdown Z = putStrLn "Lift-off!"
countdown (S k) = do putStrLn (show (S k))
                     usleep 1000000
                     countdown k

readNumber : HasIO io => io (Maybe Nat)
readNumber = do ns <- getLine
                if all isDigit (unpack ns)
                   then pure $ Just (stringToNatOrZ ns)
                   else pure Nothing
                

partial
countdowns : HasIO io => io ()
countdowns = do putStr "Enter starting number: "
                Just num <- readNumber
                  | Nothing => putStrLn "We accept only numbers, sorry!"
                countdown num
                putStr "Try again? [y/n]: "
                choice <- getLine
                if choice == "y" || choice == "yes"
                   then countdowns
                   else pure ()