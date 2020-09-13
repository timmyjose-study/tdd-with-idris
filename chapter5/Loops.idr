module Loops

import System

%default total

countdown : HasIO io => (n : Nat) -> io ()
countdown Z = putStrLn "Lift-off!"
countdown (S k) =  do putStrLn (show (S k))
                      sleep 1
                      countdown k