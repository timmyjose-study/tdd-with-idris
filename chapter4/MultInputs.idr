module MultInputs

import System.REPL

%default total

multInputs : Integer -> String -> Maybe (String, Integer)
multInputs prod inp = 
  let val = cast inp in
      if val <= 0 
         then Nothing
         else let newVal = prod * val in
                  Just ("Running product: " ++ show newVal ++ "\n", newVal)

partial
main : IO ()
main = replWith 1 "Value: " multInputs