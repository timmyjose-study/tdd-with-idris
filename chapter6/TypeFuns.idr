module TypeFuns

import Data.Strings

%default total

StringOrInt : Bool -> Type
StringOrInt False = String
StringOrInt True = Int

getStringOrInt : (isInt : Bool) -> StringOrInt isInt
getStringOrInt False = "Hello" 
getStringOrInt True = 100

valToString : (isInt : Bool) -> StringOrInt isInt -> String
valToString False s = trim s
valToString True i = cast i

-- we can have practically any valid form in the type signature

valToStringCase : (isInt : Bool) -> (case isInt of False => String 
                                                   True => Int) -> String
valToStringCase False s = trim s
valToStringCase True i = cast i
