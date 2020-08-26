module FCTypes

StringOrInt : (x : Bool) -> Type
StringOrInt b = case b of
                     False => Int
                     True => String

getStringOrInt : (x : Bool) -> StringOrInt x
getStringOrInt b = case b of
                        False => 100
                        True => "Hundred"

valToString : (x : Bool) -> StringOrInt x -> String
valToString b val = case b of
                         False => cast val
                         True => val
