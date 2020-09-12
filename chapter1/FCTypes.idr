module FCTypes

%default total

StringOrInt : Bool -> Type
StringOrInt b = case b of
                     False => String
                     True => Int

getStringOrInt : (b : Bool) -> StringOrInt b
getStringOrInt b = case b of
                        False => "94"
                        True => 94

valToString : (b : Bool) -> StringOrInt b -> String
valToString b val = case b of
                         False => val
                         True => cast val