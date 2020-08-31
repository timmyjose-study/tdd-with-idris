module FCTypes

StringOrInt : (x : Bool) -> Type
StringOrInt x = case x of
                     False => String
                     True => Int

getStringOrInt : (x : Bool) -> StringOrInt x
getStringOrInt x = case x of
                        False => "Ninety-four"
                        True => 94

valToString : (x : Bool) -> StringOrInt x -> String
valToString x val = case x of
                         False => val
                         True => cast val
