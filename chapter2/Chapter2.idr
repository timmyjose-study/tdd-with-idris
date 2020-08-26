module Chapter2

import Data.List
import Data.Strings

-- tuples

myPair : (Nat, String)
myPair = (100, "Hundred")

-- lists

wordCount : String -> Nat
wordCount = length . words

allLengths : List String -> List Nat
allLengths = map length 