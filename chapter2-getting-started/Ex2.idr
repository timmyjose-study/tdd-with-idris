module Ex2

import Data.List
import Data.Strings

-- 2.

palindrome : String -> Bool
palindrome str = str == reverse str

-- 3.

palindrome' : String -> Bool
palindrome' str = toLower str == toLower (reverse str)

-- 4.

palindrome'' : String -> Bool
palindrome'' str = if length str > 10 
                      then toLower str == toLower (reverse str)
                      else False

-- 5.

palindrome''' : Nat -> String -> Bool
palindrome''' len str =  if length str > len
                            then toLower str == toLower (reverse str)
                            else False

-- 6.

counts : (str : String) -> (Nat, Nat)
counts str = (length (words str), length str)

-- 7.

topTen : Ord a => List a -> List a
topTen = take 10 . sortBy (\x, y => compare y x)

-- 8.

overLength : Nat -> List String -> Nat
overLength len = length . filter (\s => length s > len)

-- 9.
-- See Palindrome.idr

-- 10.
--- See Counts.idr