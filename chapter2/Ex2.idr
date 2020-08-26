module Ex2

import Data.List
import Data.Strings

--- 2

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
palindrome''' n str = if length str > n
                        then toLower str == toLower (reverse str)
                        else False

 -- 6.

counts : String -> (Nat, Nat)
counts str = (length (words str), length str)

-- 7.

topTen : Ord a => List a -> List a
topTen = take 10 . sortBy (\x, y => compare y x)

-- 8.

overLength : Nat -> List String -> Nat
overLength n = length . filter (\s => length s > n)

-- 9 & 10 - See Counts.idr and Palindrome.idr
