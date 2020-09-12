module Ex2

import Data.List
import Data.Strings

%default total

-- 2.

palindrome : String -> Bool
palindrome s = s == reverse s

-- 3.

palindrome1 : String -> Bool
palindrome1 s = toLower s == toLower (reverse s)

-- 4.

palindrome2 : String -> Bool
palindrome2 s = if len > 10
                   then toLower s == toLower (reverse s)
                   else False
  where
    len : Nat
    len = length s

-- 5.

palindrome3 : Nat -> String -> Bool
palindrome3 n s = if length s > n 
                     then toLower s == toLower (reverse s)
                     else False

-- 6.

partial
counts : String -> (Nat, Nat)
counts str = (length (words str), length str)

-- 7.

topTen : Ord a => List a -> List a
topTen = take 10 . sortBy (\x, y => compare y x)

-- 8.

overLength : Nat -> List String -> Nat
overLength n = length . filter (\s => length s > n) 
